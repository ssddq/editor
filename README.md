At the moment, this is mostly a proof-of-concept, and there are few user-facing features. The program has:

* Basic modal editing: `i` to enter insert mode, `Esc` to leave it

* Basic navigation with the arrow keys

* Local seeking: with `s` (forward) and `Shift + s` (backward) followed by a 2 character sequence to search for

* Syntax highlighting: currently, there is a `flatparse`-based syntax highlighter for Haskell source code. However, there is no filetype detection as yet, so the parser will be applied to any file you open.

The program requires a file as an argument, and can be passed a small number of optional arguments:

* `--render INTxINT` specifies the pixel dimensions of the render canvas as `<height>x<width>`. On a device with memory or performance constraints (e.g. if you have integrated graphics), you may wish to set this to a lower value. On a high DPI display (e.g. most modern laptops), you can likely decrease the render resolution significantly without any perceptible impact in quality.

* `--size FLOAT` and `--dpi FLOAT` can be used to specify the font size in points and display DPI respectively. Internally, these are only ever used in tandem -- the only reason you may wish to set the DPI is to ensure that the font size (which is a physical measure) can be properly converted to a pixel size.

* `--font FILE` can be used to provide a TrueType (`.ttf`) font source file. Note that the TTF parser is *not* currently complete, and likely will not be for some time. I have tested a small number of fixed-width and proportional fonts without issue, but (in particular) any font that does not provide a format 4 `cmap` encoding table will cause the program to crash immediately.

## Building

To build from source, you can simply clone the repository *recursively* with

    git clone --recurse-submodules https://github.com/ssddq/editor

and build with `cabal build`. If you have Nix installed, `nix develop` should provide a shell with the required dependencies.

A prebuilt binary for x86-64 Linux distributions is also automatically generated at <https://github.com/ssddq/editor-release>, which you can run with
    
    nix run github:ssddq/editor-release

This is likely to require additional steps if you are not on NixOS; please visit that repository for more instructions.

## Technical features

The technical points that may be of interest:

* Files are *never* loaded into memory: the file state is stored as (essentially) a strict `IntMap` of changes -- insertions or deletions -- keyed on byte positions in the underlying file. Line positions are stored in (essentially) a red-black tree of arrays.

* Every frame, the relevant portion of the file is read from the disk and merged with the editor state into (essentially) a `Stream (Of Char) IO ()`. The stream is terminated as soon as the renderer has consumed enough characters to fill the given render area.

* Syntax highlighting is provided by streaming the results of a stateful `flatparse` parser with signature `state -> Parser e (Color, state)`. The parser is fed chunks of bytestrings with some minimal length (currently 512), and the results are streamed as they are produced.

  Parsers manage their own internal state (which may be trivial), and can request a limited amount of context (currently 512 bytes) so that they can begin parsing earlier than the start of the render area.

* The file is scanned for newline characters eagerly and asynchronously on startup. Operations that depend on the array of newline positions (e.g. line traversal, inserting a newline character) only block until enough of the array has been generated that they can proceed. This means that *there is no perceptible difference in opening and editing a 1 KB file and opening and editing a 1 GB file*, unless you (immediately) jump to the last line in a large file.

  You can generate a 1 GB text file with:

      base64 /dev/urandom | head -c 1000000000 > sample.txt

Currently only UTF-8 encoded text files are supported; attempting to open a binary file will likely terminate the program immediately.

## Warnings

This is very much *not* safe Haskell.

While I'm not aware of any serious issues in the filebuffer implementation, there is no built-in way to save changes to a file as a precaution. The internal file state is complicated -- to say the least -- and it's still entirely possible that there are subtle issues (e.g. adjacent characters get transposed) and only occur in rare cases (e.g. at the end of the file once the depth of the tree exceeds 17).

If you like to live dangerously, you can recompile it and enable saving with little effort: in `main/Main.hs`, simply take the stream used for rendering and provide a keybind that writes it to a handle instead.

## Performance

Since this is in active development and many things (e.g. the exact content of the file being opened) can have dramatic impacts on performance, you should only use the numbers below as a *general* indication of performance.

On a desktop computer with an AMD 7900X and RX 6750 XT:

* Given a 1 GB file with 70-80 characters per line, scanning for newlines takes <2 seconds on my device to produce an array just over ~100MB. Note, however, that startup is still *instant*: 2 seconds is only the time it takes for the scan to complete (asynchronously).

  On a 100 MB file, it completes in < 0.4s. These seemed measurably slower than e.g. helix in a terminal. However, on extremely large files (1 GB) both memory usage and the initial start up time (i.e. perceived latency) are considerably lower.

* Frame times depend on the file, but hover somewhere around 1 ms on my desktop. On my far less capable Framework laptop, frame times were around 7 ms. These should be mostly impercetible.

  Note that frame rates are hard-limited by the poll rate of `SDL.waitEvent`, which is roughly ~40 ms on my device. Frames are only ever rendered in response to SDL events (see: `main/Main.hs`), and you should expect your device to be idle for the most part.

* Syntax highlighting incurs a variable performance penalty, but it can be as low as 10% (i.e. frame times only increase by 10%). This seems to be largely due to `flatparse` generating *extremely* efficient parsers! Requesting the maximum context currently allowed (512 bytes) on the current Haskell parser brought the penalty up to only 20%.

  The main source of performance degradation here (other than excessive backtracking) is breaking up bytestrings: parsers that can color whole words at a time have a dramatically smaller performance impact than parsers that color letter-by-letter.

## Project layout

There are two main logical components: the renderer and the filebuffer.

The filebuffer is almost entirely contained in the `filebuffer` package, which provides the main data structures holding the editor state along with functions to modify and traverse it, and to stream the underlying file with the edits applied.

The renderer spans a few packages

* `vma` generates bindings to the Vulkan Memory Allocator library.

* `shaders` generates SPIR-V bytecode for the shaders used by the renderer, and exports these as Haskell bytestrings.

* `parse-font` contains a (not yet complete) parser for .ttf font files.

* `syntax` contains a stateful `flatparse` syntax highlighter for Haskell.

* `renderer` sets up the Vulkan render pipeline, and generates indirect draw calls from (essentially) a `Stream (Of Char) IO ()`.

The `common` package provides shared interfaces for some types across packages, along with shared utility functions. The `tools` package provides the preprocessor used in `renderer` (see below), as well as some formatting tools.

Unsurprisingly, the executable is generated from `main/Main.hs`.

## Syntax

There are some non-standard things about the code -- and the `renderer` package in particular -- that are worth mentioning.

**Type parameter records.** Nearly all primary modules in `renderer`, as well as `main`, are run through the preprocessor provided by `tools/preprocessor`. This contains a preprocessor and a Template Haskell library that parses type parameter records, transforming

    f :: Vk { parameter1 = I, parameter2 = X }
      -> ...

into

    f :: forall ... . Vk ... I ... X ...
      -> ...

The types `I` and `X` are substituted for the type parameters `parameter1` and `parameter2` named in the type declaration

    data Vk ... parameter1 ... parameter2 ...

and the remaining type parameters are quantified over. This transformation is completely mechanical, though Template Haskell is used to export (and then retrieve) the type parameter names used in the original declaration.

The type of nearly every field of `Vk` is expressed in terms of a type family, which uses the flag `I` or `X` in the associated type parameter to determine whether the field has its 'base' type, or is empty and has type `()`. For fields whose 'base' type `f` has kind `* -> *`, the flag `A b` is used instead, and the resulting field has type `f b`.

Every function that is used directly in the main pipeline of the program takes the form

    f ::    Vk { ... }
      -> IO Vk { ... }

and these are composed with `(>>=)`.

**Function application.** Two non-standard operators for function application appear quite often -- especially when invoking Vulkan commands or creating Vulkan structs -- namely:

* `(|-)` which is *left-associative* function application with a precedence of *3*, and

* `(|*)` which is *left-associative* function application with a precedence of *2*.

If you're familiar with `BlockArguments`, these are used the same way `do` sometimes is for non-monadic arguments.
