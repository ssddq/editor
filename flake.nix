{

  inputs =
  { nixpkgs.url = "github:NixOS/nixpkgs";

    release =
    { flake = false;
      url   = "https://github.com/ssddq/editor/releases/download/binary/editor";
    };

    vma-lib =
    { flake = false;
      url   = "git+https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator?ref=master&rev=0c7ad4e85944b38ef6e4d2becfd58bf58385812e";
    };
  };

  outputs = { self, nixpkgs, vma-lib, release }:
  let system = "x86_64-linux";

      pkgs = nixpkgs.legacyPackages.${system};

      haskellPackages = pkgs.haskell.packages.ghc962;

      alex  = haskellPackages.alex;
      happy = haskellPackages.happy;

      cabal2nix = package: source: args:
        haskellPackages.callCabal2nixWithOptions package source "--no-check --no-haddock" args;

      hsc-gen = cabal2nix "hsc-gen" ./vma/hsc-gen {};

      vma = (cabal2nix "vma" ./vma {inherit hsc-gen; }).overrideAttrs
        ( oldAttrs:
          { buildInputs =  ( oldAttrs.buildInputs )
                        ++ [ pkgs.vulkan-loader
                             pkgs.vulkan-headers
                           ];

            configurePhase =
            ''
              mkdir -p ./VulkanMemoryAllocator/include
              cp -r ${vma-lib.outPath}/* ./VulkanMemoryAllocator/
              ${oldAttrs.configurePhase}
            '';
           }
        );

      common = cabal2nix "common" ./common {};

      preprocessor = cabal2nix "preprocessor" ./tools/preprocessor
        { inherit parser;
          inherit formatter;
        };

      parser = cabal2nix "parser" ./tools/parser {};

      formatter = cabal2nix "formatter" ./tools/formatter
        { inherit common;
          inherit parser;
        };

      parse-font = cabal2nix "parse-font" ./parse-font
        { inherit common; };

      shaders = (cabal2nix "shaders" ./shaders {}).overrideAttrs
        ( oldAttrs:
          { buildInputs =  ( oldAttrs.buildInputs )
                        ++ [ pkgs.shaderc ];
          }
        );

      filebuffer = cabal2nix "filebuffer" ./filebuffer
        { inherit common; };

      renderer = cabal2nix "renderer" ./renderer
        { inherit common;
          inherit vma;
          inherit preprocessor;
        };
  in
  { packages.${system}.default = pkgs.stdenv.mkDerivation
    { name = "editor";
      version = "0.1";

      src = ./.;

      nativeBuildInputs = [ pkgs.autoPatchelfHook ];

      buildInputs = with pkgs;
      [ glibc
        libstdcxx5
        vulkan-loader
        SDL2
        gmp
        libffi
        elfutils
        gcc
        zlib
        zstd
        xz
        bzip2
        xorg.libxcb
        xorg.libX11
        xorg.libXext
        xorg.libXcursor
        xorg.libXi
        xorg.libXfixes
        xorg.libXrandr
        xorg.libXScrnSaver
        xorg.libXau
        xorg.libXdmcp
        xorg.libXrender
      ];

      installPhase =
      ''
        install -m755 -D ${release.outPath} $out/bin/editor
      '';
    };

    editor = cabal2nix "editor" ./.
    { inherit vma;
      inherit shaders;
      inherit parse-font;
      inherit preprocessor;
      inherit filebuffer;
      inherit common;
      inherit renderer;
    };

    editor-shell = pkgs.mkShell
    { shellHook =
      ''
        export PATH=''$PATH:${self.outPath}/scripts
        export LD_LIBRARY_PATH=''$LD_LIBRARY_PATH:${pkgs.vulkan-loader}/lib
      '';

      buildInputs =
      [ haskellPackages.ghc
        haskellPackages.ghcid
        haskellPackages.cabal-install

        pkgs.vulkan-loader
        pkgs.vulkan-headers

        pkgs.SDL2

        pkgs.stylish-haskell

        alex
        happy

        formatter
      ];


      inputsFrom =
      [ self.editor.env ];

    };

    devShells.${system} =
    { default = self.editor-shell;
    };

  };

}
