{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gen where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Data.ByteString.UTF8             qualified as BS
import Data.Char
import Data.List

end :: Parser String
end = option "" (fmap (\x -> [x])
                      (char '\n'))

singleCharacter :: Parser String
singleCharacter = do
  c <- choice [ notChar '\n'
              , char '\n' >> lookAhead space >> return '\n'
              ]
  return [c]

line :: Parser String
line = do
  many1' singleCharacter
  end
  return ""

handle :: Parser String
handle = do
  string "VK_DEFINE_HANDLE("
  handleName <- manyTill' anyChar (char ')')
  return $ "data " ++ handleName ++ "_T\n"
        ++ "type " ++ handleName ++ " = Ptr " ++ handleName ++ "_T\n\n"

handle_nonDispatchable :: Parser String
handle_nonDispatchable = do
  string "VK_DEFINE_NON_DISPATCHABLE_HANDLE("
  handleName <- manyTill' anyChar (char ')')
  return $ "data " ++ handleName ++ "_T\n"
        ++ "type " ++ handleName ++ " = VkPtr " ++ handleName ++ "_T\n\n"

vkFlags :: Parser String
vkFlags = do
  string "typedef VkFlags "
  name <- manyTill' anyChar (string "Flags;")
  let flags = name ++ "Flags"
  let bitmask  = name ++ "Bitmask"
  return $ "newtype " ++ bitmask ++ " (a :: FlagType) = " ++ bitmask ++ " VkFlags\n  deriving (Eq, Ord, Storable)\n\n"
        ++ "type " ++ flags ++ " = " ++ bitmask ++ " FlagMask\n"
        ++ "pattern " ++ flags ++ " :: VkFlags -> " ++ bitmask ++ " FlagMask\n"
        ++ "pattern " ++ flags ++ " n = " ++ bitmask ++ " n\n"
        ++ "deriving instance Bits (" ++ bitmask ++ " FlagMask)\n"
        ++ "deriving instance FiniteBits (" ++ bitmask ++ " FlagMask)\n\n"

commentMulti :: Parser String
commentMulti = do
  many' space
  string "/*"
  manyTill' anyChar (string "*/")
  many' space
  return ""

commentSingle :: Parser String
commentSingle = do
  many' space
  string "//"
  manyTill' anyChar (char '\n')
  many' space
  return ""

comment :: Parser String
comment = choice [commentSingle, commentMulti]

enumDeclaration :: Parser String
enumDeclaration = do
  many' space
  option "" comment
  s1 <- satisfy (isUpper)
  s2 <- many' $ choice [ satisfy (isUpper)
                       , digit
                       , char '_'
                       ]
  manyTill' anyChar (choice [char ',', lookAhead $ char '}'])
  many' space
  return $ s1:s2

wordType :: Parser String
wordType = do
  string "uint"
  bits <- many' digit
  string "_t"
  return $ "Word" ++ bits

floatType :: Parser String
floatType = do
  string "float"
  return $ "Float"

varTypePtr :: Parser String
varTypePtr = do
  typeName <- many' $ choice [ letter_ascii
                             , digit
                             , char '_'
                             ]
  option "" (space >> compileConst)
  char '*'
  return $ "(Ptr " ++ typeName ++ ")"

varType :: Parser String
varType = do
  typeName <- many' $ choice [ letter_ascii
                             , digit
                             , char '_'
                             ]
  return $ typeName

sizeType :: Parser String
sizeType = do
  string "size_t"
  return $ "CSize"

charPtr :: Parser String
charPtr = do
  string "char*"
  return $ "CString"

-- bit ad hoc, could be done better
voidPtrPtr :: Parser String
voidPtrPtr = do
  string "void* VMA_NULLABLE*"
  return $ "(Ptr (Ptr Void))"


voidPtr :: Parser String
voidPtr = do
  string "void*"
  return $ "(Ptr Void)"

ty :: Parser String
ty = do
  choice [ wordType
         , floatType
         , sizeType
         , voidPtrPtr
         , voidPtr
         , charPtr
         , varTypePtr
         , varType
         ]

compileConsts :: Parser [String]
compileConsts = many' compileConst

dynamicSize :: Parser String
dynamicSize = do
  dynSize <- many1' $ choice [ letter_ascii
                             , char '_'
                             , digit
                             ]
  return $ "#{const " ++ dynSize ++ "}"

structField
  :: String
  -> Parser String
structField structName = do
  many' space
  many' comment
  many' space
  option "" (string "const")
  many' space
  option [""] compileConsts
  many' space
  typeName <- ty
  many' space
  option [""] compileConsts
  many' space
  varName <- manyTill' (choice [letter_ascii,digit]) (choice [space, char '[', char ';', lookAhead $ char '}'])
  dynSize <- option "1" dynamicSize
  option ' ' (char ']')
  option ' ' (char ';')
  many' space
  return $ "('FieldMeta \"" ++ varName ++ "\" " ++ typeName ++ " 'False #{offset " ++ structName ++ ", " ++ varName ++ "} " ++ dynSize ++ " 'True 'True)"

ifDef :: Parser String
ifDef = do
  many' space
  option "" comment
  many' space
  ifdef <- choice [string "#if", string "#endif"]
  condition <- manyTill' anyChar (choice [char '\n', char '\r' >> char '\n', char '\r', char '\\' >> char '*', char '\\' >> char '\\'])
  return $ (BS.toString ifdef) ++ condition

ifDefPre :: Parser String
ifDefPre = do
  ifField <- ifDef
  return $ "\n" ++ ifField

ifDefPost :: Parser String
ifDefPost = do
  ifField <- ifDef
  return $ "\n" ++ ifField

firstStructDeclaration
  :: String
  -> Parser String
firstStructDeclaration structName = do
  many' comment
  ifField <- option "" ifDefPre
  many' comment
  struct <- structField structName
  many' comment
  ifField2 <- option "" ifDefPost
  return $ "\n    '[ " ++ ifField ++  struct ++ ifField2

structDeclaration
  :: String
  -> Parser String
structDeclaration structName = do
  many' comment
  ifField <- option "" ifDefPre
  many' comment
  structF <- structField structName
  many' comment
  ifField2 <- option "" ifDefPost
  return $ ifField ++ "\n     , " ++ structF ++ ifField2

createBitPatterns
  :: String
  -> String
  -> String
createBitPatterns maskName bitName = "pattern " ++ bitName ++ " :: " ++ maskName ++ " a\n"
                                      ++ "pattern " ++ bitName ++ " = " ++ maskName ++ " #{const " ++ bitName ++ " }\n\n"

createEnumPatterns
  :: String
  -> String
  -> String
createEnumPatterns enumName valueName = "pattern " ++ valueName ++ " :: " ++ enumName ++ " \n"
                                      ++ "pattern " ++ valueName ++ " = " ++ enumName ++ " #{const " ++ valueName ++ " }\n\n"

createEnumShow
  :: String
  -> String
createEnumShow enumName = "  showsPrec _ " ++ enumName ++ " = showString \"" ++ enumName ++ "\"\n"

vkFlagBits :: Parser String
vkFlagBits = do
  string "typedef enum "
  name <- manyTill' letter_ascii (string "FlagBits"
                                 >> many' space
                                 >> char '{'
                                 )
  let flagBits = name ++ "FlagBits"
  let bitMask = name ++ "Bitmask"
  bitDecs <- many'  enumDeclaration
  char '}'
  return $ "type " ++ flagBits ++ " = " ++ bitMask ++ " FlagBit\n"
        ++ "pattern " ++ flagBits ++ " :: VkFlags -> " ++ bitMask ++ " FlagBit\n"
        ++ "pattern " ++ flagBits ++ " n = " ++ bitMask ++ " n\n\n"
        ++ concat (fmap (createBitPatterns bitMask) bitDecs)
        ++ "instance Show (" ++ bitMask ++ " a) where\n"
        ++ concat (fmap createEnumShow bitDecs) ++ "\n\n"

enum :: Parser String
enum = do
  string "typedef enum "
  name <- manyTill' letter_ascii ( many' space
                                 >> char '{'
                                 )
  enumDecs <- many' enumDeclaration
  char '}'
  return $ "newtype " ++ name ++ " = " ++ name ++ " Word32\n"
        ++ "  deriving (Eq, Ord, Enum, Storable)\n"
        ++ concat (fmap (createEnumPatterns name) enumDecs)
        ++ "instance Show " ++ name ++ " where\n"
        ++ concat (fmap createEnumShow enumDecs) ++ "\n\n"



struct :: Parser String
struct = do
  string "typedef struct "
  structName <- manyTill' letter_ascii ( many' space
                                 >> char '{'
                                 )
  firstStruct <- firstStructDeclaration structName
  structDecs <- manyTill' (structDeclaration structName) (lookAhead $ char '}')
  char '}'
  return $ "data " ++ structName ++ "'\n"
        ++ "type " ++ structName ++ " = VkStruct " ++ structName ++ "'\n\n"
        ++ "instance VulkanMarshal " ++ structName ++ " where\n"
        ++ "  type StructRep " ++ structName ++ " = 'StructMeta \"" ++ structName ++ "\" " ++ structName ++ " #{size " ++ structName ++ " } #{alignment " ++ structName ++ "}"
        ++ firstStruct ++ concat structDecs ++ "\n     ] 'False 'False '[]\n\n"

argType :: Parser String
argType = do
  many' space
  option "" comment
  many' space
  option "" (string "const")
  many' space
  option [""] compileConsts
  many' space
  typeName <- ty
  manyTill' anyChar (choice [char '\n', lookAhead $ char ')'])
  return $ typeName

function :: Parser String
function = do
  choice [string "VMA_CALL_PRE ", string "VKAPI_ATTR "]
  returnType <- choice [ string "VkResult " >> return "IO VkResult"
                       , string "void "     >> return "IO ()"
                       ]
  choice [string "VMA_CALL_POST ", string "VKAPI_CALL "]
  functionName <- manyTill' (choice [letter_ascii, digit]) (char '(')
  argTypes <- manyTill' argType (char ')')
  return $ "foreign import ccall unsafe \"" ++ functionName ++ "\"\n  " ++ functionName ++ " :: "
        ++ intercalate ("\n  " ++ replicate (length functionName) ' ' ++ " -> ") (argTypes ++ [returnType])
        ++ "\n\n"
        ++ "type HS_" ++ functionName ++ " = " ++ intercalate ("\n  " ++ replicate (length functionName) ' ' ++ " -> ") (argTypes ++ [returnType])
        ++ "\n\n"
        ++ "type PFN_" ++ functionName ++ " = FunPtr HS_" ++ functionName ++ "\n\n"

vmaFunctionPtr :: Parser String
vmaFunctionPtr = do
  string "typedef void (VKAPI_PTR* PFN_"
  many' space
  functionName <- manyTill' (choice [letter_ascii, digit]) (char ')')
  char '('
  many' space
  argTypes <- manyTill' argType (char ')')
  return $ "type HS_" ++ functionName ++ " = " ++ intercalate ("\n           " ++ replicate (length functionName) ' ' ++ " -> ") (argTypes ++ ["IO ()"]) ++ "\n\n"
        ++ "type PFN_" ++ functionName ++ " = FunPtr HS_" ++ functionName ++ "\n\n"
--        ++ "foreign import ccall unsafe \"PFN_" ++ functionName ++ "\"\n  pfn_" ++ functionName ++ " :: "
--        ++ intercalate ("\n      " ++ replicate (length functionName) ' ' ++ " -> ") (argTypes ++ ["IO ()"])
--        ++ "\n\n"

vkFunctionPtr :: Parser String
vkFunctionPtr = do
  string "typedef "
  choice [ string "VkResult "
         , string "void "
         , string "PFN_vkVoidFunction "
         ]
  string "(VKAPI_PTR *PFN_"
  many' space
  functionName <- manyTill' (choice [letter_ascii, digit]) (lookAhead $ choice [string ")", string "KHR"])
  khr <- option "" (string "KHR")
  string ")("
  many' space
  manyTill' argType (char ')')
  return $ "foreign import ccall unsafe \"&" ++ functionName ++ "\"\n  pfn_" ++ functionName ++ (BS.toString khr)  ++ " :: FunPtr HS_" ++ functionName ++ (BS.toString khr)
        ++ "\n\n"


fileBreak :: Parser String
fileBreak = do
  many' space
  string "//    IMPLEMENTATION"
  return ""


file :: Parser String
file = do
  lines <- manyTill' (choice [ handle
                             , handle_nonDispatchable
                             , vkFlags
                             , vkFlagBits
                             , enum
                             , struct
                             , function
                             , vmaFunctionPtr
                             , vkFunctionPtr
                             , line
                             ]
                     )
                     (choice [ fileBreak >> return ()
                             , endOfInput
                             ]
                     )
  return . concat $ lines

compileConst :: Parser String
compileConst = do
  many' space
  choice [ string "VMA_NOT_NULL_NON_DISPATCHABLE"
         , string "VMA_NULLABLE_NON_DISPATCHABLE"
         , string "VMA_CALL_PRE"
         , string "VMA_CALL_POST"
         , string "VMA_NULLABLE"
         , string "VMA_LEN_IF_NOT_NULL"
         , string "VMA_NOT_NULL"
         , string "VMA_NULLABLE"
         ]
  option ' ' (char '('
             >> many' (choice [letter_ascii, char '"', char ':'])
             >> char ')')
  many' space
  return ""
