-- |
module LstCom.Constants where

import Prelude

c_TIMEOUT_LST_COM :: Int
c_TIMEOUT_LST_COM = 10000000

c_TIMEOUT_LST_COM_TXT :: String
c_TIMEOUT_LST_COM_TXT = "Die Auswertung der List Comprehension dauert zu lange"

c_MAX_STEPS :: Int
c_MAX_STEPS = 1000

c_MAX_STEPS_TXT :: String
c_MAX_STEPS_TXT = "Die maximal mögliche Schrittanzahl wurde überschritten"

c_MAX_LIST_ELEMENTS :: Int
c_MAX_LIST_ELEMENTS = c_MAX_STEPS

c_MAX_LIST_ELEMENTS_TXT :: String
c_MAX_LIST_ELEMENTS_TXT = "Die maximal mögliche Anzahl an Listenelementen wurde überschritten"

c_NO_RESULT :: String
c_NO_RESULT = "-"

c_LST_COM_EXPR_NAME :: String
c_LST_COM_EXPR_NAME = ".exp"

c_COMMENT :: String
c_COMMENT = ".com"

c_COM_NEW_LST_COM_EXPR :: String
c_COM_NEW_LST_COM_EXPR = "Berechne neues Listenelement: "

c_COM_NO_LST_COM_EXPR :: String
c_COM_NO_LST_COM_EXPR = "Neues Listenelement wird aufgrund von False-Guard nicht berechnet"

c_COM_EVAL_EXPR_1 :: String
c_COM_EVAL_EXPR_1 = "Setze "

c_COM_EVAL_EXPR_2 :: String
c_COM_EVAL_EXPR_2 = " auf den Wert: "

c_COM_DONT_EVAL_EXPR :: String
c_COM_DONT_EVAL_EXPR = " wird aufgrund von False-Guard nicht berechnet"

errorAdd :: String
         -> String
errorAdd input = e_ERROR ++ input ++ e_POSITION

e_LOGIC_ERROR :: String
e_LOGIC_ERROR = "Das Programm enthält einen Logikfehler und kann aus diesem Grund Ihre Eingabe leider nicht auswerten"

e_ERROR :: String
e_ERROR = "Fehler - "

e_POSITION :: String
e_POSITION = " : "

e_WRONG_GUARD :: String
e_WRONG_GUARD = errorAdd "Guard muss zu True oder False auswerten"

e_WRONG_GENERATOR :: String
e_WRONG_GENERATOR = errorAdd "Generator muss zu einer Liste auswerten"

e_WRONG_PATTERN :: String
e_WRONG_PATTERN = errorAdd "Falsches Pattern für Pattern Matching"

e_CANNOT_EXECUTE :: String
e_CANNOT_EXECUTE = errorAdd "Der Ausdruck kann nicht ausgewertet werden"

e_BACKSLASH_NOT_ALLOWED :: String
e_BACKSLASH_NOT_ALLOWED = errorAdd "Backslash ist in dieser Anwendung verboten"

e_LST_COM_WRONG_START :: String
e_LST_COM_WRONG_START = errorAdd "List Comprehension muss mit eckiger Klammer auf starten"

e_LST_COM_WRONG_END :: String
e_LST_COM_WRONG_END = errorAdd "List Comprehension muss mit eckiger Klammer zu enden"

e_LST_COM_NO_VER_LINE :: String
e_LST_COM_NO_VER_LINE = errorAdd "List Comprehension muss '|' beinhalten"

e_LST_COM_MANY_VER_LINES :: String
e_LST_COM_MANY_VER_LINES = errorAdd "List Comprehension darf nur ein '|' beinhalten"

e_LST_COM_NO_LEFT_EXPR :: String
e_LST_COM_NO_LEFT_EXPR = errorAdd "List Comprehension benötigt einen Ausdruck links von '|'"

e_LST_COM_NO_QUAL :: String
e_LST_COM_NO_QUAL = errorAdd "List Comprehension benötigt mindestens einen Qualifier rechts von '|'"

e_VAR_NOT_DECLARED :: String
e_VAR_NOT_DECLARED = errorAdd "Variable wurde nicht deklariert"

e_VAR_ALREADY_DECLARED :: String
e_VAR_ALREADY_DECLARED = errorAdd  "Variable kann nicht doppelt deklariert werden"

e_QUOTATION_APOSTROPHE_NOT_CLOSED :: String
e_QUOTATION_APOSTROPHE_NOT_CLOSED = errorAdd "Anführungszeichen/Apostroph wurde nicht geschlossen"

e_INV_COMMA_NOT_CLOSED :: String
e_INV_COMMA_NOT_CLOSED = errorAdd "Backquote wurde nicht geschlossen"

e_BRACKET_NOT_CLOSED :: String
e_BRACKET_NOT_CLOSED = errorAdd "Klammer wurde nicht geschlossen"

e_BRACKET_NOT_OPENED :: String
e_BRACKET_NOT_OPENED = errorAdd "Klammer wurde nicht geöffnet"

e_NO_INPUT :: String
e_NO_INPUT = errorAdd "Keine Eingabe"

e_EMPTY_LIST :: String
e_EMPTY_LIST = errorAdd "Leere Liste nicht erlaubt"

e_EMPTY_TUPLE :: String
e_EMPTY_TUPLE = errorAdd "Leeres Tupel nicht erlaubt"

e_FORBIDDEN_VAR_NAME :: String
e_FORBIDDEN_VAR_NAME = errorAdd "Variablenname nicht erlaubt"

e_INPUT_OUT_OF_TPL_LST :: String
e_INPUT_OUT_OF_TPL_LST = errorAdd "Eingabe außerhalb von Liste/Tupel"

e_COMMA_WITHOUT_INPUT :: String
e_COMMA_WITHOUT_INPUT = errorAdd "Keine Eingabe vor oder nach Komma"

e_COLON_WITHOUT_INPUT :: String
e_COLON_WITHOUT_INPUT = errorAdd "Keine Eingabe vor oder nach Doppelpunkt"

e_MINUS_WITHOUT_INPUT :: String
e_MINUS_WITHOUT_INPUT = errorAdd "Keine Eingabe nach Minus"

e_SIGN_NOT_ALLOWED :: String
e_SIGN_NOT_ALLOWED = errorAdd "Ungültiges Zeichen"

e_NUM_SIGN_NOT_ALLOWED :: String
e_NUM_SIGN_NOT_ALLOWED = errorAdd "Ungültiges Zeichen in Fließkommazahl"

e_UNKNOWN_FUNCTION :: String
e_UNKNOWN_FUNCTION = errorAdd "Unbekannte Funktion"

e_FUNCTION_NOT_ALLOWED :: String
e_FUNCTION_NOT_ALLOWED = errorAdd "Die Funktion ist in diesem Programm verboten"

e_FUNCTION_NEEDS_FUNC_AS_ARG :: String
e_FUNCTION_NEEDS_FUNC_AS_ARG = errorAdd "Die Funktion benötigt eine Funktion als Argument"

e_FUNCTION_NOT_ALLOWED_AS_LEFT_SEC :: String
e_FUNCTION_NOT_ALLOWED_AS_LEFT_SEC = errorAdd "Die Funktion darf nicht in einer linken Sektion verwendet werden"

e_FUNCTION_NOT_ENOUGH_ARGS :: String
e_FUNCTION_NOT_ENOUGH_ARGS = errorAdd "Der Funktion wurden zu wenige Argumente übergeben"

e_FUNCTION_TOO_MUCH_ARGS :: String
e_FUNCTION_TOO_MUCH_ARGS = errorAdd "Der Funktion wurden zu viele Argumente übergeben"

e_FUNCTION_NEED_NOT_ENOUGH_ARGS :: String
e_FUNCTION_NEED_NOT_ENOUGH_ARGS = errorAdd "Die Funktion benötigt zu wenige Argumente"

e_FUNCTION_NEED_TOO_MUCH_ARGS :: String
e_FUNCTION_NEED_TOO_MUCH_ARGS = errorAdd "Die Funktion benötigt zu viele Argumente"

e_ARITH_SEQ_NO_START :: String
e_ARITH_SEQ_NO_START = errorAdd "Eine arithmetische Sequenz braucht einen Startwert"

e_ARITH_SEQ_NO_END :: String
e_ARITH_SEQ_NO_END = errorAdd "Eine arithmetische Sequenz braucht einen Endwert"

e_ARITH_SEQ_TOO_MUCH_ARGS :: String
e_ARITH_SEQ_TOO_MUCH_ARGS = errorAdd "Die arithmetische Sequenz hat zu viele Argumente"

e_IF_WITHOUT_THEN_ELSE :: String
e_IF_WITHOUT_THEN_ELSE = errorAdd "If ohne then/else"

e_IF_WITHOUT_EXPR :: String
e_IF_WITHOUT_EXPR = errorAdd "If ohne Ausdruck"

e_THEN_WITHOUT_EXPR :: String
e_THEN_WITHOUT_EXPR = errorAdd "Then ohne Ausdruck"

e_ELSE_WITHOUT_EXPR :: String
e_ELSE_WITHOUT_EXPR = errorAdd "Else ohne Ausdruck"

e_POINT_WITHOUT_NUM :: String
e_POINT_WITHOUT_NUM = errorAdd "Vor und nach einem Punkt muss eine Zahl stehen"

e_INVALID_CHAR :: String
e_INVALID_CHAR = errorAdd "Ungültiges Char"

e_INPUT_OUT_OF_CHAR :: String
e_INPUT_OUT_OF_CHAR = errorAdd "Eingabe außerhalb von Char"

e_INVALID_STRING :: String
e_INVALID_STRING = errorAdd "Ungültiger String"

e_INPUT_OUT_OF_STRING :: String
e_INPUT_OUT_OF_STRING = errorAdd "Eingabe außerhalb von String"

c_KEYWORDS :: [String]
c_KEYWORDS = ["if", "then", "else", "case", "of", "class", "data", "default"
            , "deriving", "do", "foreign", "import", "infix", "infixl", "infixr"
            , "instance", "let", "in", "module", "newtype", "qualified", "type"
            , "_"]

c_SYMBOL_FUNCS :: [String]
c_SYMBOL_FUNCS = ["!!", "*", "/", "+", "-", "^", "++", "&&", "||", "$", "=="
                , "/=", "<", "<=", ">=", ">"]

c_REJECTED_PRELUDE_FUNCS :: [String]
c_REJECTED_PRELUDE_FUNCS = ["appendFile", "catch", "cycle", "either"
                          , "enumFrom", "enumFromThen", "error", "fail", "fmap"
                          , "getChar", "getContents", "getLine", "interact"
                          , "ioError", "iterate", "lookup", "maybe", "mapM"
                          , "mapM_", "maxBound", "minBound", "print", "putChar"
                          , "putStr", "putStrLn",  "read", "readFile", "readIO"
                          , "readList", "readLn", "readParen", "reads"
                          , "readsPrec", "repeat", "return", "sequence"
                          , "sequence_", "showChar", "showList", "showParen"
                          , "shows", "showsPrec", "showString", "toEnum"
                          , "undefined", "userError", "writeFile"]

c_FUNCS_WITH_1_FUNC_ARG :: [(String, Int)]
c_FUNCS_WITH_1_FUNC_ARG = [("break", -1), ("concatMap", -1), ("dropWhile", -1)
                         , ("filter", -1), ("foldl", -2), ("foldl1", -2)
                         , ("foldr", -2), ("foldr1", -2), ("map", -1)
                         , ("takeWhile", -1), ("flip", -2), ("scanl", -2)
                         , ("scanl1", -2), ("scanr", -2), ("scanr1", -2)
                         , ("zipWith", -2), ("zipWith3", -3), ("curry", -1)
                         , ("uncurry", -2)]

c_FUNCS_WITH_2_FUNC_ARGS :: [(String, Int, Int)]
c_FUNCS_WITH_2_FUNC_ARGS = [("until", -1, -1)]

c_PRELUDE_FUNCS :: [(String, Int)]
c_PRELUDE_FUNCS = [("abs", 1), ("acos", 1), ("acosh", 1), ("all", 2), ("and", 1)
                 , ("any", 2), ("asin", 1), ("asinh", 1), ("asTypeOf", 2)
                 , ("atan", 1), ("atan2", 2), ("atanh", 1), ("break", 2)
                 , ("ceiling", 1), ("compare", 2), ("concat", 1)
                 , ("concatMap", 2), ("const", 2), ("cos", 1), ("cosh", 1)
                 , ("curry", 3), ("decodeFloat", 1), ("div", 2), ("divMod", 2)
                 , ("drop", 2), ("dropWhile", 2), ("elem", 2)
                 , ("encodeFloat", 2), ("enumFromThenTo", 3), ("enumFromTo", 2)
                 , ("even", 1), ("exp", 1), ("exponent", 1), ("filter", 2)
                 , ("flip", 3), ("floatDigits", 1), ("floatRadix", 1)
                 , ("floatRange", 1), ("floor", 1), ("foldl", 3), ("foldl1", 2)
                 , ("foldr", 3), ("foldr1", 2), ("fromEnum", 1)
                 , ("fromInteger", 1), ("fromIntegral", 1), ("fromRational", 1)
                 , ("fst", 1), ("gcd", 2), ("head", 1), ("id", 1), ("init", 1)
                 , ("isDenormalized", 1), ("isIEEE", 1) , ("isInfinite", 1)
                 , ("isNaN", 1), ("isNegativeZero", 1), ("last", 1)
                 , ("lcm", 2), ("length", 1), ("lex", 1), ("lines", 1)
                 , ("log", 1), ("logBase", 2), ("map", 2), ("max", 2)
                 , ("maximum", 1), ("min", 2), ("minimum", 1), ("mod", 2)
                 , ("negate", 1), ("not", 1), ("notElem", 2), ("null", 1)
                 , ("odd", 1), ("or", 1), ("otherwise", 0), ("pi", 0)
                 , ("pred", 1), ("product", 1), ("properFraction", 1)
                 , ("quot", 2), ("quotRem", 2), ("realToFrac", 1), ("recip", 1)
                 , ("rem", 2), ("replicate", 2), ("reverse", 1), ("round", 1)
                 , ("scaleFloat", 2), ("scanl", 3), ("scanl1", 2), ("scanr", 3)
                 , ("scanr1", 2), ("seq", 2), ("show", 1), ("significand", 1)
                 , ("signum", 1), ("sin", 1), ("sinh", 1), ("snd", 1)
                 , ("splitAt", 2), ("sqrt", 1), ("subtract", 2), ("succ", 1)
                 , ("sum", 1), ("tail", 1), ("take", 2), ("takeWhile", 2)
                 , ("tan", 1), ("tanh", 1), ("toInteger", 1), ("toRational", 1)
                 , ("truncate", 1), ("uncurry", 2), ("unlines", 1), ("until", 3)
                 , ("unwords", 1), ("unzip", 1), ("unzip3", 1), ("words", 1)
                 , ("zip", 2), ("zip3", 3), ("zipWith", 3), ("zipWith3", 4)]

c_PRELUDE_FUNCS_NAMES :: [String]
c_PRELUDE_FUNCS_NAMES =
  let foldFunc = \lst (func, _) -> lst ++ [func]
      lst = foldl foldFunc [] c_PRELUDE_FUNCS
  in lst

c_FUNCS_WITH_1_FUNC_ARG_NAMES :: [String]
c_FUNCS_WITH_1_FUNC_ARG_NAMES =
  let foldFunc = \lst (func, _) -> lst ++ [func]
      lst = foldl foldFunc [] c_FUNCS_WITH_1_FUNC_ARG
  in lst

c_FUNCS_WITH_2_FUNC_ARGS_NAMES :: [String]
c_FUNCS_WITH_2_FUNC_ARGS_NAMES =
  let foldFunc = \lst (func, _, _) -> lst ++ [func]
      lst = foldl foldFunc [] c_FUNCS_WITH_2_FUNC_ARGS
  in lst
