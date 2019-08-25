module DataStore2

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) ->
             DataStore
addToStore (MkData schema _ items) newitem = MkData schema _ (addToData items)
  where addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize)
                    (SchemaType schema)
        addToData [] = [newitem]
        addToData (x :: xs) = newitem :: x :: xs

display : (schema : Schema) -> SchemaType schema -> String
display SString x = x
display SInt x = show x
display (s1 .+. s2) (t1, t2) = "(" ++ display s1 t1 ++ ", " ++ display s2 t2 ++
                               ")"

getAction : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getAction pos store = case integerToFin pos (size store) of
                           Nothing => Just ("Out of range", store)
                           Just id => Just (display (schema store) (index id (items store)) ++
                                            "\n", store)

data Command: Schema -> Type where
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Size : Command schema
  Search : String -> Command schema
  Quit : Command schema

parseBySchema : (schema : Schema) -> (str : String) -> Maybe (SchemaType schema)
parseBySchema SString str = Just str
parseBySchema SInt str = Just (cast str)
parseBySchema (x .+. y) str = ?parseBySchema_rhs_3

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String)
               -> Maybe (Command schema)
parseCommand schema "add" str = case parseBySchema schema str of
                                     Nothing => Nothing
                                     Just item => Just (Add item)
parseCommand schema "get" val = case all isDigit (unpack val) of
                                     False => Nothing
                                     True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "size" "" = Just Size
parseCommand schema "search" str = Just (Search str)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
    case parse (schema store) input of
         Nothing => Just ("Invalid command\n", store)
         Just Quit => Nothing
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => case integerToFin pos (size store) of
                                Nothing => Just ("Out of range \n", store)
                                Just id => Just (?show (index id (items store)) ++ "\n", store)
         Just Size => Just ("Number of items: " ++ show (size store) ++ "\n", store)
         Just (Search x) => case findIndices (isInfixOf x) (map ?show (items store)) of
                                 [] => Just ("Item not found\n", store)
                                 idx => Just (concat (map showSearchIdx idx), store)
    where showSearchIdx : Fin (size store) -> String
          showSearchIdx id = show (finToNat id) ++ ": " ++ ?show (index id (items store)) ++ "\n"

