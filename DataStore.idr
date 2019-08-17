module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

-- Why not define datastore as a Vect n String?
-- also addToData is fairly ugly.  Idealy we would want MkData (S size) (items ++ [y])
-- problem there is that we need a proof that (S size) is (size + 1) and I don't
-- know how to express that in Idris.with (_)
  -- know how to express that in Idris.| with_pat = ?know_rhs
addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) y = MkData (S size) (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [y]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Size -- ex 1
             | Search String  -- ex 2,3
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)  -- good luck remembering the ltrim
                                                                 -- when re-implementing.

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
    case parse input of
         Nothing => Just ("Invalid command\n", store)
         Just Quit => Nothing
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => case integerToFin pos (size store) of
                                Nothing => Just ("Out of range \n", store)
                                Just id => Just (index id (items store) ++ "\n", store)
         Just Size => Just ("Number of items: " ++ show (size store) ++ "\n", store)
         Just (Search x) => case findIndices (isInfixOf x) (items store) of
                                 [] => Just ("Item not found\n", store)
                                 idx => Just (concat (map showSearchIdx idx), store)
    where showSearchIdx : Fin (size store) -> String
          showSearchIdx id = show (finToNat id) ++ ": " ++ index id (items store) ++ "\n"


main : IO ()
main = replWith (MkData _ []) "Command: " processInput

