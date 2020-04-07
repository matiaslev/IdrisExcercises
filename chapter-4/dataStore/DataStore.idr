module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items') = item :: addToData items'

data Command = Add String
               | Get Integer
               | Size
               | Search String
               | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" _ = Just Size
parseCommand "search" substring = Just (Search substring)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                             case integerToFin pos (size store) of
                                   Nothing => Just ("Out of range\n", store)
                                   (Just id) => Just (index id store_items ++ "\n", store)

getCases : Nat -> String -> Vect n String -> String
getCases idx str [] = ""
getCases idx str (head :: tail) = case isInfixOf str head of
                              False => getCases (idx + 1)str tail
                              True =>  (show idx ++ ": " ++ head) ++ "\n" ++ getCases (idx+1) str tail

search : String -> DataStore -> Maybe (String, DataStore)
search substring store = let cases = getCases 0 substring (items store) in
                             case cases == "" of
                                   False => Just (cases, store)
                                   True => Just ("can't find cases for: " ++ substring ++ ", sorry.\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos) => getEntry pos store
                              Just Size => Just ("Size: " ++ show (size store) ++ "\n", store)
                              Just (Search substring) => search substring store
                              Just Quit => Nothing

main : IO()
main = replWith (MkData _ []) "Command: " processInput
