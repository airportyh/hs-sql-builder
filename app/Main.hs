module Main where

import qualified SQL

main :: IO ()
main = 
  let statement = SQL.Statement {
    SQL.select = Just [SQL.Field "id", SQL.Field "name"],
    SQL.from = [SQL.Table "users"],
    SQL.where_ = Just [
      SQL.Where (SQL.Field "age") SQL.GT (SQL.Literal "8"),
      SQL.Where (SQL.Field "password") SQL.IS (SQL.Literal "null")
    ],
    SQL.group = Just ["name", "age"],
    SQL.with = Nothing
  }
  in putStrLn $ SQL.render statement

