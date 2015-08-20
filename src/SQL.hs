module SQL
    ( Statement (Statement),
      Field (Field),
      Table (Table),
      Operator (GT, LT, IS),
      Literal (Literal),
      Where (Where),
      With (With),
      select,
      from,
      where_,
      group,
      render,
      with
    ) where

import qualified Data.List as List

data Table =
  Table String

data Field =
  Field String

data Operator = GT | LT | IS | EQ

data Literal =
  Literal String

data Where =
  Where Field Operator Literal

data Alias =
  Alias String

data With =
  With Statement Alias

data Statement = 
  Statement {
    select :: Maybe [Field],
    from :: [Table],
    where_ :: Maybe [Where],
    group :: Maybe [String],
    with :: Maybe With
  }

render :: Statement -> String
render stat = 
  renderSelect stat ++ "\n" ++
  renderFrom stat ++ "\n" ++
  renderWhere stat ++ "\n" ++
  renderGroup stat

renderSelect stat = "select " ++ 
  let selects = select stat
  in case selects of
    Nothing -> "*"
    Just xs -> List.intercalate ", " 
      (map (\(Field name) -> name) xs)

renderFrom stat = "from " ++
  (List.intercalate ", "
    (map (\(Table name) -> name) (from stat)))

renderWhere stat = 
  let wheres = where_ stat
  in case wheres of
    Nothing -> ""
    Just clauses ->
      let pieces = map renderWhereClause clauses
      in "where " ++ (List.intercalate " and " pieces)

renderWhereClause 
  (Where
    (Field field)
    op
    (Literal literal)
  ) = field ++ " " ++ (renderOperator op) ++ " " ++ literal

renderOperator op =
  case op of
    SQL.GT -> ">"
    SQL.LT -> "<"
    SQL.IS -> "is"

renderGroup stat =
  let groups = group stat
  in case groups of
    Nothing -> ""
    Just clauses ->
      "group by " ++ (List.intercalate ", " clauses)