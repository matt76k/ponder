module Text.Ponder.Pos
( SourceName, Line, Column
, SourcePos
, sourceLine, sourceColumn, sourceName
, newPos, initialPos
, updatePosChar, backPosChar
) where

type SourceName = String
type Line       = Int
type Column     = Int

data SourcePos = SourcePos SourceName !Line !Column
  deriving ( Eq, Ord )

newPos :: SourceName -> Line -> Column -> SourcePos
newPos name line column = SourcePos name line column

initialPos :: SourceName -> SourcePos
initialPos name = newPos name 1 1

sourceName :: SourcePos -> SourceName
sourceName (SourcePos name _line _column) = name

sourceLine :: SourcePos -> Line
sourceLine (SourcePos _name line _column) = line

sourceColumn :: SourcePos -> Column
sourceColumn (SourcePos _name _line column) = column

updatePosChar :: SourcePos -> SourcePos
updatePosChar (SourcePos name line column)
  = SourcePos name line (column+1)

backPosChar :: SourcePos -> SourcePos
backPosChar (SourcePos name line column)
  = SourcePos name line (column-1)

instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn = " (line " ++ show line ++
                       ", column " ++ show column ++
                       ")"
