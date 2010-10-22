module Distribution.DocIdx.Common where

-- | Elements of the table of contents.
data TocItem = TocItem String String
             | TocSeparator
             | TocNewline
               deriving (Eq, Ord, Show)
