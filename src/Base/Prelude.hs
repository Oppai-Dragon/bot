module Base.Prelude
  ( wordsBy
  ) where

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy _ [] = []
wordsBy func str =
  let part1 = takeWhile func str
      rest =
        case dropWhile func str of
          [] -> []
          [_] -> []
          x -> tail x
   in part1 : wordsBy func rest
