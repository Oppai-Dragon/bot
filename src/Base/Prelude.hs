module Base.Prelude
  ( wordsBy
  , findNotEqual
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

findNotEqual :: (Eq a, Show a) => [a] -> [a] -> IO ()
findNotEqual [] [] = putStrLn "All is equal"
findNotEqual [] arr =
  putStrLn "First array ended. Second remaining array" >> print arr
findNotEqual arr [] =
  putStrLn "Second array ended. First remaining array" >> print arr
findNotEqual (x:restX) (y:restY) =
  if x == y
    then findNotEqual restX restY
    else putStrLn $ show x <> " and " <> show y
