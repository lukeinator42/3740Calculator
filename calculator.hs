import Control.Monad
import Data.List

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

calcFunction :: [Double] -> String -> Maybe [Double]
calcFunction (x:y:ys) "*" = return ((x * y):ys)
calcFunction (x:y:ys) "+" = return ((x + y):ys)
calcFunction (x:y:ys) "-" = return ((y - x):ys)
calcFunction (x:y:ys) "/" = return ((y / x):ys)
calcFunction (x:y:ys) "^" = return ((y ** x):ys)
calcFunction (x:xs)   "ln" =return (log x:xs)
calcFunction (x:xs)   "cos" =return (cos x:xs)
calcFunction (x:xs)   "sin" =return (sin x:xs)
calcFunction (x:xs)   "tan" =return (tan x:xs)
calcFunction (x:xs)   "acos" =return (acos x:xs)
calcFunction (x:xs)   "asin" =return (asin x:xs)
calcFunction (x:xs)   "atan" =return (atan x:xs)
calcFunction xs numberString = liftM (:xs) (readMaybe numberString)


solveFunction :: String -> Maybe Double
solveFunction st = do
    [result] <- foldM calcFunction [] (words st)
    return result

printFunction :: Show a => Maybe a -> IO ()
printFunction (Just x) = print x
printFunction n        = print n

calculator :: String -> IO ()
calculator x = printFunction( solveFunction x)
