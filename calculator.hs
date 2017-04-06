import Control.Monad
import Data.List

-- Function to read a string and parse double using maybe
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

-- Function to parse operator and operands using maybe
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
calcFunction (x:xs)   "log" =return (log x:xs)
calcFunction (x:xs)   "log10" =return (logBase 10 x:xs)
calcFunction (x:xs)   "log2" =return (logBase 2 x:xs)
calcFunction (x:xs)   "sqrt" =return (sqrt x:xs)
calcFunction (x:xs)   "cbrt" =return (x**(1/3):xs)
calcFunction (x:xs)   "abs" =return (abs(x):xs)
calcFunction xs numberString = liftM (:xs) (readMaybe numberString)


-- function to parse entire string using foldM, calcFunction
solveFunction :: String -> Maybe Double
solveFunction st = do
    [result] <- foldM calcFunction [] (words st)
    return result


-- convert result of Maybe into printable form
printFunction :: Show a => Maybe a -> IO ()
printFunction (Just x) = print x
printFunction n        = print n

-- print the result of calculation and print function
calculator :: String -> IO ()
calculator x = printFunction( solveFunction x)
