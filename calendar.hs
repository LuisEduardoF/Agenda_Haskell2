module Calendar where

import Schedule

getInt :: String -> Int
getInt str = read str

format_calendario (x : xs) = do
    let bissexto = read x :: Bool
    let calendario = [map getInt y | y <- (map words xs)]
    
    (bissexto, calendario)

read_calendario = do
    calendario_input <- readFile "calendario.txt"
    let calendario_text = lines calendario_input
    
    return (format_calendario calendario_text)