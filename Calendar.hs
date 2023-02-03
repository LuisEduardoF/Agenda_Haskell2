module Calendar where
import Util

getBissexto (a, _) = a
getCalendario (_, a) = a
getMes ns mes = ns!!(mes-1)
isDiaNaoUtil [] dia_n = False
isDiaNaoUtil (x:xs) dia_n | x == dia_n = True
                          | otherwise = isDiaNaoUtil xs dia_n

checkDiaUtil mes_n dia_n calendario = do
    let cal = getCalendario calendario
    
    not (isDiaNaoUtil (getMes cal mes_n) dia_n)

format_calendario (x : xs) = do
    let bissexto = read x :: Bool
    let calendario = [map getInt y | y <- (map words xs)]
    
    (bissexto, calendario)

read_calendario = do
    calendario_input <- readFile "calendario.txt"
    let calendario_text = lines calendario_input
    
    return (format_calendario calendario_text)

getDiasMes calendar = [31, if (getBissexto calendar) then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]