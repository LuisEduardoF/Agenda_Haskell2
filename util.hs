module Util where

getInt :: String -> Int
getInt str = read str

strToChar :: Char -> String
strToChar c = [c]

wordsWhen  :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

getTurnoManha = [8..12]
getTurnoTarde = [14..18]
getHorarioComercial = getTurnoManha ++ getTurnoTarde

is_horario_comercial :: Int -> Bool
is_horario_comercial horario_ini = (horario_ini) `elem` getHorarioComercial

is_valid_compromisso :: Int -> Int -> Bool
is_valid_compromisso horario_ini duracao = (horario_ini + duracao) <= 18 && (is_horario_comercial horario_ini) && (if ((horario_ini) `elem` getTurnoManha) then (horario_ini + duracao) <= 12 else True)

charFound c [] = False
charFound c (x:xs)
    | c == x = True
    | otherwise = charFound c xs

