module OP where
    
import BST
import Util
import Agenda
import Calendar
import Data.List

data Result = Bool Bool | Save | Exit | BST BSTSchedule deriving (Eq, Show)

-- BST get
getBST :: Result -> BSTSchedule
getBST (BST bst) = bst

-- Sai do programa
exit :: BSTSchedule -> IO Result
exit bst = do
    return Exit

-- Recarrega a agenda
do_reload_agenda :: IO Result
do_reload_agenda = do
    agenda <- read_agenda
    let bst = (constructor_bst agenda Empty)

    return (BST bst)

-- Checa disponibilidade
read_is_free :: IO (Int, Int, Int, Int)
read_is_free = do
    putStrLn ("Mes:")
    mes_input <- getLine
    let mes_n = read mes_input :: Int

    putStrLn ("Dia:")
    dia_input <- getLine
    let dia_n = read dia_input :: Int
    
    putStrLn ("Horario:")
    horario_input <- getLine
    let horario_ini = read horario_input :: Int

    putStrLn ("Duracao:")
    duracao_input <- getLine
    let duracao = read duracao_input :: Int

    return (mes_n, dia_n, horario_ini, duracao)

do_is_free:: BSTSchedule -> (Bool, [[Int]]) -> IO Result
do_is_free bst calendario = do
    (mes_n, dia_n, horario_ini, duracao) <- read_is_free

    if ((checkDiaUtil mes_n dia_n calendario) && (is_valid_compromisso horario_ini duracao)) then do
        return (Bool (not (searchSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst)))
    else
        return (Bool False)

-- Adiciona Compromisso
read_insercao_compromisso = do
    putStrLn ("Mes:")
    mes_input <- getLine
    let mes_n = read mes_input :: Int

    putStrLn ("Dia:")
    dia_input <- getLine
    let dia_n = read dia_input :: Int
    
    putStrLn ("Horario:")
    horario_input <- getLine
    let horario_ini = read horario_input :: Int

    putStrLn ("Duracao:")
    duracao_input <- getLine
    let duracao = read duracao_input :: Int

    putStrLn ("Tipo:")
    tipo <- getLine

    return (mes_n, dia_n, horario_ini, duracao, tipo)
read_type tipo = do
    if(tipo == "presencial") then do
        putStrLn ("Local:")
        local <- getLine
        return [local]
    else do
        putStrLn ("Ferramenta:")
        ferramenta <- getLine

        putStrLn ("Link:")
        link <- getLine
        return [ferramenta, link]
insercao_compromisso mes_n dia_n horario_ini duracao tipo metadata calendario bst = do
    if (not (searchSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst) && (is_valid_compromisso horario_ini duracao) && (checkDiaUtil mes_n dia_n calendario)) then do
        case tipo of
            "presencial" -> return (BST (insertSchedule (ScheduleP {month = mes_n, day = dia_n, start = horario_ini, duration = duracao, t = tipo, local = metadata!!0}) bst))
            "videoconferencia" -> return (BST (insertSchedule (ScheduleV {month = mes_n, day = dia_n, start = horario_ini, duration = duracao, t = tipo, local = metadata!!0, link = metadata!!1}) bst))
    else
        return (BST bst)
do_insercao_compromisso bst calendario = do
    (mes_n, dia_n, horario_ini, duracao, tipo) <- read_insercao_compromisso
    metadata <- read_type tipo
    insercao_compromisso mes_n dia_n horario_ini duracao tipo metadata calendario bst

-- Reagendamento de compromisso
read_reagendamento_compromisso = do
    putStrLn ("Mes Antigo:")
    mes_input <- getLine
    let mes_n = read mes_input :: Int
    
    putStrLn ("Dia Antigo:")
    dia_input <- getLine
    let dia_n = read dia_input :: Int
    
    putStrLn ("Horario Antigo:")
    horario_input <- getLine
    let horario_ini = read horario_input :: Int

    putStrLn ("Duraca Antigo:")
    duracao_input <- getLine
    let duracao = read duracao_input :: Int
    
    putStrLn ("Mes Novo:")
    newMes_input <- getLine
    let newMes_n = read newMes_input :: Int

    putStrLn ("Dia Novo:")
    newDia_input <- getLine
    let newDia_n = read newDia_input :: Int
    
    putStrLn ("Horario Novo:")
    newHorario_input <- getLine
    let newHorario_ini = read newHorario_input :: Int

    putStrLn ("Duracao Novo:")
    newDuracao_input <- getLine
    let newDuracao = read newDuracao_input :: Int
    
    putStrLn ("Tipo:")
    tipo <- getLine

    return (mes_n, dia_n, horario_ini, duracao, newMes_n, newDia_n, newHorario_ini, newDuracao, tipo)

do_reagendamento_compromisso bst calendario = do
    (mes_n, dia_n, horario_ini, duracao, newMes_n, newDia_n, newHorario_ini, newDuracao, tipo) <- read_reagendamento_compromisso
    
    let newBst = deleteSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst
    metadata <- read_type tipo

    if (bst == newBst) then 
        insercao_compromisso mes_n dia_n horario_ini duracao tipo metadata calendario bst
    else if (not (searchSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst)) then
        insercao_compromisso newMes_n newDia_n newHorario_ini newDuracao tipo metadata calendario bst
    else 
        return (BST bst)

-- Insercao de compromisso mais breve
read_insercao_compromisso_breve = do
    putStrLn ("Mes:")
    mes_input <- getLine
    let mes_n = read mes_input :: Int

    putStrLn ("Dia:")
    dia_input <- getLine
    let dia_n = read dia_input :: Int

    putStrLn ("Duracao:")
    duracao_input <- getLine
    let duracao = read duracao_input :: Int
    
    putStrLn ("Tipo:")
    tipo <- getLine
    
    return (mes_n, dia_n, duracao, tipo)
try_insercao_breve :: BSTSchedule -> (Bool, [[Int]]) -> Int -> Int -> [Int] -> Int -> String -> [String] -> IO Result
try_insercao_breve bst calendario mes_n dia_n [] duracao tipo metadata = return (BST bst)
try_insercao_breve bst calendario mes_n dia_n (x:xs) duracao tipo metadata = do
    newBST <- insercao_compromisso mes_n dia_n x duracao tipo metadata calendario bst
    if (getBST (newBST) /= bst) then
        return newBST
    else
        try_insercao_breve bst calendario mes_n dia_n xs duracao tipo metadata
insercao_compromisso_breve bst calendario mes_n dia_n duracao tipo metadata = do
    let horario_comercial = getHorarioComercial
    try_insercao_breve bst calendario mes_n dia_n horario_comercial duracao tipo metadata

do_insercao_compromisso_breve :: BSTSchedule -> (Bool, [[Int]]) -> IO Result
do_insercao_compromisso_breve bst calendario = do
    (mes_n, dia_n, duracao, tipo) <- read_insercao_compromisso_breve

    metadata <- read_type tipo
    
    insercao_compromisso_breve bst calendario mes_n dia_n duracao tipo metadata

-- Cancelamento de compromisso
read_cancelamento_compromisso = do
    putStrLn ("Mes:")
    mes_input <- getLine
    let mes_n = read mes_input :: Int

    putStrLn ("Dia:")
    dia_input <- getLine
    let dia_n = read dia_input :: Int
    
    putStrLn ("Horario:")
    horario_input <- getLine
    let horario_ini = read horario_input :: Int

    return (mes_n, dia_n, horario_ini)
do_cancelamento_compromisso bst calendario = do
    (mes_n, dia_n, horario_ini) <- read_cancelamento_compromisso

    return (BST (deleteSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = 0}) bst))

-- Gravar Agenda
write_schedule_cmp_p p = (t p) ++ "\n" ++ show (start p) ++ "," ++ show (duration p) ++ "\n" ++ local p ++ "\n" ++ if (t p == "videoconferencia") then link p else ""
write_schedule [] mes dia = ""
write_schedule (x:xs) mes dia | (month x) /= mes = (if (mes /= 0) then "\n" else "") ++ show (month x) ++ "\n" ++ (write_schedule (x:xs) (month x) dia)
                              | (day x)   /= dia = show (day x) ++ "\n" ++ (write_schedule (x:xs) mes (day x))
                              | otherwise = (write_schedule_cmp_p x) ++ (write_schedule xs mes dia)
write_agenda bst = do
    let agenda = inOrder bst
    
    writeFile "agenda.txt" (write_schedule agenda 0 0)

    return (BST bst)

-- Intervalo
read_insercao_compromisso_intervalo = do
    putStrLn ("Mes:")
    mes_input <- getLine
    let mes_n = read mes_input :: Int

    putStrLn ("Dia:")
    dia_input <- getLine
    let dia_n = read dia_input :: Int
    
    putStrLn ("Duracao:")
    duracao_input <- getLine
    let duracao = read duracao_input :: Int

    putStrLn ("Prazo:")
    prazo_input <- getLine
    let prazo = read prazo_input :: Int

    return (mes_n, dia_n, duracao, prazo)
interval_dia mes dia [x] bst = []
interval_dia mes dia (x:xs) bst | not (searchSchedule (Schedule {month = mes, day = dia, start = x, duration = 1}) bst) = 0 : interval_dia mes dia xs bst
                                | otherwise = 1 : interval_dia mes dia xs bst
create_interval mes prazo dia_aux calendar bst  | prazo == dia_aux = []
                                                | not (checkDiaUtil mes dia_aux calendar) = [1,1,1,1,1,1,1,1] ++ create_interval mes prazo (dia_aux+1) calendar bst
                                                | otherwise = interval_dia mes dia_aux getTurnoManha bst ++ interval_dia mes dia_aux getTurnoTarde bst ++ create_interval mes prazo (dia_aux+1) calendar bst
set_interval atual [] aux = [] 
set_interval atual (x:xs) aux | x == atual = set_interval atual xs (aux+1)
                              | otherwise = (aux, atual) : set_interval x (x:xs) 0

interval mes dia prazo calendar bst = do
    let hours = create_interval mes (prazo+dia) dia calendar bst
    let intervalo = set_interval (hours!!0) hours 0
    intervalo

locate_interval int (x:xs) c | x == int = c
                             | otherwise = locate_interval int xs (c+((\(x, y) -> x) x))

safe_minimum [] = (-1, 0)
safe_minimum interval = minimum ( interval ) 
intervalo_minimo interval duracao = safe_minimum ( filter (\(x, y) -> x >= duracao && y == 0) interval )

safe_maximum [] = (-1, 0)
safe_maximum interval = maximum ( interval ) 
intervalo_maximo interval duracao = safe_maximum ( filter (\(x, y) -> x >= duracao && y == 0) interval )

getStartByHours hours = ([8..11] ++ [14..17])!!(mod hours 8)
getDayByHours hours = div hours 8


schedule_intervalo_minimo mes dia prazo duracao bst calendar = do
    let int = interval mes dia prazo calendar bst
    let int_min = (intervalo_minimo int duracao)
    if  (int_min == (-1, 0)) then
        bst
    else do
        let hours = locate_interval int_min int 0
        let newDay = (dia + getDayByHours hours)
        let newStart = getStartByHours hours
        insertSchedule (Schedule {month = mes, day = newDay, start = newStart, duration = duracao}) bst

do_insercao_compromisso_minimo bst calendar = do
    (mes_n, dia_n, duracao, prazo) <- read_insercao_compromisso_intervalo
    return (BST (schedule_intervalo_minimo mes_n dia_n prazo duracao bst calendar))

schedule_intervalo_maximo mes dia prazo duracao bst calendar = do
    let int = interval mes dia prazo calendar bst
    let int_max = (intervalo_maximo int duracao)
    if  (int_max == (-1, 0)) then
        bst
    else do
        let hours = locate_interval int_max int 0
        let newDay = (dia + getDayByHours hours)
        let newStart = getStartByHours hours
        insertSchedule (Schedule {month = mes, day = newDay, start = newStart, duration = duracao}) bst

do_insercao_compromisso_maximo bst calendar = do
    (mes_n, dia_n, duracao, prazo) <- read_insercao_compromisso_intervalo
    return (BST (schedule_intervalo_maximo mes_n dia_n prazo duracao bst calendar))