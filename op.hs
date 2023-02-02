module OP where
    
import BST
import Util
import Agenda
import Calendar

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

    return (mes_n, dia_n, horario_ini, duracao)

insercao_compromisso mes_n dia_n horario_ini duracao calendario bst = do
    if (not (searchSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst) && (is_valid_compromisso horario_ini duracao) && (checkDiaUtil mes_n dia_n calendario)) then do
        let newBst = insertSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst
        return (BST newBst)
    else
        return (BST bst)
do_insercao_compromisso bst calendario = do
    (mes_n, dia_n, horario_ini, duracao) <- read_insercao_compromisso

    insercao_compromisso mes_n dia_n horario_ini duracao calendario bst

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

    return (mes_n, dia_n, horario_ini, duracao, newMes_n, newDia_n, newHorario_ini, newDuracao)

do_reagendamento_compromisso bst calendario = do
    (mes_n, dia_n, horario_ini, duracao, newMes_n, newDia_n, newHorario_ini, newDuracao) <- read_reagendamento_compromisso
    
    let newBst = deleteSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst

    if (bst == newBst) then 
        insercao_compromisso mes_n dia_n horario_ini duracao calendario bst
    else if (not (searchSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst)) then
        insercao_compromisso newMes_n newDia_n newHorario_ini newDuracao calendario bst
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

    return (mes_n, dia_n, duracao)
try_insercao_breve :: BSTSchedule -> (Bool, [[Int]]) -> Int -> Int -> [Int] -> Int -> IO Result
try_insercao_breve bst calendario mes_n dia_n [] duracao = return (BST bst)
try_insercao_breve bst calendario mes_n dia_n (x:xs) duracao = do
    newBST <- insercao_compromisso mes_n dia_n x duracao calendario bst
    if (getBST (newBST) /= bst) then
        return newBST
    else
        try_insercao_breve bst calendario mes_n dia_n xs duracao
insercao_compromisso_breve bst calendario mes_n dia_n duracao = do
    let horario_comercial = getHorarioComercial
    try_insercao_breve bst calendario mes_n dia_n horario_comercial duracao

do_insercao_compromisso_breve :: BSTSchedule -> (Bool, [[Int]]) -> IO Result
do_insercao_compromisso_breve bst calendario = do
    (mes_n, dia_n, duracao) <- read_insercao_compromisso_breve

    insercao_compromisso_breve bst calendario mes_n dia_n duracao

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

