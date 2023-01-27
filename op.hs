module OP where
    
import BST
import Util
import Agenda
import Calendar

data Result = Bool Bool | Occupied | Exit | BST BSTSchedule deriving (Eq, Show)

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

    if ((checkDiaUtil mes_n dia_n calendario) && (is_horario_comercial horario_ini)) then do
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
do_insercao_compromisso bst calendario = do
    (mes_n, dia_n, horario_ini, duracao) <- read_insercao_compromisso

    if (not (searchSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst) && (is_valid_compromisso horario_ini duracao)) then do
        let newBst = insertSchedule (Schedule {month = mes_n, day = dia_n, start = horario_ini, duration = duracao}) bst
        return (BST newBst)
    else
        return (BST bst)


