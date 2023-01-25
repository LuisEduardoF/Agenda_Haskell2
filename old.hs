import System.IO
import System.Directory
import System.Exit

data Answer a = IO a | Bool 

getInt :: String -> Int
getInt str = read str

getListNum str = map getInt (words str)

-- Auxiliares
inicio [] = []
inicio [x] = []
inicio (x:xs) = x : inicio xs

menu = do
    putStrLn ("0 – Sair")
    putStrLn ("1 - Recuperar agenda")
    putStrLn ("2 – Verificar disponibilidade de horário")
    putStrLn ("3 – Inserir compromisso no horário")
    putStrLn ("4 – Inserir compromisso mais breve")
    putStrLn ("5 – Inserir compromisso no intervalo mínimo")
    putStrLn ("6 – Inserir compromisso no intervalo máximo")
    putStrLn ("7 – Cancelar compromisso")
    putStrLn ("8 – Reagendar compromisso")
    putStrLn ("9 – Gravar agenda")
    
    option_text <- getLine
    let option = getInt option_text

    return option

percorre [] = ""
percorre (x:xs) = x ++ "\n" ++ percorre xs

qntd_dias_meses calendario mes_n  | getBissexto calendario = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]!!(mes_n-1)
                                  | otherwise = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]!!(mes_n-1)
-- Lê agenda
add_cont_compromissos [] = []
add_cont_compromissos ((a,b):xs) | is_entre_turnos (a, a, b) = (a, getHoraInicial (a, a, b), 12 - getHoraInicial (a, a, b)) : (a, 14, getDuracao (a, a, b) - (12 - getHoraInicial (a, a, b))) : add_cont_compromissos xs
                                 | otherwise = (a, getHoraInicial (a, a, b), getDuracao (a, a, b)) : add_cont_compromissos xs
check_valid_compromissos compromissos = [(is_horario_comercial (a, a, b)) && (is_valid_compromisso (a, a, b)) | (a,b) <- compromissos] 
read_day_agenda [] days = return (days, [])
read_day_agenda (x:y:xs) days =
    if x == "" then
        return (days, y:xs)
    else do
        let day = read x :: Int
        let compromissos_input = read y :: [(Int, Int)]
        if (and (check_valid_compromissos compromissos_input)) then do
            let compromissos = add_cont_compromissos compromissos_input
            read_day_agenda xs (days ++ [(day, compromissos)])
        else do
            return ([], [])

read_mes_agenda [] meses = return meses
read_mes_agenda (x:xs) meses =
    if xs == [] then
        return meses
    else do
        let mes = read x :: Int
        (days, ns) <- read_day_agenda xs []
        if (days == [] && ns == []) then do
            return []
        else  
            read_mes_agenda ns (meses ++ [(mes, days)]) 

read_agenda = do
    fileExist <- doesFileExist "agenda.txt"
    if fileExist then do
        agenda_input <- readFile "agenda.txt"
        let agenda_text = lines agenda_input
        
        agenda <- read_mes_agenda agenda_text []
        if (agenda == []) then do
            putStrLn ("Leitura da agenda inválida:\n" ++ (percorre agenda_text))
            return agenda
        else do
            putStrLn ("Leitura da agenda com sucesso:\n" ++ (percorre agenda_text))
            return agenda
    else 
        return []
-- Gravar Agenda
format_file [] = ""
format_file [x] = percorre x
format_file (x:xs) = percorre x ++ "\n" ++ format_file xs
write_dias_agenda [] = []
write_dias_agenda (x:xs) = show (getDia_n x) : show(getCompromissos_print x) : write_dias_agenda xs
write_meses_agenda agenda = return [show (getMes_n x) : write_dias_agenda (getDias x)| x <- agenda]
write_agenda agenda = do
    out <- write_meses_agenda agenda
    -- DEBUG: putStrLn (show out)
    writeFile "agenda.txt" (format_file out)
    return agenda

-- Compromissos
getHoraOrigem (a, _, _) = a
getHoraInicial (_, a, _) = a
getDuracao (_, _, a) = a
getHoraFinal (_, a, b) = a+b
getTurnoManha = [8..12]
getTurnoTarde = [14..18]
getHorarioComercial = getTurnoManha ++ getTurnoTarde
cmpCompromissos c1 c2 | (getHoraFinal c1) <= (getHoraInicial c2) = -1 -- c1 antes de c2
                      | (getHoraInicial c1) >= (getHoraFinal c2) = 1 -- c1 depois de c2
                      | otherwise = 0 -- c1 durante c2
is_horario_comercial comp = (getHoraInicial comp) `elem` getHorarioComercial
is_entre_turnos comp = ((getHoraInicial comp) `elem` getTurnoManha) && (not ((getHoraFinal comp) `elem` getTurnoManha))
is_valid_compromisso comp = (getHoraFinal comp) <= 18

-- Dia
getDia_n (a, _) = a
getCompromissos (_, a) = a
getCompromissos_print dia = [(b, c) | (a, b, c) <- getCompromissos dia]
put_compromissos [] compromisso = [compromisso]
put_compromissos (x:xs) compromisso = do
    if (cmpCompromissos x compromisso) == 1 then do
        compromisso : x : xs
    else do
        x : put_compromissos xs compromisso
checkCompromissos [] comp = True
checkCompromissos (x:xs) comp | cmpCompromissos x comp == 0 = False
                              | cmpCompromissos x comp == -1 = checkCompromissos xs comp
                              | otherwise = True
 
-- Mes
checkDia [] dia_n = (dia_n, [])
checkDia (x:xs) dia_n | (getDia_n x) == dia_n = x
                      | otherwise = checkDia xs dia_n
getMes ns mes = ns!!(mes-1)
getMes_n (a, _) = a
getDias  (_, a) = a
put_dias [] dia = [dia]
put_dias (x:xs) dia | getDia_n dia < getDia_n x = dia : x : xs
                    | getDia_n dia == getDia_n x = dia : xs
                    | otherwise = x : (put_dias xs dia)
remove_dia mes dia = (getMes_n mes, [x | x <- getDias mes, (getDia_n x) /= (getDia_n dia)])
changeDia mes dia = (getMes_n mes, put_dias (getDias mes) dia)
check_availabel calendario mes_n _ [] = []
check_availabel calendario mes_n [] (y:ys) = (y, [0, 0, 0, 0, 0, 0, 0, 0]) : check_availabel calendario mes_n [] ys
check_availabel calendario mes_n (x:xs) (y:ys) | (getDia_n x) > y = if (checkDiaUtil mes_n y calendario) then (y, [0, 0, 0, 0, 0, 0, 0, 0]) : check_availabel calendario mes_n (x:xs) ys else (y, [1, 1, 1, 1, 1, 1, 1, 1]) : check_availabel calendario mes_n (x:xs) ys
                                               | (getDia_n x) == y = (y, [if (checkCompromissos (getCompromissos x) (k,k,1)) then 0 else 1 | k <- getHorarioComercial, (k /= 12) && (k /= 18)]) : check_availabel calendario mes_n xs ys
                                               | otherwise = check_availabel calendario mes_n xs (y:ys)

-- Agenda
checkMes [] mes_n = (mes_n, [])
checkMes (x:xs) mes_n | (getMes_n x) == mes_n = x
                      | otherwise = checkMes xs mes_n

inicialize_agenda :: [(Int, [a])]
inicialize_agenda = [(x, []) | x <- [1..12]]
changeMes [] mes = [mes]
changeMes (x:xs) mes | (getMes_n x) == (getMes_n mes) = mes : xs
                     | (getMes_n x) > (getMes_n mes) = mes : x : xs
                     | otherwise = x : changeMes xs mes
removeMes agenda mes = [x | x <- agenda, (getMes_n x) /= (getMes_n mes)]

-- Calendário
getBissexto (a, _) = a
getCalendario (_, a) = a
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

-- Verifica disponibilidade
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
is_free agenda calendario mes_n dia_n horario_ini duracao = do
    let mes = checkMes agenda mes_n
    let dia = checkDia (getDias mes) dia_n
    let compromisso = (horario_ini, horario_ini, duracao)
    if ((checkDiaUtil mes_n dia_n calendario) && (is_horario_comercial compromisso)) then do
        let compromissos = getCompromissos dia
        checkCompromissos compromissos compromisso
    else
        False
do_is_free agenda calendario = do
    (mes_n, dia_n, horario_ini, duracao) <- read_is_free
    
    putStrLn (show (is_free agenda calendario mes_n dia_n horario_ini duracao))

    return agenda
-- Insercao de compromisso
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
add_compromisso agenda mes_n dia_n horario_ini duracao = do
    let oldMes = checkMes agenda mes_n
    let oldDay = checkDia (getDias oldMes) dia_n
    let compromisso = (horario_ini, horario_ini, duracao)
    if (is_entre_turnos compromisso) then do
        let compromisso1 = (horario_ini, horario_ini, 12 - horario_ini)
        let compromisso2 = (horario_ini, 14, duracao - (12 - horario_ini))
        if ((checkCompromissos (getCompromissos oldDay) compromisso1) && (checkCompromissos (getCompromissos oldDay) compromisso2)) then do
            let newDay1 = (dia_n, put_compromissos (getCompromissos oldDay) compromisso1)
            let newMes1 = changeDia oldMes newDay1
            let newAgenda1 = changeMes agenda newMes1

            let newDay2 = (dia_n, put_compromissos (getCompromissos newDay1) compromisso2)
            let newMes2 = changeDia newMes1 newDay2
            let newAgenda2 = changeMes newAgenda1 newMes2
            newAgenda2
        else do
            agenda
    else do
        let newDay = (dia_n, put_compromissos (getCompromissos oldDay) compromisso)
        let newMes = changeDia oldMes newDay
        let newAgenda = changeMes agenda newMes
        newAgenda
insercao_compromisso agenda calendario mes_n dia_n horario_ini duracao = do 
    if ((is_free agenda calendario mes_n dia_n horario_ini duracao) && (is_valid_compromisso (horario_ini, horario_ini, duracao))) then
        add_compromisso agenda mes_n dia_n horario_ini duracao
    else
        agenda
do_insercao_compromisso agenda calendario = do
    (mes_n, dia_n, horario_ini, duracao) <- read_insercao_compromisso

    return (insercao_compromisso agenda calendario mes_n dia_n horario_ini duracao)

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
cancelamento_compromisso agenda mes_n dia_n horario_ini = do 
    let mes = checkMes agenda mes_n
    let dia = checkDia (getDias mes) dia_n
    let compromissos = getCompromissos dia

    let newCompromissos = [x | x <- compromissos, (getHoraOrigem x) /= horario_ini]
    let newDay = (getDia_n dia, newCompromissos)
    if (newCompromissos == []) then do
        let newMes = remove_dia mes newDay
        if (getDias newMes) == [] then do
            let newAgenda = removeMes agenda newMes
            newAgenda
        else do
            let newAgenda = changeMes agenda newMes
            newAgenda
    else do
        let newMes = changeDia mes newDay
        let newAgenda = changeMes agenda newMes

        newAgenda
do_cancelamento_compromisso agenda calendario = do
    (mes_n, dia_n, horario_ini) <- read_cancelamento_compromisso

    return (cancelamento_compromisso agenda mes_n dia_n horario_ini)

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
reagendamento_compromisso agenda calendario mes_n dia_n horario_ini duracao newMes_n newDia_n newHorario_ini newDuracao = do
    let mes = checkMes agenda mes_n
    let dia = checkDia (getDias mes) dia_n
    let compromissos = getCompromissos dia

    let newAgenda = cancelamento_compromisso agenda mes_n dia_n horario_ini

    if (agenda == newAgenda) then 
        insercao_compromisso newAgenda calendario mes_n dia_n horario_ini duracao
    else if (is_free agenda calendario newMes_n newDia_n newHorario_ini newDuracao) then
        insercao_compromisso newAgenda calendario newMes_n newDia_n newHorario_ini newDuracao
    else 
        agenda
do_reagendamento_compromisso agenda calendario = do
    (mes_n, dia_n, horario_ini, duracao, newMes_n, newDia_n, newHorario_ini, newDuracao) <- read_reagendamento_compromisso

    return (reagendamento_compromisso agenda calendario mes_n dia_n horario_ini duracao newMes_n newDia_n newHorario_ini newDuracao)

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
try_insercao_breve agenda calendario mes_n dia_n [] duracao = agenda
try_insercao_breve agenda calendario mes_n dia_n (x:xs) duracao = do
    let newAgenda = insercao_compromisso agenda calendario mes_n dia_n x duracao
    if (newAgenda /= agenda) then
        newAgenda
    else
        try_insercao_breve agenda calendario mes_n dia_n xs duracao
insercao_compromisso_breve agenda calendario mes_n dia_n duracao = do
    let horario_comercial = getHorarioComercial
    try_insercao_breve agenda calendario mes_n dia_n horario_comercial duracao
do_insercao_compromisso_breve agenda calendario = do
    (mes_n, dia_n, duracao) <- read_insercao_compromisso_breve

    return (insercao_compromisso_breve agenda calendario mes_n dia_n duracao)
-- Available
make_available agenda calendario mes_n dia_n prazo = do
    let dias_mes = qntd_dias_meses calendario mes_n
    let mes = checkMes agenda mes_n
    if (dia_n + prazo) > dias_mes then do 
        let range = [dia_n..dias_mes] 
        let availabel = check_availabel calendario mes_n (getDias mes) range
        let availabel_prox = make_available agenda calendario (mes_n+1) 1 (prazo - (dias_mes - dia_n))
        [(mes_n, availabel)] ++ availabel_prox
    else do
        let range = [dia_n..(dia_n + prazo)]
        let availabel = check_availabel calendario mes_n (getDias mes) range
        [(mes_n, availabel)]
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

-- Insercao de compromisso intervalo maximo
max_horario d1 d2 duracao = do
    if(d1 < d2 && d2 >= duracao) then
        d2
    else if(d1 > d2 && d1 >= duracao) then
        d1
    else
        d2
find_max_horarios [] _ duracao_max horario_ini_max duracao_aux horario_aux duracao inSequence = (duracao_max, horario_ini_max, duracao_aux, horario_aux, inSequence)
find_max_horarios (x:xs) (y:ys) duracao_max horario_ini_max duracao_aux horario_aux duracao inSequence | x == 1 = if inSequence then find_max_horarios xs ys (max_horario duracao_aux duracao_max duracao) (if (max_horario duracao_aux duracao_max duracao) == duracao_aux then horario_aux else horario_ini_max) 0 horario_aux duracao False else find_max_horarios xs ys duracao_max horario_ini_max duracao_aux horario_aux duracao inSequence
                                                                                                       | otherwise = if inSequence then find_max_horarios xs ys duracao_max horario_ini_max (duracao_aux+1) horario_aux duracao inSequence else find_max_horarios xs ys duracao_max horario_ini_max (duracao_aux+1) y duracao True
find_max_dias [] dia_max horario_max duracao_max duracao duracao_aux horario_aux inSequence = (dia_max, horario_max, duracao_max, duracao_aux, horario_aux, inSequence)
find_max_dias (x:xs) dia_max horario_max duracao_max duracao duracao_aux horario_aux inSequence = do
    let dia_n = (\(a,b)->a) x
    let horarios = (\(a,b)->b) x
    let (duracao_max_aux, horario_max_aux, duracao_aux2, horario_aux2, inSequence2) = find_max_horarios horarios ([8..11] ++ [14..17]) duracao_max horario_max duracao_aux horario_aux duracao inSequence
    
    if (duracao_max_aux > duracao_max) then
        find_max_dias xs dia_n horario_max_aux duracao_max_aux duracao duracao_aux2 horario_aux2 inSequence2
    else
        find_max_dias xs dia_max horario_max duracao_max duracao duracao_aux2 horario_aux2 inSequence2

find_max_meses [x] mes_max dia_max horario_max duracao_max duracao duracao_aux horario_aux inSequence = do
    let mes_n = getMes_n x
    let dias_available = getDias x   
    let (dia_max_aux, horario_max_aux, duracao_max_aux, duracao_aux2, horario_aux2, inSequence2) = find_max_dias dias_available dia_max horario_max duracao_max duracao duracao_aux horario_aux inSequence

    if (duracao_aux2 > 0) then do
        if ((duracao_max_aux + duracao_aux) > duracao_max) then
            (mes_n, dia_max_aux, horario_max_aux, duracao_max_aux)
        else
            (mes_max, dia_max, horario_max, duracao_max)
    else do
        if ((duracao_max_aux + duracao_aux) > duracao_max) then
            (mes_n, dia_max_aux, horario_max_aux, duracao_max_aux)
        else
            (mes_max, dia_max, horario_max, duracao_max)
find_max_meses (x:xs) mes_max dia_max horario_max duracao_max duracao duracao_aux horario_aux inSequence = do
    let mes_n = getMes_n x
    let dias_available = getDias x   
    let (dia_max_aux, horario_max_aux, duracao_max_aux, duracao_aux2, horario_aux2, inSequence2) = find_max_dias dias_available dia_max horario_max duracao_max duracao duracao_aux horario_aux inSequence

    if (duracao_max_aux > duracao_max) then
        find_max_meses xs mes_n dia_max_aux horario_max_aux duracao_max_aux duracao duracao_aux2 horario_aux2 inSequence2
    else
        find_max_meses xs mes_max dia_max horario_max duracao_max duracao duracao_aux2 horario_aux2 inSequence2

check_max agenda calendario mes_n dia_n duracao prazo = do
    let availabel = make_available agenda calendario mes_n dia_n prazo
    find_max_meses availabel mes_n dia_n (-1) (minBound :: Int) duracao 0 8 False

insercao_compromisso_maximo agenda calendario mes_n dia_n duracao prazo = do
    let (mes_max, dia_max, horario_max, duracao_max) = check_max agenda calendario mes_n dia_n duracao prazo
    if (horario_max == (-1)) then 
        agenda
    else
        insercao_compromisso agenda calendario mes_max (dia_max - ((duracao_max-1) `div` 8)) horario_max duracao

do_insercao_compromisso_maximo agenda calendario = do
    (mes_n, dia_n, duracao, prazo) <- read_insercao_compromisso_intervalo
    return (insercao_compromisso_maximo agenda calendario mes_n dia_n duracao prazo)

-- Insercao de compromisso intervalo minimo
min_horario d1 d2 duracao = do
    if(d1 > d2 && d2 >= duracao) then
        d2
    else if(d1 < d2 && d1 >= duracao) then
        d1
    else
        d2
find_min_horarios [] _ duracao_min horario_ini_min duracao_aux horario_aux duracao inSequence = (duracao_min, horario_ini_min, duracao_aux, horario_aux, inSequence)
find_min_horarios (x:xs) (y:ys) duracao_min horario_ini_min duracao_aux horario_aux duracao inSequence | x == 1 = if inSequence then find_min_horarios xs ys (min_horario duracao_aux duracao_min duracao) (if (min_horario duracao_aux duracao_min duracao) == duracao_aux then horario_aux else horario_ini_min) 0 horario_aux duracao False else find_min_horarios xs ys duracao_min horario_ini_min duracao_aux horario_aux duracao inSequence
                                                                                                       | otherwise = if inSequence then find_min_horarios xs ys duracao_min horario_ini_min (duracao_aux+1) horario_aux duracao inSequence else find_min_horarios xs ys duracao_min horario_ini_min (duracao_aux+1) y duracao True
find_min_dias [] dia_min horario_min duracao_min duracao duracao_aux horario_aux inSequence = (dia_min, horario_min, duracao_min, duracao_aux, horario_aux, inSequence)
find_min_dias (x:xs) dia_min horario_min duracao_min duracao duracao_aux horario_aux inSequence = do
    let dia_n = (\(a,b)->a) x
    let horarios = (\(a,b)->b) x
    let (duracao_min_aux, horario_min_aux, duracao_aux2, horario_aux2, inSequence2) = find_min_horarios horarios ([8..11] ++ [14..17]) duracao_min horario_min duracao_aux horario_aux duracao inSequence
    
    if (duracao_min_aux < duracao_min) then
        find_min_dias xs dia_n horario_min_aux duracao_min_aux duracao duracao_aux2 horario_aux2 inSequence2
    else
        find_min_dias xs dia_min horario_min duracao_min duracao duracao_aux2 horario_aux2 inSequence2

find_min_meses [x] mes_min dia_min horario_min duracao_min duracao duracao_aux horario_aux inSequence = do
    let mes_n = getMes_n x
    let dias_available = getDias x   
    let (dia_min_aux, horario_min_aux, duracao_min_aux, duracao_aux2, horario_aux2, inSequence2) = find_min_dias dias_available dia_min horario_min duracao_min duracao duracao_aux horario_aux inSequence

    if (duracao_min_aux < duracao_min) then
        (mes_n, dia_min_aux, horario_min_aux, duracao_min_aux)
    else
        (mes_min, dia_min, horario_min, duracao_min)
find_min_meses (x:xs) mes_min dia_min horario_min duracao_min duracao duracao_aux horario_aux inSequence = do
    let mes_n = getMes_n x
    let dias_available = getDias x   
    let (dia_min_aux, horario_min_aux, duracao_min_aux, duracao_aux2, horario_aux2, inSequence2) = find_min_dias dias_available dia_min horario_min duracao_min duracao duracao_aux horario_aux inSequence

    if (duracao_min_aux < duracao_min) then
        find_min_meses xs mes_n dia_min_aux horario_min_aux duracao_min_aux duracao duracao_aux2 horario_aux2 inSequence2
    else
        find_min_meses xs mes_min dia_min horario_min duracao_min duracao duracao_aux2 horario_aux2 inSequence2

check_min agenda calendario mes_n dia_n duracao prazo = do
    let availabel = make_available agenda calendario mes_n dia_n prazo
    find_min_meses availabel mes_n dia_n (-1) (maxBound :: Int) duracao 0 8 False

insercao_compromisso_minimo agenda calendario mes_n dia_n duracao prazo = do
    let (mes_min, dia_min, horario_min, duracao_min) = check_min agenda calendario mes_n dia_n duracao prazo
    if (horario_min == (-1)) then 
        agenda
    else
        insercao_compromisso agenda calendario mes_min (dia_min - ((duracao_min-1) `div` 8)) horario_min duracao

do_insercao_compromisso_minimo agenda calendario = do
    (mes_n, dia_n, duracao, prazo) <- read_insercao_compromisso_intervalo
    return (insercao_compromisso_minimo agenda calendario mes_n dia_n duracao prazo)

-- Sai do programa
exit agenda = do
    exitSuccess
    return agenda

-- Act by the option
option_switch option agenda calendario | option == 0 = exit agenda
                                       | option == 1 = read_agenda
                                       | option == 2 = do_is_free agenda calendario
                                       | option == 3 = do_insercao_compromisso agenda calendario
                                       | option == 4 = do_insercao_compromisso_breve agenda calendario
                                       | option == 5 = do_insercao_compromisso_minimo agenda calendario
                                       | option == 6 = do_insercao_compromisso_maximo agenda calendario
                                       | option == 7 = do_cancelamento_compromisso agenda calendario
                                       | option == 8 = do_reagendamento_compromisso agenda calendario
                                       | option == 9 = write_agenda agenda
                                       | otherwise = return agenda
-- Main Loop
main_loop agenda calendario = do
    option <- menu
    
    newAgenda <- option_switch option agenda calendario

    main_loop newAgenda calendario
-- Main
main = do
    agenda <- read_agenda
    calendario <- read_calendario
    main_loop agenda calendario
    return 0
