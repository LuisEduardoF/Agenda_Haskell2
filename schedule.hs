module Schedule where 
import System.IO
import System.Directory
import Data.Char(digitToInt)

data Schedule = Schedule { month :: Int, day :: Int, start :: Int, duration :: Int } deriving (Eq, Show)

instance Ord Schedule where
    (<) s1 s2 = (end s1) <= (start s2) -- s1 antes de s2
    (>) s1 s2 = (start s1) >= (end s2)

data BSTSchedule = Empty | Node Schedule (BSTSchedule) (BSTSchedule) deriving (Show)

insertSchedule :: Schedule -> BSTSchedule -> BSTSchedule
insertSchedule schedule Empty = Node schedule Empty Empty
insertSchedule schedule (Node node left right)
    | schedule < node = Node node (insertSchedule schedule left) right
    | schedule > node = Node node left (insertSchedule schedule right)
    | otherwise = (Node node left right)

searchSchedule :: Schedule -> BSTSchedule -> Bool
searchSchedule _ Empty = False
searchSchedule schedule (Node node left right)
    | schedule == node = True
    | schedule < node = searchSchedule schedule left
    | otherwise = searchSchedule schedule right

deleteSchedule :: Schedule -> BSTSchedule -> BSTSchedule
deleteSchedule _ Empty = Empty
deleteSchedule schedule (Node node left right)
    | schedule == node = deleteRoot (Node node left right)
    | schedule < node = Node node (deleteSchedule schedule left) right
    | otherwise = Node node left (deleteSchedule schedule right)

deleteRoot :: BSTSchedule -> BSTSchedule
deleteRoot (Node _ Empty right) = right
deleteRoot (Node _ left Empty) = left
deleteRoot (Node _ left right) = Node minRight left (deleteSchedule minRight right)
    where minRight = findMin right

findMin :: BSTSchedule -> Schedule
findMin (Node schedule Empty _) = schedule
findMin (Node _ left _) = findMin left


end :: Schedule -> Int
end schedule = (start schedule) + (duration schedule)

read_cmp_presencial (x:y:xs) dia mes bst = do
    let horario_ini = digitToInt (head x)
    let duration = digitToInt (last xs)

    return (horario_ini, duration)

read_cmp_video (x:y:xs) dia mes bst = do
    let horario_ini = digitToInt (head x)
    let duration = digitToInt (last xs)

    return (horario_ini, duration)

read_cmp (x:xs) dia mes bst = do
    if (x == "presencial") then do
        let (horario_ini, duration) = read_cmp_presencial xs dia mes bst
        let bst2 = insertSchedule (Schedule { month = mes, day = dia, start = horario_ini, duration = duration })
        return (xs, bst2)
    else if (x == "videoconferencia") then do
        let (horario_ini, duration) = read_cmp_video xs dia mes bst
        let bst2 = insertSchedule (Schedule { month = mes, day = dia, start = horario_ini, duration = duration })
        return (xs, bst2)
    else do
        return (xs, bst)

read_day_agenda :: [Char] -> Int -> BSTSchedule -> ([Char], BSTSchedule)
read_day_agenda (x:xs) mes bst = do
    if x == "" then
        return (xs, bst)
    else do
        let day = read x :: Int
        (ns, bst2) <- read_cmp xs day mes bst
        if (and (check_valid_compromissos compromissos_input)) then do
            let compromissos = add_cont_compromissos compromissos_input
            read_day_agenda xs bst2
        else do
            return ([], bst2)

read_mes_agenda :: [String] -> BSTSchedule -> BSTSchedule
read_mes_agenda [] bst = bst
read_mes_agenda (x:xs) bst = do
    if xs == [] then
        bst
    else do
        let mes = read x :: Int
        let (ns, bst2) = read_day_agenda xs mes bst
        if (ns == []) then do
            bst2
        else  
            read_mes_agenda ns bst2

read_agenda = do
    fileExist <- doesFileExist "agenda.txt"
    if fileExist then do
        schedule_input <- readFile "agenda.txt"
        let schedule_text = lines schedule_input

        agenda <- read_mes_agenda schedule_text Empty
        
        return agenda
    else 
        return Empty