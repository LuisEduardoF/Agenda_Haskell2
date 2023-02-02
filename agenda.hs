module Agenda where

import Util
import Calendar

data Schedule = ScheduleV { month :: Int, day :: Int, start :: Int, duration :: Int, local :: String, link :: String} | ScheduleP { month :: Int, day :: Int, start :: Int, duration :: Int, local :: String} | Schedule { month :: Int, day :: Int, start :: Int, duration :: Int} deriving (Show)

instance Ord Schedule where
    (<) d1 d2 =
        case (<) (month d1) (month d2) of
            False -> case (==) (month d1) (month d2) of
                        False -> False
                        True -> case (<) (day d1) (day d2) of
                                False -> case (==) (day d1) (day d2) of
                                            False -> False
                                            True -> case (<=) (end d1) (start d2) of
                                                x -> x
                                True -> True
            True -> True

    (<=) d1 d2 =
        case (<) (month d1) (month d2) of
            False -> case (==) (month d1) (month d2) of
                        False -> False
                        True -> case (<) (day d1) (day d2) of
                                False -> case (==) (day d1) (day d2) of
                                            False -> False
                                            True -> case (<) (end d1) (start d2) of
                                                x -> x
                                True -> True
            True -> True
    
    (>) d1 d2 =
        case (>) (month d1) (month d2) of
            False -> case (==) (month d1) (month d2) of
                        False -> False
                        True -> case (>) (day d1) (day d2) of
                                False -> case (==) (day d1) (day d2) of
                                            False -> False
                                            True -> case (>=) (start d1) (end d2) of
                                                x -> x
                                True -> True
            True -> True
    
    (>=) d1 d2 =
        case (>=) (month d1) (month d2) of
            False -> case (==) (month d1) (month d2) of
                        False -> False
                        True -> case (>=) (day d1) (day d2) of
                                False -> case (==) (day d1) (day d2) of
                                            False -> False
                                            True -> case (>=) (start d1) (end d2) of
                                                x -> x
                                True -> True
            True -> True

instance Eq Schedule where
    (==) d1 d2 = case (==) (month d1) (month d2) of
            False -> False
            True -> case (==) (day d1) (day d2) of
                False -> False
                True -> case (==) (start d1) (start d2) of
                    False -> False
                    True -> True
    
end :: Schedule -> Int
end schedule = (start schedule) + (duration schedule)

read_cmp_v_agenda (x:y:z:xs) mes dia type_cmp agenda = do
    let compromisso = wordsWhen (==',') x
    let horario_ini = read (compromisso!!0) :: Int
    let duration = read (compromisso!!1) :: Int
    let local = y
    let link = z

    return (xs, ScheduleV { month = mes, day = dia, start = horario_ini, duration = duration, local = local, link = link})

read_cmp_agenda (x:xs) mes dia agenda = do
    let compromisso = wordsWhen (==',') x
    let horario_ini = read (compromisso!!0) :: Int
    let duration = read (compromisso!!1) :: Int

    return (xs, Schedule { month = mes, day = dia, start = horario_ini, duration = duration})

read_cmp_p_agenda (x:y:xs) mes dia type_cmp agenda = do
    let compromisso = wordsWhen (==',') x
    let horario_ini = read (compromisso!!0) :: Int
    let duration = read (compromisso!!1) :: Int
    let local = y

    return (xs, ScheduleP { month = mes, day = dia, start = horario_ini, duration = duration, local = local})

read_cmp_type_agenda [] mes dia agenda = return ([], agenda)
read_cmp_type_agenda (x:xs) mes dia agenda = do
    if(x /= "") then do
        let type_cmp = x
        if(type_cmp == "presencial") then do
            (ns, cmp) <- read_cmp_p_agenda xs mes dia type_cmp agenda
            read_cmp_type_agenda ns mes dia (agenda ++ [cmp])
        else if(type_cmp == "videoconferencia") then do
            (ns, cmp) <- read_cmp_v_agenda xs mes dia type_cmp agenda
            read_cmp_type_agenda ns mes dia (agenda ++ [cmp])
        else do
            if(charFound ',' x) then do
                (ns, cmp) <- read_cmp_agenda (x:xs) mes dia agenda
                read_cmp_type_agenda ns mes dia (agenda ++ [cmp])
            else
                return (x:xs, agenda)
    else do
        return (x:xs, agenda)

read_day_agenda [] mes agenda = return ([], agenda)
read_day_agenda (x:xs) mes agenda = do
    if(x /= "") then do
        let day = read x :: Int
        (ns, agenda_new) <- read_cmp_type_agenda xs mes day agenda
        read_day_agenda ns mes agenda_new
    else do
        return (xs, agenda)

read_mes_agenda [] agenda = return agenda
read_mes_agenda (x:xs) agenda = do
    let mes = read x :: Int
    (ns, agenda_new) <- read_day_agenda xs mes agenda
    read_mes_agenda ns agenda_new

read_agenda :: IO [Schedule]
read_agenda = do
    agenda_input <- readFile "agenda.txt"
    let agenda_text = lines agenda_input
    let agenda = []

    read_mes_agenda agenda_text agenda