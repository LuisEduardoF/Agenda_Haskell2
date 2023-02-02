module Main where 

import OP
import Util
import Calendar
import Agenda
import BST

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
option_switch option bst calendario | option == 0 = exit bst
                                    | option == 1 = do_reload_agenda 
                                    | option == 2 = do_is_free bst calendario
                                    | option == 3 = do_insercao_compromisso bst calendario
                                    | option == 4 = do_insercao_compromisso_breve bst calendario
                                    --   | option == 5 = do_insercao_compromisso_minimo bst calendario
                                    --   | option == 6 = do_insercao_compromisso_maximo bst calendario
                                    | option == 7 = do_cancelamento_compromisso bst calendario
                                    | option == 8 = do_reagendamento_compromisso bst calendario
                                    --   | option == 9 = write_bst bst
                                    | otherwise = return (BST bst)

main_loop (BST bst) calendario = do
    option <- menu
    
    newBst <- option_switch option bst calendario
    if (newBst == (Bool True) || newBst == (Bool False)) then do
        putStrLn (show(newBst))
        main_loop (BST bst) calendario
    else if(newBst == Exit) then do
        return Exit
    else do
        putStrLn (show(newBst))
        main_loop newBst calendario

main = do
    calendario <- read_calendario
    bst <- do_reload_agenda
    main_loop bst calendario
    return 0