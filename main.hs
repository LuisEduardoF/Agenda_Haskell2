import Calendar
import Schedule

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

option_switch option bst calendario | option == 0 = return bst
                                    --   | option == 1 = read_agenda
                                    --   | option == 2 = do_is_free agenda calendario
                                    --   | option == 3 = do_insercao_compromisso agenda calendario
                                    --   | option == 4 = do_insercao_compromisso_breve agenda calendario
                                    --   | option == 5 = do_insercao_compromisso_minimo agenda calendario
                                    --   | option == 6 = do_insercao_compromisso_maximo agenda calendario
                                    --   | option == 7 = do_cancelamento_compromisso agenda calendario
                                    --   | option == 8 = do_reagendamento_compromisso agenda calendario
                                    --   | option == 9 = write_agenda agenda
                                    --   | otherwise = return agenda

main_loop bst calendario = do
    option <- menu
    
    newBst <- option_switch option bst calendario

    -- main_loop newBst calendario
    return newBst

main = do
    agenda <- read_agenda
    calendario <- read_calendario
    main_loop agenda calendario
    return 0