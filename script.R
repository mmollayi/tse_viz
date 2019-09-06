tse_syms <- c(FOLD = "46348559193224090", KCHI = "16405556680571453")


fold <- get_symbols(tse_syms["FOLD"])
fold <- mutate(fold, pers_date = conv_date(date))
