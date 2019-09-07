url <- paste0("http://www.tsetmc.com/tsev2/chart/data/Index.aspx?i=", "32097828799138957", "&t=value")
index_raw <- xml2::read_html(url)
index_raw %>% 
    rvest::html_text() %>% 
    str_split(";") %>%
    .[[1]] %>% 
    str_split(",") %>% 
    do.call(what = rbind) %>%
    as_tibble(.name_repair = "unique") %>% 
    set_names("date", "value") %>% 
    mutate(value = as.numeric(value))

tse_syms <- c(FOLD = "46348559193224090", KCHI = "16405556680571453")


FOLD <- get_symbols(tse_syms["FOLD"])
KCHI <- get_symbols(tse_syms["KCHI"])

fold <- FOLD %>% 
    mutate(pers_date = conv_date(date)) %>% 
    filter(pers_date %:>:% "1398-01-01") %>% 
    mutate(return = calc_return(adjusted)) 
    


kchi <- KCHI %>% 
    mutate(pers_date = conv_date(date)) %>% 
    filter(pers_date %:>:% "1398-01-01")
