get_symbols <- function(symbol = NULL, env = NULL, auto_assign = FALSE, verbose = FALSE,
                        return_class = c("tibble", "xts"), adj_prices = FALSE) {
    
    if (is.null(env)) auto_assign <- FALSE
    return_class <- match.arg(return_class)
    
    inscode <- symbol
    
    url <- httr::modify_url("http://www.tsetmc.com/tsev2/data/Export-txt.aspx",
                            query = list(t = "i", a = 2, b = 0, i = inscode))
    
    clnms <- c("date", "open", "high", "low", "close",
               "value", "volume", "n_trades", "last_day")
    
    response <- httr::GET(url)
    
    if ((response$status_code < 200) || (response$status_code >= 300)) {
        stop(httr::content(response, as="text"), call. = FALSE)
    }
    
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    content <- readr::read_csv(content, skip = 1, col_types = "-Dddddddd-d-", col_names = clnms,
                               locale = readr::locale(date_format = "%Y%m%d"))
    
    content <- content %>%
        mutate(adj_ratio = lag(last_day) / close) %>%
        mutate(adj_ratio = ifelse(is.na(adj_ratio), 1, adj_ratio)) %>%
        mutate(adj_ratio = cumprod(adj_ratio)) %>%
        mutate(adjusted = close * adj_ratio)
    
    if (adj_prices) {
        content <- content %>%
            mutate_at(vars(open:close), funs(. * adj_ratio))
    }
    
    content <- content %>%
        select(date:close, adjusted, value:n_trades)
    
    if (return_class == "xts") content <- as.xts(zoo::read.csv.zoo(content))
    
    # assign(instruments[i], tse_data, env)
    
    return(content)
}

conv_date_e2p <- function(x) {
    ConvCalendar::as.OtherDate(x, calendar = "persian") %>%
        unclass() %>% as_tibble() %>% 
        mutate_at(.vars = vars(day, month), stringr::str_pad, width = 2, side = "left", pad = "0") %>% 
        select(year, month, day) %>% 
        invoke(.f = paste, sep = "-")
}

conv_date_p2e <- function(x, sep = NULL) {
    if (is.null(sep)) {
        date_vec <- str_sub(x, c(1, 5, 7), c(4, 6, 8))
    } else {
        date_vec <- str_split(x, sep) %>% unlist()
    }
    
    date_vec %>% 
        as.integer() %>% 
        matrix(ncol = 3, byrow = TRUE) %>% 
        `colnames<-`(c("year", "month", "day")) %>% 
        apply(2, list) %>% flatten() %>% 
        invoke(.f = ConvCalendar::OtherDate, calendar = "persian") %>% 
        as.Date()
}

pers_date_to_mat <- function(x) {
    strsplit(x, fixed = TRUE, split = "-") %>% 
        unlist() %>% as.integer() %>% 
        matrix(ncol = 3, byrow = TRUE)
}

`%:>:%` <- function(x, y) {
    x <- str_remove_all(x, "-") %>% as.integer()
    y <- str_remove_all(y, "-") %>% as.integer()
    x > y
}

`%:<:%` <- function(x, y) {
    x <- str_remove_all(x, "-") %>% as.integer()
    y <- str_remove_all(y, "-") %>% as.integer()
    x < y
}

`%:==:%` <- function(x, y) {
    x <- str_remove_all(x, "-") %>% as.integer()
    y <- str_remove_all(y, "-") %>% as.integer()
    x == y
}

`%:>=:%` <- function(x, y) {
    x <- str_remove_all(x, "-") %>% as.integer()
    y <- str_remove_all(y, "-") %>% as.integer()
    x >= y
}

`%:<=:%` <- function(x, y) {
    x <- str_remove_all(x, "-") %>% as.integer()
    y <- str_remove_all(y, "-") %>% as.integer()
    x <= y
}

calc_return <- function(x) {
    y <- lead(x, defualt = last(adjusted))
    (x - y) / y
}
