# initial -----------------------------------------------------------------
# library(future.apply)
suppressPackageStartupMessages({
    library(mice)
    library(progress)
    library(tidyverse)
})

res <- list()
tmp <- list()
time0 <- time1 <- Sys.time()
functions_classification <- readxl::read_excel("FunctionsList.xlsx", sheet = "Лист1")

load(paste0("export/", sort(dir("export", pattern = glob2rx("*data_extracted*RData")), decreasing = TRUE)[1]))
df_raw <- random_points %>% 
    sf::st_drop_geometry() %>% 
# Отбираем ТОЛЬКО  числовые показатели 
# Категории  НЕ превращаем в числа, а временно исключаем
    select(where(is.numeric)) %>%
    select(-any_of(c("id", "ID", "lon", "lat", "Участок", "Plot", "wc"))) %>% 
    `colnames<-`(make.names(colnames(.), unique = TRUE)) %>% 
    mutate_all(~as.numeric(scale(.x))) %>% 
    as_tibble()
# Убираем переменные с более чем 10% NA
tmp$na_ratio <- apply(df_raw, 2, function(x) mean(is.na(x)))
df_standardized <- df_raw[,tmp$na_ratio <= 0.12]
cat("\nКоличество почвенных параметров для расчета до импутации:", ncol(df_standardized), "\n")
Sys.time() - time1

# Обработка пропущенных данных --------------------------------------------
time1 <- Sys.time()
cat('\nОбработка пропущенных данных, ~15 минут\n')
imputed <- futuremice(
        # df_standardized[1:1000,1:2], 
        df_standardized,
        m = 10, 
        maxit = 5, # set 10 for final run
        method = 'pmm', 
        n.core = 10, # 500 MB per thread
        parallelseed = 10
)

df_imputed <- 1:10 %>% 
    map_dfr(
        ~.x %>% 
            complete(imputed, .) %>% 
            mutate(id = 1:14063) %>% 
            as_tibble,
        rbind    
        ) %>% 
    pivot_longer(names_to = "nm", values_to = "vl", -id) %>% 
    pivot_wider(names_from = nm, values_from = vl, values_fn = mean) %>% 
    select(-id)

tmp$miss <- sapply(df_imputed, function(x){sum(is.na(x))})
tmp$miss <- sort(tmp$miss[tmp$miss > 0], decreasing = TRUE)
cat(
    "По-прежнему с пропусками остаются переменные: \n",
    paste0(paste0(tmp$miss, " пропусков: ", names(tmp$miss)), collapse = "\n")
)

df_imputed <- df_imputed[,sapply(df_imputed, function(x){sum(is.na(x))}) == 0]

# Проверка на бесконечные значения
if(any(sapply(df_imputed, function(x) any(is.infinite(x))))){
    cli::cli_abort(paste0(
        "⚠ Обнаружены бесконечные значения в переменных: ", 
        paste0(colnames(df_imputed[,sapply(df_imputed, function(x) any(is.infinite(x)))]), collapse = ", ")
    ))
} else {
    cli::cli_alert_success("Бесконечные значения не обнаружены ✓\n")
}
cat("\nКоличество почвенных параметров для расчета после импутации:", ncol(df_standardized), "\n")
Sys.time() - time1

time1 <- Sys.time()



# export ------------------------------------------------------------------
save.image("tmp.RData")
Sys.time() - time0


    