# load --------------------------------------------------------------------
suppressPackageStartupMessages(library(sf))
library(wdpar)
suppressPackageStartupMessages(library(dplyr))
library(purrr)
library(parallel)
library(rmapshaper)
# Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe")
Sys.setenv(CHROMOTE_CHROME = "/usr/bin/brave")

time1 <- time0 <- Sys.time()
protected <- wdpa_fetch(
    "global", 
    download_dir = "vectors/14_WDPA.global/",
    wait = TRUE,
    force_download = FALSE,
    check_version = FALSE
)
cat("Data downloaded\n")
Sys.time() - time1
time1 <- Sys.time()

#### @@@@@
# protected <- sample_frac(protected, 0.2)
####

# processing --------------------------------------------------------------
protected <- protected %>%
    mutate(chunk = rep(1:100, length.out = n())) %>%
    group_split(chunk)
res <- list()
cat("0 % done\n")
for(i in 1:10){
    res[[i]] <- protected[(1:10)*i] %>% 
        mclapply(function(xx){
            yy <- xx |> 
                filter(
                    DESIG_TYPE %in% c("National", "International"), 
                    STATUS != "Proposed",
                    REALM != "Marine") |> 
                mutate(tp = st_geometry_type(geometry)) |>
                filter(tp %in% c("POLYGON", "MULTIPOLYGON")) |> 
                select(
                    -SITE_ID, -SITE_PID, -IUCN_CAT, -chunk,
                    -DESIG_ENG, -DESIG_TYPE, -SITE_TYPE, -tp, -REALM,  #-RESTRICT, 
                    -GOV_TYPE, -VERIF, -INLND_WTRS, -OWN_TYPE, -MANG_AUTH, -MANG_PLAN, 
                    -CONS_OBJ, -SUPP_INFO, -METADATAID, -PRNT_ISO3, -ISO3, -GOVSUBTYPE, 
                    -OWNSUBTYPE, -OECM_ASMT, -NAME, -DESIG, -INT_CRIT, -NO_TAKE, 
                    -NO_TK_AREA, -STATUS, -STATUS_YR, -REP_M_AREA, -REP_AREA) |>
                st_cast("POLYGON") |>
                rmapshaper::ms_simplify(
                    quiet = TRUE,
                    keep = 0.25, 
                    sys = TRUE, 
                    snap = TRUE, 
                    sys_mem = 6, 
                    method = "vis") |> 
                wdpar::st_repair_geometry() |>
                suppressWarnings()
            bb <- do.call(rbind, lapply(st_geometry(yy), st_bbox))
            return(cbind(yy, bb))
        }, mc.cores = 7) 
    cat(i*10, "% done\n")
} 
res <- flatten_dfr(res)
cat("Data prepared\n")
Sys.time() - time1
time1 <- Sys.time()

# store as cache ----------------------------------------------------------
res %>%
    mutate(chunk = rep(1:10, length.out = n())) %>%
    group_split(chunk) %>%
    mclapply(
        function(i){
            st_write(i, paste0("vectors/14_WDPA.cached/", i$chunk[1], ".gpkg"), "d", quiet = TRUE)
        },
        mc.cores = 10
    ) |>
    invisible()
cat("Data cache saved\n")
Sys.time() - time1
time1 <- Sys.time()

cli::cli_alert_success("Task 00 is done!\n")
Sys.time() - time0