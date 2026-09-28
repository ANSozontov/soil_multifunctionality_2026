library(sf)
library(wdpar)
library(dplyr)
library(purrr)
library(parallel)
library(rmapshaper)
# Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe")
Sys.setenv(CHROMOTE_CHROME = "/usr/bin/brave")

time1 <- Sys.time()
protected <- wdpa_fetch(
    "global", 
    download_dir = "vectors/14_WDPA.global/",
    wait = TRUE,
    force_download = FALSE,
    check_version = FALSE
) 
Sys.time() - time1

protected <- protected %>%
    mutate(tp = st_geometry_type(geometry)) %>% 
    filter(
        DESIG_TYPE %in% c("National", "International"), 
        tp == "MULTIPOLYGON", 
        STATUS != "Proposed",
        REALM != "Marine") %>% 
    select(
        -SITE_ID, -SITE_PID,
        -DESIG_ENG, -DESIG_TYPE, -SITE_TYPE, -tp, -REALM,  #-RESTRICT, 
        -GOV_TYPE, -VERIF, -INLND_WTRS, -OWN_TYPE, -MANG_AUTH, -MANG_PLAN, 
        -CONS_OBJ, -SUPP_INFO, -METADATAID, -PRNT_ISO3, -ISO3, -GOVSUBTYPE, 
        -OWNSUBTYPE, -OECM_ASMT, -NAME, -DESIG, -INT_CRIT, -NO_TAKE, 
        -NO_TK_AREA, -STATUS, -STATUS_YR, -REP_M_AREA, -REP_AREA) %>% 
    st_cast("POLYGON") %>% 
    mutate(id = 1:nrow(.), .before = 1) %>% 
    suppressWarnings() 
cat("Raw data copied from disk to RAM\n")


prot_bboxes <- protected %>% 
    # slice(1:300) %>% 
    mutate(chunk = rep(1:8, length.out = n())) %>% 
    group_split(chunk) %>% 
    mclapply(
        function(xx){
            xx |> 
                st_geometry() |>
                map_dfr(~.x |> 
                            st_bbox() |> 
                            as.list() |>
                            as_tibble()
                ) |>
                mutate(id = xx$id, .before = 1)
        }, 
        mc.cores = 8
    ) %>% 
    map_dfr(rbind) %>% 
    arrange(id)



if(all(protected$id == prot_bboxes$id)){
    prot_bboxes <- cbind(protected, prot_bboxes[,-1])
    rm(protected)
    # saveRDS(prot_bboxes, "prot_bboxes.rds")
} else {
    cli::cli_abort("something went wrong")
}
# if(all(c(protected$SITE_PID == prot_bboxes$SITE_PID, protected$SITE_ID == prot_bboxes$SITE_ID))){
#     prot_bboxes <- cbind(protected, select(prot_bboxes, -SITE_ID, -SITE_PID))
#     rm(protected)
#     # saveRDS(prot_bboxes, "prot_bboxes.rds")
# } else {
#     cli::cli_abort("something went wrong")
# }
cat("bboxes are prepared\n")

tt <- Sys.time()
res <- prot_bboxes %>% 
    # slice(1:100000) %>%
    mutate(chunk = rep(1:10, length.out = n())) %>% 
    group_split(chunk) %>% 
    mclapply(
        function(xx){
            xx |> 
                rmapshaper::ms_simplify(keep = 0.25, sys = TRUE, snap = TRUE, method = "vis") |> 
                wdpar::st_repair_geometry() 
        }, 
        mc.cores = 5
    )
Sys.time() - tt

1:length(res) %>% 
    # `[`(1:2) %>% 
    mclapply(
        function(i){
            st_write(res[[i]], paste0("vectors/14_WDPA.cached/", i, ".gpkg"), as.character(i), quiet = TRUE) 
        }, 
        mc.cores = 10
    )
cat("geometry is simplified now \n")