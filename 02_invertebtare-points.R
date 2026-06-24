cat("\nTask 2 started...\n(usually takes 5-10 mins on 16 cores)") 
cat("\nSubtask 2.1 (points load) started...\n")
my_time0 <- Sys.time()
my_time <- Sys.time()
# subtask 1 ---------------------------------------------------------------
suppressPackageStartupMessages({
    library(tidyverse)
    # library(parallel)
})

# nematodes
if(!dir.exists("2020_global_nematode_dataset")){
    system("git clone https://github.com/hooge104/2020_global_nematode_dataset")
    cli::cli_alert_success("Nematodes dataset has been downloaded")
} else {
    cli::cli_alert_info("Nematodes dataset is aready downloaded")
}

nematodes <- readr::read_delim(
        "2020_global_nematode_dataset/data/nematode_full_dataset_wBiome.csv",
        show_col_types = F) %>% 
    select(
        lat_nm = Pixel_Lat, lon_nm = Pixel_Long, 
        Bacterivores:Total_Number) %>% 
    group_by(lat_nm, lon_nm) %>% 
    summarise_all(mean) %>% 
    ungroup %>% 
    filter(!is.na(lat_nm), !is.na(lon_nm)) %>% 
    mutate(NMID = 1:nrow(.), .before = 1) 

# earthworms 
path <- "Phillips_2021/1880_26_Dataset/Phillips_sWorm_2021-02-18/1880 Phillips"
EW_sites <- readr::read_delim(
        paste0(path, "/SiteData_sWorm_2021-02-18.csv"), 
        show_col_types = F) %>% 
    group_by(file, Study_Name, Site_Name) %>% 
    dplyr::summarise(
        lat_ew = mean(Latitude_decimal_degrees, na.rm = TRUE), 
        lon_ew = mean(Longitude_decimal_degrees, na.rm = TRUE),
        .groups = "drop") %>% 
    filter(!is.na(lat_ew), !is.na(lon_ew)) %>% 
    mutate(EWID = 1:nrow(.), .before = 1) 
    
EW_occs <- readr::read_delim(
        paste0(path, "/SppOccData_sWorm_2021-02-18.csv"), 
        show_col_types = F) %>% 
    select(file, Study_Name, Site_Name, Family, 
           spec = SpeciesBinomial, 
           Ecological_group, Abundance, Abundance_Unit) 

cat("\nSubtask 2.1 finished:\n"); Sys.time() - my_time; cat("\n")

# subtask 2 ---------------------------------------------------------------
cat("\nSubtask 2.2 (distances 1) started...\n") 
my_time <- Sys.time()
# check for distance
path0 <- str_subset(dir(pattern = "RData"), "inv.points.sparsed2")
if(length(path0)<1){
    cat(
        "\nEw and Nm points have NOT been matched already", 
        "\nThere is no prepared data, computing is started...\n")
    # source("script_2b_distances_stage1.R")
    library(parallel)
    a <- tidyr::expand_grid(
        EWID = EW_sites$EWID,
        NMID = nematodes$NMID, 
        dis_deg = NA, 
        dis_km = NA)
    
    
    result_deg <- mclapply(
        1:nrow(a), 
        FUN = function(i){
            max(
                abs(
                    c(
                        nematodes[a$NMID[i],]$lat_nm - EW_sites[a$EWID[i],]$lat_ew, 
                        nematodes[a$NMID[i],]$lon_nm - EW_sites[a$EWID[i],]$lon_ew
                    )
                )
            )
        }, 
        # SIMPLIFY = TRUE, 
        mc.cores = 16
    )
    
    result_deg <- purrr::flatten_dbl(result_deg)
    
    a$dis_deg <- result_deg
    
    sparsed1 <- dplyr::filter(a, dis_deg < 0.5)
    
    
    
    path <- Sys.time() |>
        as.character() |>
        stringr::str_split_1("\\.")
    path <- path[1] |>
        stringr::str_replace_all( ":", "-")
    save(list = "sparsed1", file = paste0("inv.points.sparsed1_", path, ".RData"))
    
    cat("\nSubtask 2.2 finished:\n"); Sys.time() - my_time; cat("\n")
    cat("\n", nrow(sparsed1), "points remain\n")
    
# subtask 3 ---------------------------------------------------------------
    cat("\nSubtask 2.3 (distances 2) started...\n") 
    my_time <- Sys.time()
    # source("script_2c_distances_stage2.R")
    
    EW_sites <-  sf::st_as_sf(
        EW_sites,
        coords = c("lon_ew", "lat_ew"), 
        crs = 4326, remove = F)
    nematodes <- sf::st_as_sf(
        nematodes,
        coords = c("lon_nm", "lat_nm"), 
        crs = 4326, 
        remove = F)
    
    # sparsed1 <- dplyr::sample_n(sparsed1, 100)
    
    result_m <- mclapply(
        1:nrow(sparsed1), 
        FUN = function(i){
            as.numeric(sf::st_distance(nematodes[sparsed1$NMID[i],], EW_sites[sparsed1$EWID[i],]))
        }, 
        mc.cores = 16
    )
    sparsed1$dis_km <- purrr::flatten_dbl(result_m)/1000
    sparsed2 <- dplyr::filter(sparsed1, dis_km <= 10)
    
    
    path <- Sys.time() |>
        as.character() |>
        stringr::str_split_1("\\.")
    path <- path[1] |>
        stringr::str_replace_all( ":", "-")
    save(list = "sparsed2", file = paste0("inv.points.sparsed2_", path, ".RData"))
    
    
    # path <- str_subset(dir(pattern = "RData"), "inv.points.sparsed2")
} else {
    cat(
        "\nEw and Nm points have been matched already", 
        "\nThere is no need to compute, prepared data will be used\n")
    load(sort(path0, decreasing = TRUE)[1])
}

EW_NM_points <- EW_sites %>% 
    filter(EWID %in% sparsed2$EWID) %>% 
    left_join(EW_occs, by = c("file", "Study_Name", "Site_Name")) %>%
    filter(!is.na(Abundance), Abundance_Unit == "Individuals per m2") %>% 
    group_by(EWID, file, Study_Name, Site_Name, lat_ew, lon_ew) %>% 
    summarise(
        EW_abu_m2 = sum(Abundance), 
        ew_units = unique(Abundance_Unit), 
        .groups = "drop"
    ) %>% 
    left_join(sparsed2, ., by = "EWID") %>% 
    filter(!is.na(file)) %>% 
    left_join(nematodes, by = "NMID") %>% 
    mutate(
        lat = round((lat_ew + lat_nm)/2, 2),
        lon = round((lon_ew + lon_nm)/2, 2),
        Unidentified = case_when(is.na(Unidentified) ~ 0, TRUE ~ Unidentified),
        file_study_site = paste(file, Study_Name, Site_Name, sep = "__"),
        .keep = "unused", 
        .after = 2
    ) %>% 
    select(-ew_units, -dis_deg, -dis_km, -file_study_site) %>% 
    group_by(lat, lon) %>% 
    summarise_if(is.numeric, ~mean(.x, na.rm = TRUE)) %>% 
    ungroup() 

readr::write_delim(EW_NM_points, paste0("export/EW-NM-points_", Sys.Date(), ".csv"))

path <- Sys.time() |>
    as.character() |>
    stringr::str_split_1("\\.")
path <- path[1] |>
    stringr::str_replace_all( ":", "-")


EW_NM_points <- sf::st_as_sf(
    EW_NM_points, 
    coords = c("lon", "lat"), 
    crs = 4326, 
    remove = F)

save(list = "EW_NM_points", file = paste0("EW-NM-points_", path, ".RData"))
cat("\nSubtask 2.3 finished:\n"); Sys.time() - my_time; cat("\n")
cat("\nTask 2 finished:\n"); Sys.time() - my_time0; cat("\n")