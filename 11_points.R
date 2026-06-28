# initial -----------------------------------------------------------------
cat("\nTask 1 started...\n") 
time0 <- Sys.time()
suppressPackageStartupMessages({
    library(sf)
    library(tidyverse)
})

# 1.1. Random points -----------------------------------------------------------
cat("\nSubtask 1 started...\nRandom points\n") 
time1 <- Sys.time()

nater_terrain <- st_read("vectors/naturalearth/ne_50m_land.shp", quiet = TRUE)

set.seed(1); dots <- tidyr::expand_grid(
        x = seq(-180, 180, by = 1), 
        y = seq(-85, 85, by = 1)) %>%
    dplyr::mutate(
        x = x + runif(nrow(.), -0.2, +0.2),
        y = y + runif(nrow(.), -0.2, +0.2)
        ) %>% 
    # sample_frac(0.01) %>% 
    st_as_sf(coords = c("x", "y"), crs = 4326, remove = F)

suppressWarnings({check <- st_intersects(dots, nater_terrain, sparse = FALSE)})

random_points <- dots[apply(check, 1, sum) > 0,]
cat("\nSubtask 1 finished:\n"); Sys.time() - time1

# 2.2. EW_NM points -------------------------------------------------------
cat("\nSubtask 2 (points load) started...\n(usually takes 5-10 mins on 16 cores)\n")
time1 <- Sys.time()
# https://idata.idiv.de/ddm/Data/ShowData/1880?version=26 EW

# earthworms 
path <- "input_data/Phillips_2021/1880_26_Dataset/Phillips_sWorm_2021-02-18/1880 Phillips"
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

# nematodes
if(!dir.exists("input_data/2020_global_nematode_dataset")){
    system("git clone https://github.com/hooge104/2020_global_nematode_dataset")
    cli::cli_alert_success("Nematodes dataset has been downloaded")
} else {
    cli::cli_alert_info("Nematodes dataset is aready downloaded")
}

nematodes <- readr::read_delim(
    "input_data/2020_global_nematode_dataset/data/nematode_full_dataset_wBiome.csv",
    show_col_types = F) %>% 
    select(
        lat_nm = Pixel_Lat, lon_nm = Pixel_Long, 
        Bacterivores:Total_Number) %>% 
    group_by(lat_nm, lon_nm) %>% 
    summarise_all(mean) %>% 
    ungroup %>% 
    filter(!is.na(lat_nm), !is.na(lon_nm)) %>% 
    mutate(NMID = 1:nrow(.), .before = 1) 

# cat("\nSubtask 2.1 finished:\n"); Sys.time() - my_time; cat("\n")

# subtask 2 
# cat("\nSubtask 2.2 (distances 1) started...\n") 
# my_time <- Sys.time()


# check for distance
path0 <- str_subset(dir("input_data", pattern = "RData"), "inv.points.sparsed")
if(length(path0)>0){
    cat(
        "\nEw and Nm points have been matched already", 
        "\nThere is no need to compute, prepared data will be used\n")
    load(sort(path0, decreasing = TRUE)[1])
} else {    
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
    
    
    
    # path <- Sys.time() |>
    #     as.character() |>
    #     stringr::str_split_1("\\.")
    # path <- path[1] |>
    #     stringr::str_replace_all( ":", "-")
    # save(list = "sparsed1", file = paste0("inv.points.sparsed1_", path, ".RData"))
    
    # cat("\nSubtask 2.2 finished:\n"); Sys.time() - my_time; cat("\n")
    # cat("\n", nrow(sparsed1), "points remain\n")
    
    # subtask 3 ---------------------------------------------------------------
    # cat("\nSubtask 2.3 (distances 2) started...\n") 
    # my_time <- Sys.time()
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
    sparsed <- sparsed2
    
    path <- Sys.time() |>
        as.character() |>
        stringr::str_split_1("\\.") %>% 
        `[`(1) |>
        stringr::str_replace_all( ":", "-")
    save(
        list = c("sparsed", ""), 
        file = paste0("inv.points.sparsed_", path, ".RData")
    )
    
    
    # path <- str_subset(dir(pattern = "RData"), "inv.points.sparsed2")

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

# readr::write_delim(EW_NM_points, paste0("export/EW-NM-points_", Sys.Date(), ".csv"))

EW_NM_points <- sf::st_as_sf(
    EW_NM_points, 
    coords = c("lon", "lat"), 
    crs = 4326, 
    remove = F)


path <- Sys.time() |>
    as.character() |>
    stringr::str_split_1("\\.") %>% 
    `[`(1) |>
    stringr::str_replace_all( ":", "-")

save(list = c("EW_NM_points", "random_points"), file = paste0("input_data/EW-NM-random-points_raw_", path, ".RData"))
cat("\nSubtask 2.3 finished:\n"); Sys.time() - my_time; cat("\n")
cat("\nTask 2 finished:\n"); Sys.time() - my_time0; cat("\n")












# save("random_points", file = paste0("random-points_terrain_", Sys.Date(), ".RData"))



cat("\nTask 1 finished:\n"); Sys.time() - my_time
