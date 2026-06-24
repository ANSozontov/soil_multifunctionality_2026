# 0. points loading ----------------------------------------------------------
# 1.5 sec
cat("\nTask 4 started...\n") 
cat("Subtask 0 started: points loading...")
time1 <- time0 <- Sys.time()
suppressMessages({
    library(sf)
    library(wdpar)
    library(parallel)
    library(raster)
    library(terra)
    library(foreign)
    library(tidyverse)
})

path <- str_subset(dir(pattern = "RData"), "data_pt1_worldcover_")
if(length(path)<1){
    cli::cli_abort("Warning! There is no points data in the working directory")
} else {
    load(sort(path, decreasing = TRUE)[1])
}

random_points <- random_points %>% 
    dplyr::filter(worldcover %in% c(
        "Bare.sparse.vegetation", "Cropland", "Grassland", "Shrubland", 
        "Herbaceous.wetland", "Moss.and.lichen", "Tree.cover")) %>% 
    rename(lon = x, lat = y) %>%
    mutate(id = 1:nrow(.), ID = 1:nrow(.), .before = 1)

EW_NM_points <- EW_NM_points %>% 
    dplyr::filter(worldcover %in% c(
        "Bare.sparse.vegetation", "Cropland", "Grassland", "Shrubland", 
        "Herbaceous.wetland", "Moss.and.lichen", "Tree.cover")) %>% 
    dplyr::select(-EWID, -NMID) %>% 
    mutate(id = 1:nrow(.), ID = 1:nrow(.), .before = 1)

pts <- rbind(EW_NM_points[,1:2], random_points[,1:2])
pts$type <- c(rep("EW", nrow(EW_NM_points)), rep("rnd", nrow(random_points)))

cat("\nSubtask 0 end. Points have been loaded\n"); Sys.time() - time1; cat("\n\n\n")

# 1. Human footprint ---------------------------------------------------------
# 1.5 sec
cat("Subtask 1 started: Human footprint...")
time1 <- Sys.time()

hfp <- terra::rast("rasters/01_human_footprint/hfp2022.tif")
tmp.points <- st_transform(EW_NM_points, st_crs(hfp))
EW_NM_points$Human_Footprint_2022 <- raster::extract(hfp, tmp.points)$hfp2022
tmp.points <- st_transform(random_points, st_crs(hfp))
random_points$Human_Footprint_2022 <- raster::extract(hfp, tmp.points)$hfp2022
rm(hfp, tmp.points)

cat("\nSubtask 1 end. Human footprint is extracted\n"); Sys.time() - time1; cat("\n\n\n")
# 2. fungal diversity --------------------------------------------------------
# 10 sec
cat("Subtask 2 started: Fngal diversity...")
time1 <- Sys.time()

if(!file.exists("rasters/02_fungal_diversity_zenodo.org_records_8013448")){
    cat("Fungal rasters absent, let's download:\n")
    dir.create("zenodo.org_records_8013448")
    files <- c("Alpha_Hotspots_and_ProtectedAreas.tif", 
               "Alpha_S_AllFungi_Consensus.tif", 
               "Alpha_S_AllFungi_GSMc.tif", "Alpha_S_AM_GSMc.tif", 
               "Alpha_S_EcM_GSMc.tif", "Alpha_S_Mold_GSMc.tif", 
               "Alpha_S_NMA_GSMc.tif", "Alpha_S_OHP_GSMc.tif", 
               "Alpha_S_Path_GSMc.tif", "Alpha_S_Ucel_GSMc.tif", 
               "Alpha_S_Yeast_GSMc.tif", "Alpha_SESPD_GSMc.tif", 
               "Alpha_SPD_GSMc.tif", "Beta_LocalTurnover.tif", 
               "Beta_Phylogenetic_AllFungi.tif", 
               "Beta_Taxonomic_AllFungi.tif", 
               "EcM_and_AM_GlobalDistribution.tif")
    for(i in 1:length(files)) {
        cat(i, " of ", length(files), ": ", files[i], "\n", sep = "")
        i %>% 
            paste0("https://zenodo.org/records/8013448/files/", ., "?download=1") %>% 
            download.file(
                url = ., 
                destfile = paste0("rasters/02_fungal_diversity_zenodo.org_records_8013448/", i)
            )
    }
    cat("Fungal rasters have been downloaded, let's extract:\n")
} else {
    cat("Fungal rasters were been downloaded already, let's extract:\n")
}

files <- dir("rasters/02_fungal_diversity_zenodo.org_records_8013448", pattern = "tif") %>% 
    str_subset("Uncertainty|AOA|Beta.*AllFungi|Protected", negate = TRUE)

res_fungi <- 1:length(files) %>% 
    # `[`(1:2) %>%
    mclapply(function(i){
        r <- terra::rast(paste0("rasters/02_fungal_diversity_zenodo.org_records_8013448/", files[i]))
        nm <- names(r)
        fungi_ewnm <- raster::extract(r, EW_NM_points) %>% 
            dplyr::select(-ID, -ends_with("IQR"))
        colnames(fungi_ewnm) <- paste0(colnames(fungi_ewnm), "...", files[i])
        fungi_rnd <- raster::extract(r, random_points) %>% 
            dplyr::select(-ID, -ends_with("IQR"))
        colnames(fungi_rnd) <- paste0(colnames(fungi_rnd), "...", files[i])
        list(fungi_ewnm=fungi_ewnm, fungi_rnd=fungi_rnd)
    }, mc.cores = 16) %>% 
    purrr::transpose()

EW_NM_points <- res_fungi %>% 
    pluck("fungi_ewnm") %>% 
    map_dfc(cbind) %>% 
    as_tibble() %>% 
    cbind(EW_NM_points, .) 

random_points <- res_fungi %>% 
    pluck("fungi_rnd") %>% 
    map_dfc(cbind) %>% 
    as_tibble() %>% 
    cbind(random_points, .) 

suppressWarnings(a <- readLines("https://zenodo.org/records/8013448"))
tibble(x = a[
    which(str_detect(a, "<strong>### Data overview")):
        which(str_detect(a, "<strong>### Source code"))
]
) %>% 
    mutate(
        x = str_remove_all(x, "<[^>]*>|`|&[^;]*;|\t"), 
        x = str_squish(x)) %>% 
    pull(x) %>% 
    `[`(-length(.)) %>% 
    write_lines("readme_soil.fungi.txt", append = TRUE)

cat("\nSubtask 2 end. Fungal diversity is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 3. E&AM --------------------------------------------------------------------
# 0.3 sec
# "10.5061/dryad.866t1g1tt" 
cat("Subtask 3 started: mycorrhiz...")
time1 <- Sys.time()

files <- dir("rasters/03_mycorrhiz_doi_10_5061_dryad_866t1g1tt", pattern = "tif")

emam <- 1:length(files) %>% 
    mclapply(function(i){
        r <- terra::rast(paste0("rasters/03_mycorrhiz_doi_10_5061_dryad_866t1g1tt/", files[i]))
        em.am_ewnm <- raster::extract(r, EW_NM_points) %>% 
            dplyr::select(-ID) %>% 
            tibble::as_tibble()
        colnames(em.am_ewnm) <- paste0("mycorrhiz...", files[i])
        em.am_rnd <- raster::extract(r, random_points) %>% 
            dplyr::select(-ID, -ends_with("IQR")) %>% 
            tibble::as_tibble()
        colnames(em.am_rnd) <- paste0("mycorrhiz...", files[i])
        list(em.am_ewnm=em.am_ewnm, em.am_rnd=em.am_rnd)
    }, mc.cores = 16) %>% 
    purrr::transpose()

EW_NM_points <- emam %>% 
    pluck("em.am_ewnm") %>% 
    map_dfc(cbind) %>% 
    as_tibble() %>% 
    cbind(EW_NM_points, .) 

random_points <- emam %>% 
    pluck("em.am_rnd") %>% 
    map_dfc(cbind) %>% 
    as_tibble() %>% 
    cbind(random_points, .) 

cat("\nSubtask 3 end. mycorrhiz... is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 4a. Soil Grids get -----------------------------------------------------------
# 1.5 sec
cat("Subtask 4 started: Soil grids data...")
time1 <- Sys.time()

files <- c("bdod", "cec", "cfvo", "clay", "nitrogen",
           "ocd", "phh2o", "sand", "silt", # "ocs",
           "soc", "wv0010", "wv0033", "wv1500")

paths <- c("0-5", "100-200", "15-30", "30-60",
           "5-15", "60-100")

files <- expand_grid(f = files, depths = paths) %>% 
    rbind(tibble(f = "ocs", depths = "0-30")) %>% 
    transmute(
        file = paste0(
            f, 
            "_", 
            depths, 
            "cm_mean_5000.tif"
        ),
        url = paste0(
            "https://files.isric.org/soilgrids/latest/data_aggregated/5000m/",
            f,
            "/",
            f, 
            "_", 
            depths, 
            "cm_mean_5000.tif")
        )

if(!dir.exists("./rasters/04_soil_grids")){
    cat("Fungal rasters absent, let's download:\n")
    dir.create("soil_grids")
    
    for(i in 1:nrow(files)) {
        cat(i, " of ", nrow(files), ": ", files$file[i], "\n", sep = "")
        if(!file.exists(paste0("rasters/04_soil_grids/", files$file[i]))){
            download.file(
                url = files$url[i], 
                destfile = paste0("rasters/04_soil_grids/", files$file[i])
            )
        }
    }
} else {
    cat("Fungal rasters were been downloaded already, let's extract:\n")
}


# 4b. Soil Grids extract ------------------------------------------------------
soilgrids_result <- 1:nrow(files) %>% 
    # `[`(1:2) %>% 
    mclapply(function(i){
        r <- terra::rast(x = paste0("rasters/04_soil_grids/", files$file[i]))
        
        tmp_points <- st_transform(EW_NM_points, st_crs(r))
        soilgrids_ewnm <- raster::extract(r, tmp_points) %>% 
            dplyr::select(-ID) %>% 
            tibble::as_tibble()
        colnames(soilgrids_ewnm) <- paste0("soil.grids...", files$file[i])
        
        tmp_points <- st_transform(random_points, st_crs(r))
        soilgrids_rnd <- raster::extract(r, tmp_points) %>% 
            dplyr::select(-ID) %>% 
            tibble::as_tibble()
        colnames(soilgrids_rnd) <- paste0("soil.grids...", files$file[i])
        list(soilgrids_ewnm=soilgrids_ewnm, soilgrids_rnd=soilgrids_rnd)
    }, mc.cores = 16) %>% 
    purrr::transpose()
    
EW_NM_points <- soilgrids_result %>% 
    pluck("soilgrids_ewnm") %>% 
    map_dfc(cbind) %>% 
    as_tibble() %>% 
    cbind(EW_NM_points, .) 

random_points <- soilgrids_result %>% 
    pluck("soilgrids_rnd") %>% 
    map_dfc(cbind) %>% 
    as_tibble() %>% 
    cbind(random_points, .) 

cat("\nSubtask 4 end. Soil grids is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 5. NDVI --------------------------------------------------------------------
# 2.5 sec
cat("Subtask 5 started: NDVI...")
time1 <- Sys.time()
# https://documentation.dataspace.copernicus.eu/Data/CopernicusServices/CLMS.html
# https://documentation.dataspace.copernicus.eu/APIs/S3.html

# download NDVI
if(!dir.exists("rasters/05_NDVI_eodata_CLMS")){
    ndvi <- expand_grid(x1 = 1:6, x2 = c('01', '11', '21')) %>% 
        transmute(cmd = paste0(
            "aws s3 cp s3://eodata/CLMS/bio-geophysical/vegetation_indices/ndvi_global_1km_10daily_v3/2020/0", 
            x1, 
            "/", 
            x2, 
            "/c_gls_NDVI_20200", 
            x1, 
            x2, 
            "0000_GLOBE_PROBAV_V3.0.1_cog/c_gls_NDVI-NDVI_20200", 
            x1, 
            x2, 
            "0000_GLOBE_PROBAV_V3.0.1.tiff ./NDVI_eodata_CLMS/"
        ))
    
    for(i in 1:nrow(ndvi)){
        system(ndvi$cmd[i])
    }
    
}

files <- dir("rasters/05_NDVI_eodata_CLMS", pattern = "tif")

ndvi <- 1:length(files) %>% 
    # `[`(1:2) %>%
    mclapply(function(i){
        r <- terra::rast(paste0("rasters/05_NDVI_eodata_CLMS/", files[i]))
        
        ndvi_ewnm <- raster::extract(r, EW_NM_points) %>% 
            dplyr::select(-ID) %>% 
            as_tibble()
        ndvi_rnd <- raster::extract(r, random_points) %>% 
            dplyr::select(-ID) %>% 
            as_tibble()
        list(ndvi_ewnm=ndvi_ewnm, ndvi_rnd=ndvi_rnd)
        
    }, mc.cores = 16)

ndvi <- purrr::transpose(ndvi)
suppressMessages({
    ndvi <- lapply(ndvi, function(a){map_dfc(a, cbind)})
})
ndvi <- map(ndvi, ~apply(.x, 1, function(y){mean(y, na.rm = TRUE)}))
ndvi$ndvi_ewnm[is.nan(ndvi$ndvi_ewnm)] <- NA
ndvi$ndvi_rnd[is.nan(ndvi$ndvi_rnd)] <- NA
EW_NM_points$ndvi_2020  <- ndvi$ndvi_ewnm
random_points$ndvi_2020 <- ndvi$ndvi_rnd

cat("\nSubtask 5 end. NDVI is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 6. 2000-2020 change  -------------------------------------------------------
# 5.5 sec
# https://www.frontiersin.org/journals/remote-sensing/articles/10.3389/frsen.2022.856903/full
cat("Subtask 6 started: 2000-2020 change...")
time1 <- Sys.time()

files <- dir("rasters/06_change_2000-2020", pattern = "tif")

change20 <- 1:length(files) %>% 
    # `[`(1:64) %>%
    mclapply(function(i){
        # rr <- terra::rast(paste0("change_2000-2020/", files[i]))
        r <- raster::raster(paste0("rasters/06_change_2000-2020/", files[i]))
        bb <- raster::bbox(r)
        
        new_rnd <- which(
            random_points$lon >= bb[1,1] &
                random_points$lon <= bb[1,2] &
                random_points$lat >= bb[2,1] &
                random_points$lat <= bb[2,2]
        )
        
        if(length(new_rnd)>0){
            new_rnd <- data.frame(
                id = new_rnd,
                ch20 = raster::extract(r, random_points[new_rnd,])
            )
        } else {
            new_rnd <- data.frame(
                id = integer(),
                ch20 = numeric()
            )
        }
        
        new_ewnm <- which(
            EW_NM_points$lon >= bb[1,1] &
                EW_NM_points$lon <= bb[1,2] &
                EW_NM_points$lat >= bb[2,1] &
                EW_NM_points$lat <= bb[2,2]
        )
        
        if(length(new_ewnm)>0){
            new_ewnm <- data.frame(
                id = new_ewnm,
                ch20 = raster::extract(r, EW_NM_points[new_ewnm,])
            )
        } else {
            new_ewnm <- data.frame(
                id = integer(),
                ch20 = numeric()
            )
        }
        
        # change20_ewnm <- raster::extract(r, EW_NM_points) %>% 
        #     dplyr::select(-ID) %>% 
        #     as_tibble()
        # change20_rnd <- raster::extract(r, random_points) %>% 
        #     dplyr::select(-ID) %>% 
        #     as_tibble()
        list(change20_ewnm=new_ewnm, change20_rnd=new_rnd)
        # 16, 9, 15, 7, 11 
    }, mc.cores = 16)

# which(!sapply(change20, is.list))

change20 <- purrr::transpose(change20)
change20 <- lapply(change20, function(a){map_dfr(a, rbind)})

EW_NM_points$change_2000.2020  <- change20$change20_ewnm$ch20 
random_points <- random_points %>% 
    left_join(change20$change20_rnd, by = "id") %>% 
    rename(change_2000.2020 = ch20)
# select(-id, change_2000.2020 = ch20) 
# EW_NM_points <- EW_NM_points %>% 
#     cbind(change20$change20_ewnm[,-1])
#     mutate(id = 1:n(), .before = 1) %>% 
#     left_join(rename(change20$change20_ewnm, change_2000.2020 = ch20), by = "id")
 # select(-id, change_2000.2020 = ch20) 

# random_points <- random_points %>% 
#     mutate(id = 1:n(), .before = 1) %>% 
#     left_join(rename(change20$change20_rnd, change_2000.2020 = ch20), by = "id")
    # select(-id, change_2000.2020 = ch20) 

cat("\nSubtask 3 end. 2000-2020 change is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 7. Forest_height_2019 ------------------------------------------------------
# 4 sec
cat("Subtask 7 started: Forest_height_2019...\n")
time1 <- Sys.time()

files <- str_subset(dir("rasters/07_forest_height_2019_glad.umu.edu", pattern = "tif"), "xml", negate = TRUE)

forest_h <- 1:length(files) %>% 
    # `[`(1:64) %>%
    mclapply(function(i){
        r <- raster::raster(paste0("rasters/07_forest_height_2019_glad.umu.edu", "/", files[i]))
        bb <- raster::bbox(r)
        
        new_rnd <- which(
            random_points$lon >= bb[1,1] &
                random_points$lon <= bb[1,2] &
                random_points$lat >= bb[2,1] &
                random_points$lat <= bb[2,2]
        )
        
        if(length(new_rnd)>0){
            new_rnd <- data.frame(
                id = new_rnd,
                forest = raster::extract(r, random_points[new_rnd,])
            )
        } else {
            new_rnd <- data.frame(
                id = integer(),
                forest = numeric()
            )
        }
        
        new_ewnm <- which(
            EW_NM_points$lon >= bb[1,1] &
                EW_NM_points$lon <= bb[1,2] &
                EW_NM_points$lat >= bb[2,1] &
                EW_NM_points$lat <= bb[2,2]
        )
        
        if(length(new_ewnm)>0){
            new_ewnm <- data.frame(
                id = new_ewnm,
                forest = raster::extract(r, EW_NM_points[new_ewnm,])
            )
        } else {
            new_ewnm <- data.frame(
                id = integer(),
                forest = numeric()
            )
        }
        
        list(forest_rnd = new_rnd, forest_ewnm = new_ewnm)
        
    }, mc.cores = 16)

forest_h <- purrr::transpose(forest_h)
forest_h <- lapply(forest_h, function(a){map_dfr(a, rbind)})

EW_NM_points <- EW_NM_points %>% 
    left_join(forest_h$forest_ewnm, by = "id") %>% 
    rename(forest_height_2019 = forest)

random_points <- random_points %>% 
    left_join(forest_h$forest_rnd, by = "id") %>% 
    rename(forest_height_2019 = forest)

cat("\nSubtask 7 end. forest_height_2019 is here!\n"); Sys.time() - time1; cat("\n\n\n")
# 8. Lithology ------------------------------------------------------------
# 1.8 sec
cat("Subtask 8 started: Lithology 1.0 model\n")
time1 <- Sys.time()


lith <- terra::rast("rasters/08_LITHO1.0/LITHO1.0.nc")
lith <- subset(lith, str_subset(names(lith), "_vs|_vp|_density|_qkappa|_qmu|_eta|ice|asthenos|water|lower_sediments|middle_sediments", negate = TRUE))
EW_NM_points <- terra::extract(lith, EW_NM_points) %>% 
    as_tibble() %>% 
    dplyr::rename(id = ID) %>%  
    pivot_longer(names_to = "layer", values_to = "val", -id) %>% 
    mutate(
        layer = str_remove_all(layer, "_depth|_bottom|_top"), 
        border = map_chr(.$layer, ~str_extract_all(.x, "bottom|top")[[1]])
    ) %>% 
    pivot_wider(names_from = border, values_from = val) %>% 
    transmute(id, layer, val = abs(top - bottom)) %>% 
    pivot_wider(names_from = layer, values_from = val) %>% 
    `colnames<-`(c("id", paste0("LITH1.0...", colnames(.)[-1]))) %>% 
    left_join(EW_NM_points, ., by = "id")

random_points <- terra::extract(lith, random_points) %>% 
    as_tibble() %>% 
    dplyr::rename(id = ID) %>%  
    pivot_longer(names_to = "layer", values_to = "val", -id) %>% 
    mutate(
        layer = str_remove_all(layer, "_depth|_bottom|_top"), 
        border = map_chr(.$layer, ~str_extract_all(.x, "bottom|top")[[1]])
    ) %>% 
    pivot_wider(names_from = border, values_from = val) %>% 
    transmute(id, layer, val = abs(top - bottom)) %>% 
    pivot_wider(names_from = layer, values_from = val) %>% 
    `colnames<-`(c("id", paste0("LITH1.0...", colnames(.)[-1]))) %>% 
    left_join(random_points, ., by = "id")

cat("\nSubtask 8 end. Lithology 1.0 model is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 9. USGS classification ------------------------------------------------------------
# 2.1 sec
cat("Subtask 9 started: USGS classification \n")
time1 <- Sys.time()

if(file.exists("rasters/09_USGS/World_Ecological_2015.tif.vat.dbf")){
    tmp <- file.rename("rasters/09_USGS/World_Ecological_2015.tif.vat.dbf", "rasters/09_USGS/World_Ecological_2015.tif.vat.dbf.tmp")
}
eco_rast <- terra::rast("rasters/09_USGS/World_Ecological_2015.tif")
eco_labs <- foreign::read.dbf("rasters/09_USGS/World_Ecological_2015.tif.vat.dbf.tmp")

EW_NM_tmp1 <- terra::extract(eco_rast, EW_NM_points) %>% 
    as_tibble() %>% 
    rename(Value = World_Ecological_2015) %>% 
    left_join(eco_labs, by = "Value") %>% 
    dplyr::select(ID, Bio_Val, Bio_Des = EF_Bio_Des, LF_Val, LF_Desc = EF_LF_Desc, Lit_Val, Lit_Des = EF_Lit_Des, GLC_Val, GLC_Des = EF_GLC_Des)
colnames(EW_NM_tmp1) <- c("ID", paste0("USGS.Ecological...", colnames(EW_NM_tmp1)[-1]))
EW_NM_points <- cbind(EW_NM_points, EW_NM_tmp1[,-1])


random_tmp1 <- terra::extract(eco_rast, random_points) %>% 
    as_tibble() %>% 
    rename(Value = World_Ecological_2015) %>% 
    left_join(eco_labs, by = "Value") %>% 
    dplyr::select(ID, Bio_Val, Bio_Des = EF_Bio_Des, LF_Val, LF_Desc = EF_LF_Desc, Lit_Val, Lit_Des = EF_Lit_Des, GLC_Val, GLC_Des = EF_GLC_Des)
colnames(random_tmp1) <- c("ID", paste0("USGS.Ecological...", colnames(random_tmp1)[-1]))
random_points <- cbind(random_points, random_tmp1[,-1])


if(file.exists("rasters/09_USGS/World_Ecological_2015.tif.vat.dbf.tmp") && !file.exists("rasters/09_USGS/World_Ecological_2015.tif.vat.dbf")){
    tmp <- file.rename("rasters/09_USGS/World_Ecological_2015.tif.vat.dbf.tmp", "rasters/09_USGS/World_Ecological_2015.tif.vat.dbf")
}
rm(EW_NM_tmp1, random_tmp1, eco_labs, eco_rast, tmp)

cat("\nSubtask 9 end. USGS classification is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 10 NASA ---------------------------------------------------------------------
# 8 sec
cat("Subtask 10 started: NASA sources \n")
time1 <- Sys.time()

files <- dir("rasters/10_NASA/", include.dirs = TRUE)
files <- map_dfr(files, ~tibble(dd = .x, ff = dir(paste0("rasters/10_NASA/", .x), pattern = "tif"))) %>% 
    filter(str_detect(ff, "zip", negate = TRUE)) %>% 
    mutate(dd = factor(dd, levels = c("NASA_carbon", "NASA_soil_respiration", "NASA_cropland", "NASA_mammals", "NASA_amphibians"))) %>% 
    arrange(dd)

res <- files %>% 
    split(1:nrow(.)) %>% 
    # `[`(1:2) %>% 
    map(~list(rst = .x, pts = pts)) %>% 
    mclapply(
        function(x){
            files <- x$rst
            pts <- x$pts
            r <- terra::extract(terra::rast(paste0("rasters/10_NASA/", files$dd, "/", files$ff)), pts) %>% 
                as_tibble
            colnames(r)[2] <- files$ff
            return(r)
        }, 
        mc.cores = 16
    )

res <- map_dfc(res, ~dplyr::select(.x, -ID)) %>% 
    `colnames<-`(paste0(files$dd, "...", files$ff)) %>% 
    cbind(pts, .) %>% 
    st_drop_geometry() %>% 
    as_tibble

EW_NM_points <- left_join(EW_NM_points, filter(res, type == "EW"), by = c("id", "ID"))
random_points <- left_join(random_points, filter(res, type == "rnd"), by = c("id", "ID"))

rm(res, files)
cat("\nSubtask 10 end. NASA sources is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 11. landscan-global-population ------------------------------------------
# 0.9 sec
cat("Subtask 11 started: landscan-global-population \n")
time1 <- Sys.time()

tt <- terra::rast("rasters/11_landscan-global-population/landscan-global-2024.tif")
res <- pts %>% 
    split(sample(1:16, nrow(.), replace = TRUE)) %>% 
    mclapply(
        function(x){
            cbind(x, landscan_global_population_2024 = terra::extract(tt, x, fun = mean, na.rm = TRUE)[,-1])
        }, 
        mc.cores = 16
    ) %>% 
    map(~split(.x, .x$type)) %>% 
    transpose() %>% 
    map(~.x %>% 
            map_dfr(rbind) %>% 
            # arrange(id))
            select(-type) %>% 
            st_drop_geometry
    )

EW_NM_points  <- left_join(EW_NM_points,  res$EW, by = c("ID", "id"))
random_points <- left_join(random_points, res$rnd, by = c("ID", "id"))
# EW_NM_points$landscan_global_population_2024 <- res$EW$landscan_global_population_2024
# random_points$landscan_global_population_2024 <- res$rnd$landscan_global_population_2024

cat("\nSubtask 11 end. landscan-global-population is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 12. Open Land map -------------------------------------------------------------------------
# 1.2 mins
cat("Subtask 12 started: Open Land map \n")
time1 <- Sys.time()
files <- dir("rasters/12_openlandmap/", pattern = "xlsx") %>% 
    sort(decreasing = TRUE) %>% 
    `[`(1) %>% 
    paste0("rasters/12_openlandmap/", .) %>% 
    readxl::read_excel() %>% 
    filter(str_detect(link, "[:space:]", negate = TRUE)) %>% 
    tidyr::fill(group) 
files$file <- str_split(files$link, "/+") %>% map_chr(~`[`(.x, length(.x)))

# summarize_12 - 1
res <- files %>% 
    filter(group == "Sentinel-5P monthly tropospheric nitrogen dioxide density") %>% 
    pull(file) %>% 
    mclapply(
        function(x){
            terra::extract(terra::rast(paste0("rasters/12_openlandmap/", x)), pts)
        }, 
        mc.cores = 12
    ) %>% 
    map_dfc(~.x[2]) %>% 
    apply(1, function(y){mean(y, na.rm = TRUE)}) %>% 
    mutate(pts, Sentinel_tropospheric_nitrogen_dioxide = .)

# summarize_12 - 2
res <- files %>% 
    filter(group == "ESA long-term snow cover fraction") %>% 
    pull(file) %>% 
    mclapply(
        function(x){
            terra::extract(terra::rast(paste0("rasters/12_openlandmap/", x)), pts)
        }, 
        mc.cores = 12
    ) %>% 
    map_dfc(~.x[2]) %>% 
    # slice(630:635)
    apply(1, function(y){mean(y, na.rm = TRUE)}) %>% 
    mutate(res, ESA_snow_cover_fraction = .)


# summarize_12 - 3
res <- files %>% 
    filter(group == "MCD19A2 long-term water vapor (perc. 50th)") %>% 
    pull(file) %>% 
    mclapply(
        function(x){
            terra::extract(terra::rast(paste0("rasters/12_openlandmap/", x)), pts)
        }, 
        mc.cores = 12
    ) %>% 
    map_dfc(~.x[2]) %>% 
    # slice(630:635)
    apply(1, function(y){mean(y, na.rm = TRUE)}) %>% 
    mutate(res, MCD19A2_longterm_water_vapor = .)

# others non 12 
others <- files %>% 
    filter_out(group %in% c("MCD19A2 long-term water vapor (perc. 50th)", "Sentinel-5P monthly tropospheric nitrogen dioxide density", "ESA long-term snow cover fraction")) %>% 
    # slice(3, 11, 16) %>% # 11, 
    filter_out(file %in% c(
        # "evi_mod13q1.stl.trend.logit.ols.beta_m_250m_s_20000101_20201231_go_espg.4326_v20230608.tif", # ...
        "fapar_proba.v.annual_d_250m_s_20140101_20171231_go_epsg.4326_v1.0.tif",  # Перекачал, все равно не читается
        "organic.carbon.stock_msa.kgm2_m_250m_b0t30cm_19500101_20171231_go_epsg.4326_v0.2.tif"  # недоступен на сайте
    )) %>% 
    pull(file) %>% 
    mclapply(
        function(x){
            terra::extract(terra::rast(paste0("rasters/12_openlandmap/", x)), pts)
        }, 
        mc.cores = 16
    ) %>% 
    map_dfc(~.x[2])

others <- "go_epsg.4326|go_espg.4326|[:digit:]{6,}|_a_v|_v1.0|_v0.2" %>% 
    str_remove_all(colnames(others), .) %>% 
    `colnames<-`(others, .) %>% 
    `colnames<-`(str_replace_all(colnames(.), "_+", "_")) %>% 
    `colnames<-`(paste0("", colnames(.))) #%>% colnames 

res <- cbind(res, others) %>% 
    st_drop_geometry %>% 
    as_tibble() 
colnames(res) <- c("id", "ID", "type", paste0("openlandmap...", colnames(res)[-1:-3]))
res <- res %>% 
    split(.$type) %>% 
    map(~dplyr::select(.x, -type))


EW_NM_points <- left_join(EW_NM_points, res$EW, by = c("id", "ID"))
random_points <- left_join(random_points, res$rnd, by = c("id", "ID"))

# files_downloaded <- dir("rasters/12_openlandmap/", pattern = ".tif") 
# files %>% 
#     mutate(ex = file %in% files_downloaded) %>% 
#     filter(ex == F) 

cat("\nSubtask 12 end. Open Land map is here!\n"); Sys.time() - time1; cat("\n\n\n")


# 13. Harmonized World Soil Database --------------------------------------
# 0.6 sec
cat("Subtask 13 started: Harmonized World Soil Database \n")
time1 <- Sys.time()
# https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v12/en/
files <- dir("rasters/13_hwsd_1.2/", ".asc")

res <- files %>% 
    mclapply(
        function(x){
            terra::extract(terra::rast(paste0("rasters/13_hwsd_1.2/", x)), pts)
        }, 
        mc.cores = 16
    ) %>% 
    map_dfc( ~dplyr::select(.x, -ID)) %>% 
    cbind(pts, .) %>% 
    st_drop_geometry() %>% 
    as_tibble %>% 
    dplyr::select(
        id, ID, type,
        total_cultivated_land = CULT_2000,
        irrigated_cultivated_land = CULTIR_2000,
        `rain-fed_cultivated_land` = CULTRF_2000,
        forest_land = FOR_2000,
        grass_scrub_woodland = GRS_2000,
        barren_or_very.sparsely.vegetated.land = NVG_2000,  
        `built-up_land` = URB_2000,
        water_bodies = WAT_2000,
        nutrient_availability = sq1,
        nutrient_retention_capacity = sq2,
        rooting_conditions = sq3,
        oxygen_availability_to_roots = sq4,
        excess_salts = sq5,
        toxicity = sq6,
        workability = sq7
    ) %>% 
    `colnames<-`(c("id", "ID", "type", paste0("HWSD_1.2...", colnames(.)[-1:-3]))) %>% 
    split(.$type) %>% 
    map(~dplyr::select(.x, -type))

EW_NM_points <- left_join(EW_NM_points, res$EW, by = c("id", "ID"))
random_points <- left_join(random_points, res$rnd, by = c("id", "ID"))
    
cat("\nSubtask 13 end. Harmonized World Soil Database is here!\n"); Sys.time() - time1; cat("\n\n\n")

# 14a. WDPA Protected areas: preparation -----------------------------------------------------
# 8 sec
cat("Subtask 14a started: WDPA Protected areas preparation \n")
time1 <- Sys.time()
files <- dir("vectors/14_WDPA.cached/", ".gpkg")
if(length(files)<32){
    cat("geometry was not prepared! preparin in progres...")
    source("04a_prepare_protected_areas.R")
} else {
    cat("geometry was been simplified already, loading... \n")
    res <- 1:32 %>% 
        # `[`(1:2) %>%
        mclapply(
            function(i){
                paste0("vectors/14_WDPA.cached/", i, ".gpkg") %>% 
                    st_read(as.character(i), quiet = TRUE) %>% # wdpar::st_repair_geometry(
                    st_transform(3857)
            }, 
            mc.cores = 16
        )
}
cat("loaded: "); (Sys.time()-time1); cat("\n\n\n")

# 14b. WDPA Protected areas: extraction  -----------------------------------------------------
# 2.5 mins
cat("Subtask 14b started: Protected areas: extraction \n")
pts <- st_transform(pts, 3857)

time1 <- Sys.time()
distances <- 1:32 %>% 
    mclapply(
        function(i){
            neighbors <- st_nearest_feature(pts, st_repair_geometry(res[[i]]))
            as.vector(st_distance(pts, res[[i]][neighbors,], by_element = TRUE))
        },
        mc.cores = 12
    ) 
Sys.time() - time1

distances <- map_dfc(distances, cbind) %>% 
    apply(1, min) %>% 
    suppressMessages()
distances <- pts %>% 
    mutate(dist_to_oopt_km = round(distances/1000, 1)) %>% 
    split(.$type) %>% 
    map(~.x %>% 
            st_drop_geometry %>% 
            select(-type)
        )

EW_NM_points <- left_join(EW_NM_points, distances$EW, by = c("ID", "id"))
random_points <- left_join(random_points, distances$rnd, by = c("ID", "id"))
cat("\nSubtask 14b end. WDPA Protected areas is here!\n"); Sys.time() - time1; cat("\n\n\n")
# export ------------------------------------------------------------------
path <- Sys.time() %>% 
    format("%Y-%m-%d")
    
save(
    list = c("EW_NM_points", "random_points"), 
    file = paste0("export/data_4_extracted_", path, ".RData")
)

list(
        random_points = random_points,
        invertebrates = EW_NM_points
    ) %>% 
    lapply(st_drop_geometry) %>% 
    writexl::write_xlsx(paste0("export/points_exported_", path, ".xlsx"))

cat("\nTask 4 finished. Total time consumed:\n"); Sys.time() - time0; cat("\n\n\n")
cat("Yor files are in 'exported' directory with dates of generation in names")