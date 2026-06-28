# 0. loading ----------------------------------------------------------
suppressMessages({
    library(sf)
    library(wdpar)
    library(parallel)
    library(raster)
    library(terra)
    library(foreign)
    library(tidyverse)
})

# 1. Human footprint ---------------------------------------------------------
v01_hfp <- terra::rast("rasters/01_human_footprint/hfp2022.tif")
# 2. fungal diversity --------------------------------------------------------
files <- dir("rasters/02_fungal_diversity_zenodo.org_records_8013448", pattern = "tif") %>% 
    str_subset("Uncertainty|AOA|Beta.*AllFungi|Protected", negate = TRUE)

v02_fungi <- lapply(paste0("rasters/02_fungal_diversity_zenodo.org_records_8013448/", files), terra::rast)
template <- v02_fungi[[1]]

v02_fungi <- lapply(v02_fungi, function(r) {
    terra::resample(r, template, method = "near")
}) 
v02_fungi <- terra::rast(v02_fungi)


v02_fungi <- 1:length(files) %>% 
    # `[`(1:2) %>%
    mclapply(function(i){
        terra::rast(paste0("rasters/02_fungal_diversity_zenodo.org_records_8013448/", files[i]))
    }, mc.cores = 16)
names(v02_fungi) <- files
rast(v02_fungi)

# 3. E&AM --------------------------------------------------------------------
files <- dir("rasters/03_mycorrhiz_doi_10_5061_dryad_866t1g1tt", pattern = "tif")

v03_emam <- lapply(paste0("rasters/03_mycorrhiz_doi_10_5061_dryad_866t1g1tt/", files), terra::rast)
template <- v03_emam[[1]]

v03_emam <- lapply(v03_emam, function(r) {
    terra::resample(r, template, method = "near")
}) 
v03_emam <- terra::rast(v03_emam)

# 4. Soil Grids -----------------------------------------------------------
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


v04_soilgrids <- 1:nrow(files) %>% 
    # `[`(1:2) %>% 
    lapply(function(i){
        terra::rast(x = paste0("rasters/04_soil_grids/", files$file[i]))
    })
v04_soilgrids <- rast(v04_soilgrids)

# 5. NDVI --------------------------------------------------------------------
files <- dir("rasters/05_NDVI_eodata_CLMS", pattern = "tif")


v05_ndvi <- 1:length(files) %>% 
    lapply(function(i){
        terra::rast(paste0("rasters/05_NDVI_eodata_CLMS/", files[i]))
    })
v05_ndvi <- rast(v05_ndvi)


# 6. 2000-2020 change  -------------------------------------------------------
# files <- dir("rasters/06_change_2000-2020", pattern = "tif")
# 
# change20 <- 1:length(files) %>% 
#     mclapply(function(i){
#         terra::rast(paste0("change_2000-2020/", files[i]))
#     }, mc.cores = 16)

# 7. Forest_height_2019 ------------------------------------------------------
# files <- str_subset(dir("rasters/07_forest_height_2019_glad.umu.edu", pattern = "tif"), "xml", negate = TRUE)

# 8. Lithology ------------------------------------------------------------
v08_lith <- terra::rast("rasters/08_LITHO1.0/LITHO1.0.nc")
# "asthenospheric_mantle_top"  "lower_crust_top_depth"  "lid_top_depth" 
v08_lith <- v08_lith[[which(str_detect(names(v08_lith), 
    "depth"))]]
names(v08_lith) <- str_remove_all(names(v08_lith), "_depth")
v08_lith <- v08_lith[[which(str_detect(names(v08_lith), 
    "sedim|water|ice", negate = TRUE))]]
names(v08_lith)[1] <- "asthenospheric_mantle"
v08_lith$lid <- v08_lith$lid_bottom - v08_lith$lid_top
v08_lith$lower_crust <- v08_lith$lower_crust_bottom - v08_lith$lower_crust_top
v08_lith$middle_crust <- v08_lith$middle_crust_bottom - v08_lith$middle_crust_top
v08_lith$upper_crust <- v08_lith$upper_crust_bottom - v08_lith$upper_crust_top
v08_lith <- v08_lith[[which(str_detect(names(v08_lith), 
    "top|bottom|lid", negate = TRUE))]]


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
    # # `[`(1:2) %>% 
    # map(~list(rst = .x, pts = pts)) %>% 
    mclapply(
        function(x){
            files <- x$rst
            # pts <- x$pts
            terra::rast(paste0("rasters/10_NASA/", files$dd, "/", files$ff))
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
v11_gpob.pop <- terra::rast("rasters/11_landscan-global-population/landscan-global-2024.tif")
v11_gpob.pop[v11_gpob.pop > 500] <- 500
# plot(v11_gpob.pop)

# 12. Open Land map -------------------------------------------------------------------------
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
            terra::rast(paste0("rasters/12_openlandmap/", x))
        }, 
        mc.cores = 15
    ) 
# %>% 
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
files <- dir("rasters/13_hwsd_1.2/", ".asc")

v13_HWSD <- lapply(paste0("rasters/13_hwsd_1.2/", files), rast) %>% 
    terra::rast()

# prepare -----------------------------------------------------------------
plot(v01_hfp)
which(str_detect(names(v03_emam), "rel.abundance"))
plot(v03_emam[[c(5, 13)]])

`&`(
    str_detect(names(v04_soilgrids), "0-5cm"),
    str_detect(names(v04_soilgrids), "wv|ocd|cfvo", negate = TRUE)
) %>% 
    which %>% 
    `[[`(v04_soilgrids, .) %>% 
    plot()
v05_ndvi_mean <- terra::app(v05_ndvi, fun = "mean", na.rm = TRUE)
plot(v05_ndvi_mean)
plot(v08_lith)
plot(v11_gpob.pop)

str_detect(names(v13_HWSD), "GRS_2000|sq4") %>% 
    which() %>% 
    `[[`(v13_HWSD, .) %>% 
    `names<-`(c("HWSD grass_woodland", "HWSD oxygen availability to roots")) %>% 
    plot()

# plot --------------------------------------------------------------------
final <- lst(v01_hfp, 
    v03_emam[[5]],
    v03_emam[[13]],
    `&`(
        str_detect(names(v04_soilgrids), "0-5cm"),
        str_detect(names(v04_soilgrids), "wv|ocd|cfvo", negate = TRUE)
    ) %>% 
        which %>% 
        `[[`(v04_soilgrids, .) %>% 
        as.list(),
    v05_ndvi_mean,
    v08_lith %>% 
        as.list(), 
    str_detect(names(v13_HWSD), "GRS_2000|sq4") %>% 
        which() %>% 
        `[[`(v13_HWSD, .) %>% 
        `names<-`(c("HWSD grass_woodland", "HWSD oxygen availability to roots")) %>% 
        as.list()
    )
final2 <- append(
    final[!sapply(final, is.list)],
    final[sapply(final, is.list)] %>% flatten()
)

plot_rst <- function(i){
    png(paste0("export/raster_", i, ".png"), width = 1350, height = 600, res = 120)
    plot.new()
    plot(final2[[i]], main = names(final2[[i]]))
    # text(x = 0, y = 1, labels = names(final2[[i]]), adj = c(0, 1), font = 1, family = "mono", cex = 0.8)
    dev.off()
    cat(i)
}

for(j in 1:18){
    plot_rst(j)
}


final <- lapply(final, function(r) {
    terra::resample(r, template, method = "near")
})





final2 <- lapply(final, function(r) {
    terra::project(r, template, method = "near")
})

list(
    
    
)







final3 <- terra::rast(final2)
names(final3) <- map(final, names) %>% flatten_chr()
plot(final3)
