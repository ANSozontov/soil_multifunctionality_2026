# points loading ----------------------------------------------------------
cat("\nTask 3 started...\n")
my_time <- Sys.time()
suppressMessages({
    library(sf)
    library(parallel)
})

wc_table <- data.frame(
    wc = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100), 
    worldcover = c("Tree.cover", "Shrubland", "Grassland",
                   "Cropland", "Built.up", "Bare.sparse.vegetation",
                   "Snow.and.Ice", "Permanent.water.bodies",
                   "Herbaceous.wetland", "Mangroves", "Moss.and.lichen")
)

check <- character()
path <- stringr::str_subset(dir(pattern = "RData"), "random-points")
if(length(path)<1){
    check <- "Warning! There is no random points data"
} else {
    load(sort(path, decreasing = TRUE)[1])
}
path <- stringr::str_subset(dir(pattern = "RData"), "EW-NM-points")
if(length(path)<1){
    check <- c(check, "Warning! There is no earthworms&nematodes points")
} else {
    load(sort(path, decreasing = TRUE)[1])
}
if(length(check>0)){
    for(i in 1:length(check)){
        cli::cli_warn(check[i])
    }
    cli::cli_abort("Run earlier scripts, please")
} else {
    cat("Points have been loaded\n")
}

# EW_NM_points <- sample_n(EW_NM_points, 50)
# random_points <- sample_n(random_points, 50)

random_points$id <- 1:nrow(random_points)
EW_NM_points$id <- 1:nrow(EW_NM_points)
rm(check, path)

# WorldCover --------------------------------------------------------------
wc_files <- dir("rasters/00_worldcover", pattern = ".tif")
cat("\nProcessing WorldCover...\n")
w <- mclapply(
    wc_files, 
    FUN = function(a){
        raster::raster(paste0("rasters/00_worldcover/", a))
    },
    mc.cores = 16
    )

cat("\nSubtask 1 (rasters loading) finished:\n"); Sys.time() - my_time; cat("\n")
my_time1 <- Sys.time()

result <- mclapply(
    1:length(w),
    # 1070:1170,
    FUN = function(i){
        bb <- raster::bbox(w[[i]])
        
        new_rnd <- random_points %>% 
            dplyr::filter(
                x >= bb[1,1], x <= bb[1,2], 
                y >= bb[2,1], y <= bb[2,2])
        if(nrow(new_rnd)>0){
            new_rnd <- data.frame(
                id = new_rnd$id, 
                wc = raster::extract(w[[i]], new_rnd)
            )
        } else {
            new_rnd <- data.frame(
                id = integer(),
                wc = numeric()
            )
        }
        
        new_ewnm <- EW_NM_points %>% 
            dplyr::filter(
                lon >= bb[1,1], lon <= bb[1,2], 
                lat >= bb[2,1], lat <= bb[2,2])
        if(nrow(new_ewnm)>0){
            new_ewnm <- data.frame(
                id = new_ewnm$id, 
                wc = raster::extract(w[[i]], new_ewnm)
            )
        } else {
            new_ewnm <- data.frame(
                id = integer(),
                wc = numeric()
            )
        }
        list(ewnm = new_ewnm, rnd = new_rnd)
    },
    mc.cores = 16
)
cat("\nSubtask 2 (rasters values extraction) finished:\n"); Sys.time() - my_time1; cat("\n")

result <- purrr::transpose(result)
result_rnd  <- purrr::map_dfr(result$rnd, rbind)
random_points <- dplyr::left_join(random_points, result_rnd, by = "id")
random_points <- dplyr::left_join(random_points, wc_table, by = "wc")
# dplyr::filter(random_points, !is.na(wc))
result_ewnm  <- purrr::map_dfr(result$ewnm, rbind)
EW_NM_points <- dplyr::left_join(EW_NM_points, result_ewnm, by = "id")
EW_NM_points <- dplyr::left_join(EW_NM_points, wc_table, by = "wc")
# dplyr::filter(EW_NM_points, !is.na(wc))

# export ------------------------------------------------------------------
EW_NM_points$id <- NULL
random_points$id <- NULL
path <- Sys.time() |>
    format("%Y-%m-%d") |>
    as.character() |>
    stringr::str_split_1("\\.")
path <- path[1] |>
    stringr::str_replace_all( ":", "-")
save(
    list = c("EW_NM_points", "random_points"), 
    file = paste0("data_pt1_worldcover_", path, ".RData")
    )

cat("\nTask 3 finished:\n"); Sys.time() - my_time; cat("\n")
