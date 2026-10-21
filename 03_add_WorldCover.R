# 3.1.A. points loading ----------------------------------------------------------
cat("\nTask 03 started...\n")
cat("\nTypically takes 18 sec\n")
cat("\nSubtask 3.1 (data loading) started\n");
time1 <- time0 <- Sys.time()
suppressMessages({
    library(sf)
    library(parallel)
    library(tidyverse)
})

check <- character()
path <- stringr::str_subset(dir("data", pattern = "RData"), "random-points")
if(length(path)<1){
    check <- "Warning! There is no random points data"
} else {
    load(paste0("data/", sort(path, decreasing = TRUE)[1]))
}
path <- stringr::str_subset(dir("data", pattern = "RData"), "EW-NM-points_2")
if(length(path)<1){
    check <- "Warning! There is no earthworms (EW) and nematodes (NM) points data"
} else {
    load(paste0("data/", sort(path, decreasing = TRUE)[1]))
}


if(length(check>0)){
    for(i in 1:length(check)){
        cli::cli_warn(check[i])
    }
    cli::cli_abort("Run earlier scripts, please")
} else {
    rm(path, check)
    cat("Points have been loaded\n")
}

# EW_NM_points <- sample_n(EW_NM_points, 50)
# random_points <- sample_n(random_points, 50)

random_points$id <- 1:nrow(random_points)
EW_NM_points$id <- 1:nrow(EW_NM_points)

wc_table <- data.frame(
    wc = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100), 
    worldcover = c("Tree cover", "Shrubland", "Grassland",
                   "Cropland", "Built up", "Bare sparse vegetation",
                   "Snow and Ice", "Permanent water bodies",
                   "Herbaceous wetland", "Mangroves", "Moss and lichen")
)

# 3.1.B. WorldCover --------------------------------------------------------------
wc_files <- dir("rasters/00_worldcover", pattern = ".tif")
cat("\nProcessing WorldCover...\n")
w <- mclapply(
    wc_files, 
    FUN = function(a){
        raster::raster(paste0("rasters/00_worldcover/", a))
    },
    mc.cores = 16
    )

cat("\nSubtask 3.1 (data loading) finished:\n"); Sys.time() - time1; cat("\n")

# 3.2. Extraction ---------------------------------------------------------
cat("\nSubtask 3.2 (values extraction) started\n")
time1 <- Sys.time()
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
cat("\nSubtask 3.2 (values extraction) finished:\n"); Sys.time() - time1; cat("\n")

# 3.3. Export ------------------------------------------------------------------
cat("\nSubtask 3.3 (export) started\n")
time1 <- Sys.time()

result <- purrr::transpose(result)
result_rnd  <- purrr::map_dfr(result$rnd, rbind)
random_points <- dplyr::left_join(random_points, result_rnd, by = "id")
random_points <- dplyr::left_join(random_points, wc_table, by = "wc")
# dplyr::filter(random_points, !is.na(wc))
result_ewnm  <- purrr::map_dfr(result$ewnm, rbind)
EW_NM_points <- dplyr::left_join(EW_NM_points, result_ewnm, by = "id")
EW_NM_points <- dplyr::left_join(EW_NM_points, wc_table, by = "wc")
# dplyr::filter(EW_NM_points, !is.na(wc))

EW_NM_points$id <- NULL
random_points$id <- NULL
path <- Sys.time() |>
    format("%Y-%m-%d") |>
    as.character()
    # stringr::str_split_1("\\.") |>
    # `[`(1) |>
    # stringr::str_replace_all( ":", "-")
save(
    list = c("EW_NM_points", "random_points"), 
    file = paste0("data/EW-NM-random-points_WorldCover_", path, ".RData")
    )

random_points <- random_points %>% 
    filter(worldcover %in% c(
        "Bare sparse vegetation", "Cropland", "Grassland", 
        "Shrubland","Herbaceous wetland", "Moss and lichen", "Tree cover"
    ))
EW_NM_points <- EW_NM_points %>% 
    filter(worldcover %in% c(
        "Bare sparse vegetation", "Cropland", "Grassland", 
        "Shrubland","Herbaceous wetland", "Moss and lichen", "Tree cover"
    ))
save(
    list = c("EW_NM_points", "random_points"), 
    file = paste0("data/EW-NM-random-points_final_", path, ".RData")
)

worldcover_statistics <- lst(random_points, EW_NM_points) %>% 
    map_dfr(~count(st_drop_geometry(.x), worldcover), .id = "tp") %>% 
    pivot_wider(names_from = tp, values_from = n, values_fill = 0) %>% 
    arrange(worldcover) %>% 
    mutate(worldcover = str_replace_all(worldcover, "\\.", " "))

print(worldcover_statistics)
writexl::write_xlsx(worldcover_statistics, paste0("export/worldcover_statistics_", Sys.Date(), ".xlsx"))

p1 <- worldcover_statistics %>% 
    filter(worldcover %in% c(
        "Bare sparse vegetation", "Cropland", "Grassland", 
        "Shrubland","Herbaceous wetland", "Moss and lichen", "Tree cover"
    )) %>% 
    pivot_longer(names_to = "dataset", values_to = "N", -worldcover) %>% 
    group_by(dataset) %>% 
    mutate(N = N/sum(N)) %>% 
    ungroup() %>% 
    ggplot(aes(x = "", y = N, fill = worldcover)) +
    geom_col(width = 1, color = "white") + 
    coord_polar(theta = "y", start = 0) + 
    facet_wrap(~dataset) + 
    labs(fill = NULL) + 
    scale_fill_brewer(palette = "Set3") +
    theme_void() + 
    theme(legend.position = "bottom")

if(interactive()){plot(p1)} else {
    ggsave(
        plot = p1,
        filename = paste0("export/pts_piechart_", Sys.Date(), ".svg"),
        width = 15, 
        height = 10,
        units = "cm"
    )
} 

nater_terrain <- st_read("vectors/naturalearth/ne_50m_land.shp", quiet = TRUE)
p2 <- random_points %>% 
    filter(worldcover %in% c(
        "Bare sparse vegetation", "Cropland", "Grassland", 
        "Shrubland","Herbaceous wetland", "Moss and lichen", "Tree cover"
    )) %>% 
    transmute( wc, worldcover, 
        Dataset = "Random \npoints", 
        alp = 0.3) %>% 
    rbind(transmute(EW_NM_points, wc, worldcover, 
        Dataset = "Earthworms & Nematodes \npoints",
        alp = 1
    )) %>% 
ggplot() + 
    geom_sf(data = nater_terrain, fill = "lightgrey")+ # data = trn,
    geom_sf(mapping = aes(color = Dataset, alpha = alp), key_glyph = "point") + 
    # geom_sf(
    #     data = filter(random_points, worldcover %in% c(
    #         "Bare sparse vegetation", "Cropland", "Grassland", 
    #         "Shrubland","Herbaceous wetland", "Moss and lichen", "Tree cover"
    #     )),
    #     shape = 1, 
    #     color = "red", 
    #     alpha = 0.3) +
    # geom_sf(
    #     data = EW_NM_points,
    #     color = "darkgreen", 
    #     fill = "darkgreen") +
    scale_colour_manual(values = c("coral", "darkgreen")) + 
    theme_bw() + 
    guides(alpha = "none") + 
    theme(
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.position = "bottom",
        legend.key = element_blank(), 
        panel.background = element_rect(
            fill = "lightblue"
        )
    )

if(interactive()){plot(p2)} else {
    ggsave(
        paste0("export/points_terrain_", Sys.Date(), ".png"), 
        p2, 
        height = 210*0.63, 
        width = 297*0.8,
        units = "mm", 
        dpi = 230
    )
}

cat("\nSubtask 3.3 (export) finished:\n"); Sys.time() - time1; cat("\n")
cat("\nTask 03 finished:\n"); Sys.time() - time0; cat("\n")
