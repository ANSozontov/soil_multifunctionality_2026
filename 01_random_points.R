cat("\nTask 1 started...\n") 
my_time <- Sys.time()
suppressPackageStartupMessages({
    library(sf)
    library(dplyr)    
})

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

check <- st_intersects(dots, nater_terrain, sparse = FALSE)

random_points <- dots[apply(check, 1, sum) > 0,]

save("random_points", file = paste0("random-points_terrain_", Sys.Date(), ".RData"))

library(ggplot2)
p <- ggplot() + 
    geom_sf(data = nater_terrain, fill = "lightgrey")+ # data = trn,
    geom_sf(
        data = random_points,
        shape = 1, 
        color = "red", 
        alpha = 0.3) +
    theme_bw() + 
    theme(
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.position = "bottom",
        panel.background = element_rect(
            fill = "lightblue"
        )
    )

ggsave(
    paste0("export/random-points_terrain_", Sys.Date(), ".pdf"), 
    p, 
    height = 210*2, 
    width = 297*2,
    units = "mm", 
    dpi = 150
)

cat("\nTask 1 finished:\n"); Sys.time() - my_time
