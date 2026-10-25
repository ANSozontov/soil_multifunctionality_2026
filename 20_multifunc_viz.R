# proof if load is faster than excel read: 
#     s <- Sys.time()
#     load("export/4data_extracted_2026-10-21.RData")
#     Sys.time() - s
#     
#     s <- Sys.time()
#     d1 <- read_excel("export/4data_extracted_2026-10-21.xlsx", sheet = 1)
#     d2 <- read_excel("export/4data_extracted_2026-10-21.xlsx", sheet = 2)
#     Sys.time() - s