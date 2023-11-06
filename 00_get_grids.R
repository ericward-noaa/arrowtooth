# get prediction grids
remotes::install_github("pbs-assess/gfplot")
remotes::install_github("pfmc-assessments/nwfscSurvey")
#remotes::install_github("afsc-gap-products/akgfmaps")
remotes::install_github("afsc-gap-products/gapindex")

nwfsc_grid <- nwfscSurvey::availablecells %>%
  dplyr::select(Cent.ID, Cent.Long, Cent.Lat, Hectares) %>%
  dplyr::rename(lon_dd = Cent.Long, lat_dd = Cent.Lat)
grid <- expand.grid("Cent.ID" = unique(nwfsc_grid$Cent.ID),
                    year = seq(min(d_nwfsc$year),max(d_nwfsc$year)))
grid_nwfsc <- dplyr::left_join(grid, nwfsc_grid)
grid_nwfsc <- add_utm_columns(grid_nwfsc, ll_names = c("lon_dd","lat_dd")) # UTM zone 10N; CRS = 32610
saveRDS(grid_nwfsc, "grid_nwfsc.rds")

url <- "https://raw.githubusercontent.com/afsc-gap-products/model-based-indices/main/extrapolation_grids/GOAThorsonGrid_Less700m.csv"
goa_grid <- read.csv(url)
names(goa_grid) <- tolower(names(goa_grid))
grid <- expand.grid("id" = unique(goa_grid$id),
                    year = seq(min(d_nwfsc$year),max(d_nwfsc$year)))
grid_goa <- dplyr::left_join(grid, goa_grid)
grid_goa <- add_utm_columns(grid_goa, ll_names = c("longitude","latitude"), utm_crs = 32610) # UTM zone 10N; CRS = 32610
grid_goa$shape_area_ha <- grid_goa$shape_area / 10000 # was in sq m
saveRDS(grid_goa, "grid_goa.rds")

pbs_grid <- gfplot::synoptic_grid
pbs_grid$X <- pbs_grid$X * 1000
pbs_grid$Y <- pbs_grid$Y* 1000
points_sf <- sf::st_as_sf(pbs_grid, coords = c("X", "Y"), crs = 32609) # UTM zone 9N is EPSG:32609
points_sf_zone10 <- sf::st_transform(points_sf, 32610)
coords <- sf::st_coordinates(points_sf_zone10)
pbs_grid <- data.frame(X = coords[, "X"]/1000, Y = coords[, "Y"]/1000, ID = seq(1, nrow(coords)),
                       area = gfplot::synoptic_grid$cell_area)
grid <- expand.grid("ID" = unique(pbs_grid$ID),
                    year = seq(min(d_pbs$year),max(d_pbs$year)))
grid_pbs <- dplyr::left_join(grid, pbs_grid)
grid_pbs$shape_area_ha <- grid_pbs$area * 100 # was in km2
saveRDS(grid_pbs, "grid_pbs.rds")

