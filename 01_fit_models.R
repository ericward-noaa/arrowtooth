library(surveyjoin)
library(sdmTMB)
library(dplyr)

d <- get_data(common = "arrowtooth flounder")

# filter to just include WCBTS / PBS / GOA surveys
d <- dplyr::filter(d, survey_name %in% c("Gulf of Alaska Bottom Trawl Survey",
                                         "NWFSC.Combo",
                                         "SYN HS","SYN QCS","SYN WCHG","SYN WCVI"),
                   year >= 2003)
d$cpue <- d$catch_weight / d$effort # effort in ha

# fit joint model to all regions
d <- dplyr::filter(d, !is.na(lon_start), !is.na(lat_start)) %>%
  add_utm_columns(ll_names = c("lon_start", "lat_start"))
cutoff_distance_km <- 25
mesh_all <- make_mesh(d, c("X","Y"), cutoff=cutoff_distance_km) # 938vertices

fit_all <- sdmTMB(cpue ~ 1,
                  spatial_varying = ~ region,
                  spatial="on",
                  spatiotemporal = "rw", # mean of ar1 field allowed to be !=0
                  family = tweedie(),
                  time="year",
                  data = d,
                  mesh=mesh_all
                  )
saveRDS(fit_all, "fit_all.rds")


# fit single model to all regions
d_nwfsc <- dplyr::filter(d, region=="nwfsc")
mesh_nwfsc <- make_mesh(d_nwfsc, c("X","Y"), cutoff=cutoff_distance_km) # 391 vertices

fit_nwfsc <- sdmTMB(cpue ~ 1,
                  #spatial_varying = ~ region,
                  spatial="on",
                  spatiotemporal = "rw", # mean of ar1 field allowed to be !=0
                  family = tweedie(),
                  data = d_nwfsc,
                  time="year",
                  mesh=mesh_nwfsc,extra_time = c(2020))
saveRDS(fit_nwfsc, "fit_nwfsc.rds")

d_pbs <- dplyr::filter(d, region=="pbs")
mesh_pbs <- make_mesh(d_pbs, c("X","Y"), cutoff=cutoff_distance_km) # 209 vertices

fit_pbs <- sdmTMB(cpue ~ 1,
                    #spatial_varying = ~ region,
                    spatial="on",
                    spatiotemporal = "rw", # mean of ar1 field allowed to be !=0
                    family = tweedie(),
                  time = "year",
                    data = d_pbs,
                    mesh=mesh_pbs)
saveRDS(fit_pbs, "fit_pbs.rds")

d_afsc <- dplyr::filter(d, region=="afsc")
mesh_afsc <- make_mesh(d_afsc, c("X","Y"), cutoff=cutoff_distance_km) # 742 vertices

fit_afsc <- sdmTMB(cpue ~ 1,
                  #spatial_varying = ~ region,
                  spatial="on",
                  spatiotemporal = "rw", # mean of ar1 field allowed to be !=0
                  family = tweedie(),
                  data = d_afsc,
                  time = "year",
                  mesh=mesh_afsc,
                  extra_time = c(2004,2006,2008,2010,2012,2014,2016,2018,2020))
saveRDS(fit_afsc, "fit_afsc.rds")
# Look at convergence. All pass necessary warnings. Spatial variance -> 0 in PBS but
# is positive in others.
# sanity(fit_pbs)
# sanity(fit_nwfsc)
# sanity(fit_afsc)

# make predictions for AK
grid_afsc <- readRDS("grid_goa.rds")
pred_afsc <- predict(fit_afsc, grid_afsc, return_tmb_object = TRUE)
afsc_index <- get_index(pred_afsc, bias_correct = TRUE, area = grid_afsc$shape_area_ha)
saveRDS(afsc_index, "afsc_index.rds")

grid_afsc$region <- "afsc"
grid_afsc$region <- factor(grid_afsc$region, levels = c("afsc", "nwfsc", "pbs"))
pred_afsc_all <- predict(fit_all, grid_afsc, return_tmb_object = TRUE)
afsc_index_all <- get_index(pred_afsc_all, bias_correct = TRUE, area = grid_afsc$shape_area_ha)
saveRDS(afsc_index_all, "afsc_index_all.rds")

# Make prediction for NWFSC survey region
grid_nwfsc <- readRDS("grid_nwfsc.rds")
pred_nwfsc <- predict(fit_nwfsc, grid_nwfsc, return_tmb_object = TRUE)
nwfsc_index <- get_index(pred_nwfsc, bias_correct = TRUE, area = pred_nwfsc$data$Hectares)
saveRDS(nwfsc_index, "nwfsc_index.rds")

grid_nwfsc$region <- "nwfsc"
grid_nwfsc$region <- factor(grid_nwfsc$region, levels = c("afsc", "nwfsc", "pbs"))
pred_nwfsc_all <- predict(fit_all, grid_nwfsc, return_tmb_object = TRUE)
nwfsc_index_all <- get_index(pred_nwfsc_all, bias_correct = TRUE, area = grid_nwfsc$Hectares)
saveRDS(nwfsc_index_all, "nwfsc_index_all.rds")

# Make prediction for BC survey region
grid_pbs <- readRDS("grid_pbs.rds")
pred_pbs <- predict(fit_pbs, grid_pbs, return_tmb_object = TRUE)
pbs_index <- get_index(pred_pbs, bias_correct = TRUE, area = grid_pbs$shape_area_ha) # area is 4 for all cells
saveRDS(pbs_index, "pbs_index.rds")

grid_pbs$region <- "pbs"
grid_pbs$region <- factor(grid_pbs$region, levels = c("afsc", "nwfsc", "pbs"))
pred_pbs_all <- predict(fit_all, grid_pbs, return_tmb_object = TRUE)
#area <- rep(4, nrow(grid_pbs))
pbs_index_all <- get_index(pred_pbs_all, bias_correct = TRUE, area = grid_pbs$shape_area_ha)
saveRDS(pbs_index_all, "pbs_index_all.rds")

