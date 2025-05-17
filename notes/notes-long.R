
#sf::st_write(slope_leakage_mask, "./data/MASK/slope_leakage_mask.shp", delete_dsn=T)
#sf::st_write(wetlands_leakage_mask, "./data/MASK/wetlands_leakage_mask.shp", delete_dsn=T)

tmap::tm_shape(wetlands) + 
    tm_raster(col.legend = tm_legend(title = "Wetlands"))

roads_mask         = sf::st_read("./data/MASK/roads_mask.shp")
slope_mask = sf::st_read("./data/MASK/slope_leakage_mask.shp")
wetlands_mask = sf::st_read("./data/MASK/wetlands_leakage_mask.shp")


sf::st_write(leakage_mask_a, "./data/MASK/leakage_mask_a.shp", delete_dsn=T)

sf::st_write(leakage_mask_b, "./data/MASK/leakage_mask_b.shp", delete_dsn=T)


tmap::tm_shape(bbox_aoi_1) + tmap::tm_borders(lwd=0) +
  tmap::tm_shape(wetlands_mask) + tmap::tm_raster("label",col.scale=tm_scale_categorical(), title="") +
  tmap::tm_shape(slope_mask) + tmap::tm_polygons(col="violet",fill="violet",fill_alpha=1,lwd=1) + 
  tmap::tm_shape(aoi) + tmap::tm_borders(col="red") +
  tmap::tm_add_legend(type="lines",col="yellow",labels="Leakage Belt (7.5km)") +  
  tmap::tm_shape(roads) + tmap::tm_lines(col="darkorange", lwd=1) + 
  tmap::tm_add_legend(type="lines", col="orange", labels = "Roads") +
  tmap::tm_shape(waterways) + tmap::tm_lines(col = "turquoise", lwd = 0.5) +
  tmap::tm_add_legend(type="lines", col="turquoise", labels = "Waterways") +
  tmap::tm_shape(counties) + tmap::tm_borders(lwd = 1, col="grey") +
  tmap::tm_add_legend(type="lines", col="grey", labels = "County Border") +  
  tmap::tm_shape(aoi) + tm_borders(col="red", lwd=2.5) + 
  tmap::tm_add_legend(type="lines", col="red", labels = "Project Area: UDef PA") +
  tmap::tm_shape(places) + tm_symbols(size=0.35,fill="pink", shape.scale = tm_scale_asis()) +
  tmap::tm_add_legend(type="symbols", col="purple", fill="purple", size=0.6,labels="Communities") +
  tmap::tm_layout(legend.position=c("left", "top"), legend.bg.color = "white", legend.text.size=0.1) +
  tmap::tm_basemap("Esri.WorldImagery")


tmap::tm_shape(aoi) + tmap::tm_borders(lwd=0) +
  tmap::tm_shape(wetlands) + tmap::tm_raster("label", col.scale = tm_scale_categorical(), title="")
  
tmap::tm_shape(wetlands) + 
    tm_raster(col.legend = tm_legend(title = "Wetlands"))
  
  tmap::tm_shape(roads) + tmap::tm_lines(col="darkorange", lwd=1) + 
  tmap::tm_add_legend(type="lines", col="orange", labels = "Roads") +
  tmap::tm_shape(waterways) + tmap::tm_lines(col = "turquoise", lwd = 0.5) +
  tmap::tm_add_legend(type="lines", col="turquoise", labels = "Waterways") +
  tmap::tm_shape(counties) + tmap::tm_borders(lwd = 1, col="grey") +
  tmap::tm_add_legend(type="lines", col="grey", labels = "County Border") +  
  tmap::tm_shape(aoi) + tm_borders(col="red", lwd=2.5) + 
  tmap::tm_add_legend(type="lines", col="red", labels = "Project Area: UDef PA") +
  tmap::tm_shape(places) + tm_symbols(size=0.35,fill="pink", shape.scale = tm_scale_asis()) +
  tmap::tm_add_legend(type="symbols", col="purple", fill="purple", size=0.6,labels="Communities") +
  tmap::tm_layout(legend.position=c("left", "top"), legend.bg.color = "white", legend.text.size=0.1) +
  tmap::tm_basemap("Esri.WorldImagery")

### Environment setup
tmap::tmap_options(max.raster = c(plot = 100000000, view = 100000000)) # expand tmap memory
base::mem.maxVSize(vsize = 100000000) # 1TB also manually paste to .Renviron config file in home directory

### PDF format
---
  title: "REDD+ Feasability Assessment"
subtitle: "Review of emissions estimates & activity data used in the RSPB Gola feasibility assessment"
date: "2024-12-23"
output:
  pdf_document:
  toc: TRUE
toc_depth: 3
pandoc_args: --listings
includes:
  in_header: preamble.tex

always_allow_html: TRUE
bibliography: references.bib
df-print: kable
---

### WORD format

output: 
  word_document:
  keep_md: TRUE
  toc: FALSE

always_allow_html: true


### HTML format

output:
  html_document:
    self_contained: yes
    mode: selfcontained

## tmap layout

tmap::tm_shape(RGB_2014) + tmap::tm_raster(title = "RGB, 2014") +
  tmap::tm_shape(country) + tmap::tm_borders(col = "blue", lwd = 2) +
  tmap::tm_shape(aoi) + tmap::tm_borders(col = "red", lwd = 1) +
  tmap::tm_basemap("Esri.WorldImagery") +  
  tmap::tm_compass(position = c("left", "bottom")) +
  tmap::tm_scale_bar() -> tm3

tmap::tm_shape(RGB_2019) + tmap::tm_raster(title = "RGB, 2019") +
  tmap::tm_shape(country) + tmap::tm_borders(col = "blue", lwd = 2) +
  tmap::tm_shape(aoi) + tmap::tm_borders(col = "red", lwd = 1) +
  tmap::tm_basemap("Esri.WorldImagery") +  
  tmap::tm_compass(position = c("left", "bottom")) +
  tmap::tm_scale_bar() -> tm4

tmap::tm_shape(RGB_2024) + tmap::tm_raster(title = "RGB, 2024") +
  tmap::tm_shape(country) + tmap::tm_borders(col = "blue", lwd = 2) +
  tmap::tm_shape(aoi) + tmap::tm_borders(col = "red", lwd = 1) +
  tmap::tm_basemap("Esri.WorldImagery") +  
  tmap::tm_compass(position = c("left", "bottom")) +
  tmap::tm_scale_bar() -> tm5



### fig layout

![Figure 1: Site map showing proposed project extent as defined by `ProjectArea.shp`](data/site_maps/site-map.png)

![Figure 2: Locator map showing position of project area within national borders](data/site_maps/locator-map.png)




# raster cleaning
BLUE_2014_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_BLUE_2014-01-04.tif")
GREEN_2014_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_GREEN_2014-01-04.tif")
NIR08_2014_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_NIR08_2014-01-04.tif")
RED_2014_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_RED_2014-01-04.tif")
SWIR16_2014_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_SWIR16_2014-01-04.tif")
SWIR22_2014_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_SWIR22_2014-01-04.tif")

BLUE_2019_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_BLUE_2019-01-02.tif")
GREEN_2019_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_GREEN_2019-01-02.tif")
NIR08_2019_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_NIR08_2019-01-02.tif")
RED_2019_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_RED_2019-01-02.tif")
SWIR16_2019_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_SWIR16_2019-01-02.tif")
SWIR22_2019_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_SWIR22_2019-01-02.tif")

BLUE_2024_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_BLUE_2024-01-16.tif")
GREEN_2024_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_GREEN_2024-01-16.tif")
NIR08_2024_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_NIR08_2024-01-16.tif")
RED_2024_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_RED_2024-01-16.tif")
SWIR16_2024_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_SWIR16_2024-01-16.tif")
SWIR22_2024_rast = terra::rast("./data/cube_gee/LANDSAT_TM-ETM-OLI_198055_SWIR22_2024-01-16.tif")

NDVI_2014_rast = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04.tif")
NDVI_2019_rast = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02.tif")
NDVI_2024_rast = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16.tif")


sits_crs = terra::crs("EPSG:32629")

terra::project(NDVI_2014_rast, sits_crs)
terra::project(NDVI_2019_rast, sits_crs)
terra::project(NDVI_2024_rast, sits_crs)

writeRaster(NDVI_2014_rast, "./data/cube_2014/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04.tif")
writeRaster(NDVI_2019_rast, "./data/cube_2019/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02.tif")
writeRaster(NDVI_2024_rast, "./data/cube_2024/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16.tif")

NDVI_STACK = c(NDVI_2014, NDVI_2019, NDVI_2024)
NDVI_2014_filled <- focal(NDVI_2014, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
NDVI_2019_filled <- focal(NDVI_2019, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
NDVI_2024_filled <- focal(NDVI_2024, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
writeRaster(NDVI_2014_filled, "./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_FILLED.tif")
writeRaster(NDVI_2019_filled, "./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_FILLED.tif")
writeRaster(NDVI_2024_filled, "./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_FILLED.tif")

NDVI_2014_filled = rast("./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_FILLED.tif")
NDVI_2019_filled = rast("./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_FILLED.tif")
NDVI_2024_filled = rast("./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_FILLED.tif")
writeRaster(NDVI_2014_filled, "./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_FILLED.tif")
writeRaster(NDVI_2019_filled, "./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_FILLED.tif")
writeRaster(NDVI_2024_filled, "./data/cube_filled/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_FILLED.tif")

NDVI_2014_3857 = terra::project(NDVI_2014_filled, "EPSG:3857")
NDVI_2019_3857 = terra::project(NDVI_2019_filled, "EPSG:3857")
NDVI_2024_3857 = terra::project(NDVI_2024_filled, "EPSG:3857")
NDVI_2014_3857 = terra::resample(NDVI_2014_3857, NDVI_2024_3857)
NDVI_2019_3857 = terra::resample(NDVI_2019_3857, NDVI_2024_3857)
NDVI_2024_3857 = terra::resample(NDVI_2024_3857, NDVI_2019_3857)

writeRaster(NDVI_2014_3857, "./data/cube_resample/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_3857.tif")
writeRaster(NDVI_2019_3857, "./data/cube_resample/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_3857.tif")
writeRaster(NDVI_2024_3857, "./data/cube_resample/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_3857.tif")
NDVI_2014_3857 = terra::rast("./data/cube_resample/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_3857.tif")
NDVI_2019_3857 = terra::rast("./data/cube_resample/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_3857.tif")
NDVI_2024_3857 = terra::rast("./data/cube_resample/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_3857.tif")

grid_size <- res(NDVI_2024_3857)  # Grid resolution in x and y
grid_origin <- c(ext(NDVI_2024_3857)[1], ext(NDVI_2024_3857)[3])  # xmin, ymin of raster

samples_country  = sf::read_sf("./data/training_samples/glance_spatial_clip.shp")
samples_3857 <- st_transform(samples_country, crs = "EPSG:3857")
samples_snapped <- st_snap_to_grid(
  x = samples_3857,     # Reprojected sf object
  size = grid_size,     # Grid resolution
  origin = grid_origin  # Grid origin
  )

st_write(samples_snapped, "./data/training_samples/snapped_samples.shp", delete_dsn = TRUE)
samples <- st_read("./data/training_samples/snapped_samples.shp")# Replace with your samples file


plot(NDVI_2024_3857)
plot(st_geometry(samples_snapped), col = "red", add=T)


ndvi_cube <- sits::sits_cube(
  type = "GRIDDING", # Ensure the type is "gridded"
  source = "MPC",
  collection = "LANDSAT-C2-L2",
  satellite = "LANDSAT",
  sensor = "TM-ETM-OLI",
  tile = "200055",
  xmin = -11.5,
  xmax = -7.37,
  ymin = 4.16,
  ymax = 8.55,
  crs = "EPSG:4326",
  bands = c("NDVI"),
  file_info = <file_information_tibble>  # Fill with appropriate file info
)

NDVI_2014_raster = raster::raster("./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04.tif")
NDVI_2019_raster = raster::raster("./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02.tif")
NDVI_2024_raster = raster::raster("./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16.tif")

NDVI_STACK_3857 <- project(NDVI_STACK, "EPSG:3857")

view(ndvi_raster_3857)
NDVI_2014_rast = terra::rast("./data/NDVIs_4326/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04.tif")
NDVI_2019_rast = terra::rast("./data/NDVIs_4326/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02.tif")
NDVI_2024_rast = terra::rast("./data/NDVIs_4326/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16.tif")

NDVI_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-02.tif")

NDVI_2014_crop = terra::crop(NDVI_2014_rast, vect(country)) #EPSG:4326 x 2
NDVI_2019_crop = terra::crop(NDVI_2019_rast, vect(country))
NDVI_2024_crop = terra::crop(NDVI_2024_rast, vect(country))

writeRaster(NDVI_2014_crop, "./data/NDVIs_4326/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_4326.tif")
writeRaster(NDVI_2019_crop, "./data/NDVIs_4326/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_4326.tif")
writeRaster(NDVI_2024_crop, "./data/NDVIs_4326/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_4326.tif")


NDVI_2014_32629 = terra::project(NDVI_2014_crop, crs(NDVI_2024_test))
NDVI_2019_32629 = terra::project(NDVI_2019_crop, crs(NDVI_2024_test))
NDVI_2024_32629 = terra::project(NDVI_2024_crop, crs(NDVI_2024_test))

writeRaster(NDVI_2014_32629, "./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_32629.tif")
writeRaster(NDVI_2019_32629, "./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_32629.tif")
writeRaster(NDVI_2024_32629, "./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_32629.tif")

NDVI_2014_32629 = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_32629.tif")
NDVI_2019_32629 = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2019-01-02_32629.tif")
NDVI_2024_32629 = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16_32629.tif")

st_crs(training_samples_sf)
training_samples_4326 = st_transform(training_samples_sf, 4326)
st_crs(training_samples_4326)
training_samples_32692 = st_transform(training_samples_sf, 32629)
st_crs(training_samples_32692)


raster::dataType(NDVI_2014_raster)
raster::dataType(NDVI_2019_raster)
raster::dataType(NDVI_2024_raster)

terra::minmax(BLUE_2014_rast, compute=T)# terra works, raster package doesnt
terra::minmax(GREEN_2014_rast, compute=T)
terra::minmax(NIR08_2014_rast, compute=T)
terra::minmax(RED_2014_rast, compute=T)
terra::minmax(SWIR16_2014_rast, compute=T)
terra::minmax(SWIR22_2014_rast, compute=T)

terra::minmax(BLUE_2019_rast, compute=T)# terra works, raster package doesnt
terra::minmax(GREEN_2019_rast, compute=T)
terra::minmax(NIR08_2019_rast, compute=T)
terra::minmax(RED_2019_rast, compute=T)
terra::minmax(SWIR16_2019_rast, compute=T)
terra::minmax(SWIR22_2019_rast, compute=T)

terra::minmax(BLUE_2024_rast, compute=T)# terra works, raster package doesnt
terra::minmax(GREEN_2024_rast, compute=T)
terra::minmax(NIR08_2024_rast, compute=T)
terra::minmax(RED_2024_rast, compute=T)
terra::minmax(SWIR16_2024_rast, compute=T)
terra::minmax(SWIR22_2024_rast, compute=T)

terra::setMinMax(BLUE_2014_rast, force=T)
terra::setMinMax(GREEN_2014_rast, force=T)
terra::setMinMax(NIR08_2014_rast, force=T)
terra::setMinMax(RED_2014_rast, force=T)
terra::setMinMax(SWIR16_2014_rast, force=T)
terra::setMinMax(SWIR22_2014_rast, force=T)

terra::setMinMax(BLUE_2019_rast, force=T)
terra::setMinMax(GREEN_2019_rast, force=T)
terra::setMinMax(NIR08_2019_rast, force=T)
terra::setMinMax(RED_2019_rast, force=T)
terra::setMinMax(SWIR16_2019_rast, force=T)
terra::setMinMax(SWIR22_2019_rast, force=T)

terra::setMinMax(BLUE_2024_rast, force=T)
terra::setMinMax(GREEN_2024_rast, force=T)
terra::setMinMax(NIR08_2024_rast, force=T)
terra::setMinMax(RED_2024_rast, force=T)
terra::setMinMax(SWIR16_2024_rast, force=T)
terra::setMinMax(SWIR22_2024_rast, force=T)

maxv <- 32767
BLUE_2014_rast_INT2S <- round(BLUE_2014_rast * maxv)
GREEN_2014_rast_INT2S <- round(GREEN_2014_rast * maxv)
NIR08_2014_rast_INT2S <- round(NIR08_2014_rast * maxv)
RED_2014_rast_INT2S <- round(RED_2014_rast * maxv)
SWIR16_2014_rast_INT2S <- round(SWIR16_2014_rast * maxv)
SWIR22_2014_rast_INT2S <- round(SWIR22_2014_rast * maxv)

BLUE_2019_rast_INT2S <- round(BLUE_2019_rast * maxv)
GREEN_2019_rast_INT2S <- round(GREEN_2019_rast * maxv)
NIR08_2019_rast_INT2S <- round(NIR08_2019_rast * maxv)
RED_2019_rast_INT2S <- round(RED_2019_rast * maxv)
SWIR16_2019_rast_INT2S <- round(SWIR16_2019_rast * maxv)
SWIR22_2019_rast_INT2S <- round(SWIR22_2019_rast * maxv)

BLUE_2024_rast_INT2S <- round(BLUE_2024_rast * maxv)
GREEN_2024_rast_INT2S <- round(GREEN_2024_rast * maxv)
NIR08_2024_rast_INT2S <- round(NIR08_2024_rast * maxv)
RED_2024_rast_INT2S <- round(RED_2024_rast * maxv)
SWIR16_2024_rast_INT2S <- round(SWIR16_2024_rast * maxv)
SWIR22_2024_rast_INT2S <- round(SWIR22_2024_rast * maxv)

writeRaster(BLUE_2014_rast_INT2S, "./data/cube_2014/LANDSAT_TM-ETM-OLI_198055_BLUE_2014-01-04_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(GREEN_2014_rast_INT2S, "./data/cube_2014/LANDSAT_TM-ETM-OLI_198055_GREEN_2014-01-04_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(NIR08_2014_rast_INT2S, "./data/cube_2014/LANDSAT_TM-ETM-OLI_198055_NIR08_2014-01-04_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(RED_2014_rast_INT2S, "./data/cube_2014/LANDSAT_TM-ETM-OLI_198055_RED_2014-01-04_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(SWIR16_2014_rast_INT2S, "./data/cube_2014/LANDSAT_TM-ETM-OLI_198055_SWIR16_2014-01-04_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(SWIR22_2014_rast_INT2S, "./data/cube_2014/LANDSAT_TM-ETM-OLI_198055_SWIR22_2014-01-04_INT2S.tif", datatype='INT2S', overwrite=TRUE)

writeRaster(BLUE_2019_rast_INT2S, "./data/cube_2019/LANDSAT_TM-ETM-OLI_198055_BLUE_2019-01-02_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(GREEN_2019_rast_INT2S, "./data/cube_2019/LANDSAT_TM-ETM-OLI_198055_GREEN_2019-01-02_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(NIR08_2019_rast_INT2S, "./data/cube_2019/LANDSAT_TM-ETM-OLI_198055_NIR08_2019-01-02_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(RED_2019_rast_INT2S, "./data/cube_2019/LANDSAT_TM-ETM-OLI_198055_RED_2019-01-02_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(SWIR16_2019_rast_INT2S, "./data/cube_2019/LANDSAT_TM-ETM-OLI_198055_SWIR16_2019-01-02_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(SWIR22_2019_rast_INT2S, "./data/cube_2019/LANDSAT_TM-ETM-OLI_198055_SWIR22_2019-01-02_INT2S.tif", datatype='INT2S', overwrite=TRUE)

writeRaster(BLUE_2024_rast_INT2S, "./data/cube_2024/LANDSAT_TM-ETM-OLI_198055_BLUE_2024-01-16_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(GREEN_2024_rast_INT2S, "./data/cube_2024/LANDSAT_TM-ETM-OLI_198055_GREEN_2024-01-16_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(NIR08_2024_rast_INT2S, "./data/cube_2024/LANDSAT_TM-ETM-OLI_198055_NIR08_2024-01-16_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(RED_2024_rast_INT2S, "./data/cube_2024/LANDSAT_TM-ETM-OLI_198055_RED_2024-01-16_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(SWIR16_2024_rast_INT2S, "./data/cube_2024/LANDSAT_TM-ETM-OLI_198055_SWIR16_2024-01-16_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(SWIR22_2024_rast_INT2S, "./data/cube_2024/LANDSAT_TM-ETM-OLI_198055_SWIR22_2024-01-16_INT2S.tif", datatype='INT2S', overwrite=TRUE)



tmap::tm_shape(RGB_2014, raster.warp = FALSE) + tmap::tm_raster(title = "RGB, 2014") +
  tmap::tm_shape(country) + tmap::tm_borders(col = "blue", lwd = 2) +
  tmap::tm_shape(aoi) + tmap::tm_borders(col = "red", lwd = 1) +
  tmap::tm_basemap("Esri.WorldImagery") +  
  tmap::tm_compass(position = c("left", "bottom")) +
  tmap::tm_scale_bar() 
#-> tm6

tmap::tm_shape(NDVI_2014) + tmap::tm_raster(title = "NDVI, 2014") +
  tmap::tm_shape(NDVI_2019) + tmap::tm_raster(title = "NDVI, 2019") +
  tmap::tm_shape(NDVI_2024) + tmap::tm_raster(title = "NDVI, 2024") +
  tmap::tm_shape(country) + tmap::tm_borders(col = "blue", lwd = 2) +
  tmap::tm_shape(aoi) + tmap::tm_borders(col = "red", lwd = 1) +
  tmap::tm_basemap("Esri.WorldImagery") +  
  tmap::tm_compass(position = c("left", "bottom")) +
  tmap::tm_scale_bar() -> tm7

tmap::tmap_arrange(tm6, tm7, ncol = 2)






cube_2014 = sits::sits_mosaic(
  cube       = cube_2014,
  roi        = country,
  output_dir = "./data/cube_2014",
  multicores = 16
)

cube_2019 = sits::sits_mosaic(
  cube       = cube_2019,
  roi        = country,
  output_dir = "./data/cube_2019",
  multicores = 16
)

cube_2024 = sits::sits_mosaic(
  cube       = cube_2024,
  roi        = country,
  output_dir = "./data/cube_2024",
  multicores = 16
)




NDVI_2014_raster = raster::raster("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04.tif")
NDVI_2019_raster = raster::raster("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02.tif")
NDVI_2024_raster = raster::raster("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16.tif")

NDVI_2014_rast = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04.tif")
NDVI_2019_rast = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02.tif")
NDVI_2024_rast = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16.tif")

maxv <- 10000
NDVI_2014_rast_INT2S <- round(NDVI_2014_rast * maxv)
NDVI_2019_rast_INT2S <- round(NDVI_2019_rast * maxv)
NDVI_2024_rast_INT2S <- round(NDVI_2024_rast * maxv)

NDVI_2014_rast_INT2S = terra::crop(NDVI_2014_rast_INT2S, vect(country))
NDVI_2019_rast_INT2S = terra::crop(NDVI_2019_rast_INT2S, vect(country))
NDVI_2024_rast_INT2S = terra::crop(NDVI_2024_rast_INT2S, vect(country))

writeRaster(NDVI_2014_rast_INT2S, "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(NDVI_2019_rast_INT2S, "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02_INT2S.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(NDVI_2024_rast_INT2S, "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16_INT2S.tif", datatype='INT2S', overwrite=TRUE)

### DONT FORGET TO CROP OR SITS BREAKS
NDVI_2014_rast_INT2S = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04_INT2S.tif")
NDVI_2019_rast_INT2S = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02_INT2S.tif")
NDVI_2024_rast_INT2S = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16_INT2S.tif")

NDVI_2014_rast_INT2S = terra::crop(NDVI_2014_rast_INT2S, vect(country))
NDVI_2019_rast_INT2S = terra::crop(NDVI_2019_rast_INT2S, vect(country))
NDVI_2024_rast_INT2S = terra::crop(NDVI_2024_rast_INT2S, vect(country))

writeRaster(NDVI_2014_rast_INT2S, "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04_INT2S_CROP.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(NDVI_2019_rast_INT2S, "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02_INT2S_CROP.tif", datatype='INT2S', overwrite=TRUE)
writeRaster(NDVI_2024_rast_INT2S, "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16_INT2S_CROP.tif", datatype='INT2S', overwrite=TRUE)
NDVI_2014 = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04.tif")
NDVI_2019 = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02.tif")
NDVI_2024 = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16.tif")

# polygon to use as project extent:
country = sf::read_sf("./data/aoi/liberia_boundary_national.shp")
# point sf object to be cropped
samples_country  = sf::read_sf("./data/training_samples/glance_spatial_clip.shp")
# spatRasters to be cropped
NDVI_files <- list(
  "2014" = "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04.tif",
  "2019" = "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02.tif",
  "2024" = "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16.tif"
)
NDVI_rasters <- lapply(NDVI_files, terra::rast)
# Ensure CRS alignment
country <- st_transform(country, crs = st_crs(NDVI_rasters[[1]])) # Align CRS
samples_country <- st_transform(samples_country, crs = st_crs(NDVI_rasters[[1]]))
# Crop SpatRasters
NDVI_cropped <- lapply(NDVI_rasters, function(raster) {
  terra::crop(raster, vect(country))
})
# Crop point sf object
samples_country_cropped <- sf::st_crop(samples_country, st_bbox(country))
# save
terra::writeRaster(NDVI_cropped[["2014"]], "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04_INT2S_CROP.tif")
terra::writeRaster(NDVI_cropped[["2019"]], "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02_INT2S_CROP.tif")
terra::writeRaster(NDVI_cropped[["2024"]], "./data/NDVIs/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16_INT2S_CROP.tif")
sf::st_write(samples_country_cropped, "./data/training_samples/samples_country_cropped.shp")






# crop spatRasters
NDVI_2014_cropped = terra::crop(NDVI_2014, vect(country))
NDVI_2014_cropped = terra::crop(NDVI_2014, vect(country))
NDVI_2014_cropped = terra::crop(NDVI_2014, vect(country))
# crop point sf objects ? .... 


NDVI_stack = raster::stack(
  NDVI_2014_raster, 
  NDVI_2019_raster,
  NDVI_2024_raster
)

rasterVis::levelplot(NDVI_stack)

#sits::sits_bands(cube_2014)
#cube_2014 = mask(cube_2014, vect(aoi))



BLUE_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_BLUE_2024-01-02.tif")
GREEN_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_GREEN_2024-01-02.tif")
SWIR16_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_SWIR16_2024-01-02.tif")
SWIR22_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_SWIR22_2024-01-02.tif")

BLUE_2024_test = raster::raster("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_BLUE_2024-01-02.tif")
GREEN_2024_test = raster::raster("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_GREEN_2024-01-02.tif")
SWIR16_2024_test = raster::raster("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_SWIR16_2024-01-02.tif")
SWIR22_2024_test = raster::raster("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_SWIR22_2024-01-02.tif")

raster::dataType(NDVI_2024_test)
raster::dataType(BLUE_2024_test)

NIR08_2024_test = raster::raster("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_NIR08_2024-01-02.tif")
RED_2024_test = raster::raster("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_RED_2024-01-02.tif")
NDVI_2024_test = raster::raster("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-02.tif")

NIR08_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_NIR08_2024-01-02.tif")
RED_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_RED_2024-01-02.tif")
NDVI_2024_test = terra::rast("./data/cube_stac/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-02.tif")

terra::crs(NIR08_2024_test)
terra::crs(RED_2024_test)
terra::crs(NDVI_2024_test)

sf::st_crs(country)
bbox = sf::st_bbox(country)

terra::minmax(NDVI_2024_test, compute=T)
terra::minmax(BLUE_2024_test, compute=T)# terra works, raster package doesnt
terra::minmax(GREEN_2024_test, compute=T)
terra::minmax(NIR08_2024_test, compute=T)
terra::minmax(RED_2024_test, compute=T)
terra::minmax(SWIR16_2024_test, compute=T)
terra::minmax(SWIR22_2024_test, compute=T)



cube_2014 = sits::sits_cube(
  source     = "MPC",
  collection = "LANDSAT-C2-L2",
  data_dir   = "./data/cube_2014",
  parse_info = c("satellite", "sensor", "tile", "band", "date"),
  roi        = aoi
)

cube_2019 = sits::sits_cube(
  source     = "MPC",
  collection = "LANDSAT-C2-L2",
  data_dir   = "./data/cube_2019",
  parse_info = c("satellite", "sensor", "tile", "band", "date"),
  roi        = aoi
)

cube_2024 <- sits::sits_cube(
  source     = "MPC",
  collection = "LANDSAT-C2-L2",
  data_dir   = "./data/cube_2024",
  parse_info = c("satellite", "sensor", "tile", "band", "date"),
  roi        = aoi
)

cube_2014 = sits::sits_apply(
  data        = cube_2014,
  NDVI        = (NIR08 - RED) / (NIR08 + RED), 
  output_dir  = "./data/cube_2014",
  memsize     = 8,
  multicores  = 16,
  progress    = T
)

cube_2019 = sits::sits_apply(
  data        = cube_2019,
  NDVI        = (NIR08 - RED) / (NIR08 + RED), 
  output_dir  = "./data/cube_2019",
  memsize     = 8,
  multicores  = 16,
  progress    = T
)

cube_2024 = sits::sits_apply(
  data        = cube_2024,
  NDVI        = (NIR08 - RED) / (NIR08 + RED), 
  output_dir  = "./data/cube_2024",
  memsize     = 8,
  multicores  = 16,
  progress    = T
)
samples_country_tb = read.csv("./data/training_samples/glance_spatial_clip.csv") |> as_tibble()


samples_csv = read.csv("./data/training_samples/modified_samples.csv") |> as_tibble()
samples=st_as_sf(samples_csv,coords=c("longitude","latitude"), crs="EPSG:4326")|>
  sf::st_transform(32629)
st_write(samples, "./data/training_samples/samples.shp")

cube_bbox <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = ndvi_cube$xmin,
  ymin = ndvi_cube$ymin,
  xmax = ndvi_cube$xmax,
  ymax = ndvi_cube$ymax
  ), crs = "EPSG:32629"))

samples
st_bbox(samples)
st_crs(samples)
samples_vect = terra::vect(samples)
samples_ext  = terra::ext(samples_vect)
# SpatExtent : 233228.150682545, 657595.947362251, 509487.696342407, 923624.889602437 (xmin, xmax, ymin, ymax)
cube_ext = terra::ext(ndvi_cube$xmin, ndvi_cube$ymin, ndvi_cube$xmax, ndvi_cube$ymax, xy=T)
# SpatExtent : 222704.965713814, 681226.831104058, 459337.709183042, 946200.552994326 (xmin, xmax, ymin, ymax)
samples_vect_cropped <- terra::crop(samples_vect, cube_ext)
samples_vect_cropped_ext <- terra::ext(samples_vect_cropped)
# SpatExtent : 233228.150682545, 657595.947362251, 509487.696342407, 923624.889602437 (xmin, xmax, ymin, ymax)
samples_vect_intersect <- terra::intersect(samples_vect, cube_ext)
samples_vect_intersect_ext <- terra::ext(samples_vect_intersect)
samples_vect_intersect_ext
# SpatExtent : 233228.150682545, 657595.947362251, 509487.696342407, 923624.889602437 (xmin, xmax, ymin, ymax)

cube_ext_poly <- terra::as.polygons(cube_ext)
terra::crs(cube_ext_poly)  = terra::crs(samples_vect)
samples_vect_intersect     = terra::intersect(samples_vect, cube_ext_poly)
samples_vect_intersect_ext = terra::ext(samples_vect_intersect)
samples_vect_intersect_ext
# SpatExtent : 233228.150682545, 657595.947362251, 509487.696342407, 923624.889602437 (xmin, xmax, ymin, ymax)

samples_vect_crop <- terra::crop(samples_vect, cube_ext_poly)
intersects <- terra::relate(samples_vect, cube_ext_poly, "intersects")
samples_vect_filtered <- samples_vect[intersects, ]
samples_vect_filtered <- terra::crop(samples_vect_filtered, cube_ext)
samples_vect_filtered_ext <- terra::ext(samples_vect_intersect)
samples_vect_filtered_ext
# SpatExtent : 233228.150682545, 657595.947362251, 509487.696342407, 923624.889602437 (xmin, xmax, ymin, ymax)

samples_sf <- st_as_sf(samples_vect)
cube_sf <- st_as_sf(cube_ext_poly)
samples_sf_clipped <- st_intersection(samples_sf, cube_sf)
st_bbox(samples_sf_clipped)
#     xmin     ymin     xmax     ymax 
# 233228.2 509487.7 657595.9 923624.9 

samples_csv_file <- system.file("extdata/samples/samples_sinop_crop.csv",
                                package = "sits"
)
# Read the csv file into an R object
samples_csv <- read.csv(samples_csv_file)
# Print the first three samples
samples_csv[1:3, ]

# filter 1: no change resulting...
#samples_vect = terra::vect(samples)
#country_vect = terra::vect(country)
#country_vect_32629 = terra::project(country_vect, "EPSG:32629")

samples_crop = terra::crop(samples_vect, country_vect_32629)
samples_crop


# filter 2: no change resulting...
#samples_clipped  = sf::st_intersection(samples, sf::st_transform(country, 32629)) # n = 364
samples_country  = samples[samples_clipped, ]
samples_country_cropped <- sf::st_crop(samples_country, cube_bbox)
sf::st_write(samples_country_cropped, "./data/training_samples/samples_country_cropped.shp")
write.csv(samples_country, "./data/training_samples/glance_spatial_clip.csv", row.names = F)


# Check if the point intersects with the cube
overlap <- sf::st_intersects(single_point, cube_bbox, sparse = FALSE)
if (!any(overlap)) {
  stop("Point does not intersect with cube extent.")
} else {
  print("Point overlaps with cube extent.")
}


cube_bbox <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = -11.5,
  ymin = 4.36,
  xmax = -7.37,
  ymax = 8.55
), crs = "EPSG:4326"))


samples_bbox <- sf::st_bbox(training_samples) %>% sf::st_as_sfc()
# Ensure overlap
overlap <- sf::st_intersects(samples_bbox, cube_bbox, sparse = FALSE)
if (!any(overlap)) {
  stop("The spatial extents of the training samples do not overlap with the cube.")
}


# Check CRS of filtered samples
samples_crs <- sf::st_crs(training_samples)

# If CRS is not aligned, reproject samples
if (!identical(samples_crs$input, "EPSG:4326")) {
  training_samples <- sf::st_transform(training_samples, crs = "EPSG:4326")
}

samples_signatures <- sits::sits_get_data(
  cube = ndvi_cube,  # Use the recreated cube if necessary
  samples = training_samples,
  bands = "NDVI"
)


print(ndvi_cube)         # Metadata for the cube
print(training_samples)  # Structure of the training samples
plot(cube_bbox)
plot(sf::st_geometry(training_samples), add = TRUE, col = "red")  # Plot training samples

install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
library("pak")
pak::pkg_sysreqs("sits", upgrade = TRUE, dependencies = T)

psych::describe(as.data.frame(samples))
summary(samples$id)
sf::st_bbox(samples)
samples_vect = terra::vect(samples)
country_vect = terra::vect(country)
samples = terra::crop(samples_vect, country_vect)
samples_sf = sf::st_as_sf(samples)



dummy_points <- st_as_sf(data.frame(
  id = c("365", "366", "367", "368"),
  start_date = as.Date(c("2014-01-01", "2014-01-01", "2014-01-01", "2014-01-01")),
  end_date = as.Date(c("2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01")),
  label = "Dummy",  # Use NA for missing values
  x = c(ndvi_cube$xmin, ndvi_cube$xmax, ndvi_cube$xmin, ndvi_cube$xmax),
  y = c(ndvi_cube$ymin, ndvi_cube$ymin, ndvi_cube$ymax, ndvi_cube$ymax)
  ), coords = c("x", "y"), crs = st_crs(samples))

samples_extended <- rbind(samples, dummy_points)
bbox_extended <- sf::st_bbox(samples_extended)
print(bbox_extended)
ndvi_cube$xmin
ndvi_cube$ymin
ndvi_cube$xmax
ndvi_cube$ymax

sf::st_write(samples_extended, "./data/training_samples/samples_extended.shp", delete_dsn = TRUE)
sf::st_write(samples_sf, "./data/training_samples/samples_extended.shp", delete_dsn = TRUE)

create_dummy_points <- function(extent, crs, samples, prefix = "dummy") {
  dummy_data <- data.frame(
    id = paste0(prefix, 1:4),
    x = c(extent$xmin, extent$xmax, extent$xmin, extent$xmax),
    y = c(extent$ymin, extent$ymin, extent$ymax, extent$ymax)
  )
  
  # Add placeholders for missing columns with NA
  missing_cols <- setdiff(names(samples), names(dummy_data))
  for (col in missing_cols) {
    dummy_data[[col]] <- NA  # Use NA to represent missing values
  }
  
  # Convert to sf object
  st_as_sf(dummy_data, coords = c("x", "y"), crs = crs)
}
# Combine original samples and dummy points
extended_samples <- rbind(samples, dummy_points)

# Save the extended samples
st_write(extended_samples, "extended_training_samples.gpkg", delete_dsn = TRUE)


samples_4326 = sf::st_transform(samples, 4326)
samples_tibble = read.csv("./data/training_samples/glance_spatial_clip.csv") |> 
  tibble::as_tibble()#4326
NDVI_2024=terra::rast("./data/NDVIs_4326/LANDSAT_TM-ETM-OLI_200055_NDVI_2024-01-16.tif")
cube_bbox_sf = sf::st_read("./data/training_samples/extent_32629.gpkg", crs = st_crs(32629)) |>
  st_cast("POLYGON") |> sf::st_as_sf() 

cube_bbox_4326 = sf::st_transform(cube_bbox_sf, 4326) |> sf::st_bbox()
#      xmin       ymin       xmax       ymax 
#-11.519030   4.151739  -7.353369   8.556431 

cube_extent_4326 = terra::ext(NDVI_2024)
# (xmin,xmax,ymin,ymax)
#-11.4977169845026, 4.60835740753314, -7.57926571517323, 8.55187167328943 

roi2 <- c("lon_min"=cube_bbox_4326[1],"lat_min"=cube_bbox_4326[3],"lon_max"=cube_bbox_4326[2],"lat_max"=cube_bbox_4326[4])


samn
roi1 <- c("lon_min"=-11.519030,"lat_min"=4.151739,"lon_max"=-7.353369,"lat_max"=8.556431)


samples = sf::st_read("./data/training_samples/samples_extended.shp") 
samples  = sf::st_read("./data/training_samples/glance_spatial_clip.shp")


psych::describe(as.data.frame(samples))


library(terra)
grid_size <- res(NDVI_STACK)  
grid_origin <- c(ndvi_cube$xmin, ndvi_cube$ymin)  # Set grid origin
library(sf)
library(lwgeom)

# Snap samples to grid
samples_snapped <- st_snap_to_grid(
  x = samples,            # The samples object (sf object)
  size = grid_size,       # The grid cell size (from resolution)
  origin = grid_origin    # The origin of the grid (xmin, ymin of cube)
)




library(sf)
library(terra)
library(lwgeom)

# Load the raster and sf objects
ndvi_raster <- rast(NDVI_STACK)  # Replace with your raster stack object
samples <- st_read("./data/training_samples/samples_extended.shp")  # Replace with your samples file

# Reproject raster and sf objects to EPSG:3857
ndvi_raster_3857 <- project(ndvi_raster, "EPSG:3857")
samples_3857 <- st_transform(samples, crs = "EPSG:3857")

# Get grid resolution and origin from the reprojected raster
grid_size <- res(ndvi_raster_3857)  # Grid resolution in x and y
grid_origin <- c(ext(ndvi_raster_3857)[1], ext(ndvi_raster_3857)[3])  # xmin, ymin of raster

# Snap sample points to the grid
samples_snapped <- st_snap_to_grid(
  x = samples_3857,     # Reprojected sf object
  size = grid_size,     # Grid resolution
  origin = grid_origin  # Grid origin
)

# Save the snapped samples to a new file (optional)
st_write(samples_snapped, "./data/training_samples/snapped_samples.shp", delete_dsn = TRUE)

# Visualize the alignment
plot(ndvi_raster_3857[[1]], main = "Snapped Samples on NDVI Raster")
plot(st_geometry(samples_snapped), col = "red", add = TRUE)

ndvi_raster_3857







timeline <- as.Date(c("2014-01-04", "2019-01-02", "2024-01-16"))  # Example
samples_sf <- st_as_sf(samples_csv, coords = c("longitude", "latitude"), crs = 3857)
samples_ndvi <- terra::extract(NDVI_STACK, st_coordinates(samples_sf), cells = TRUE)

# Process extracted NDVI values
samples_ndvi_strip <- split(as.matrix(samples_ndvi[, -1]), seq_len(nrow(samples_ndvi)))

# Ensure each NDVI entry matches the timeline length
stopifnot(all(sapply(samples_ndvi_strip, length) == length(timeline)))


# Step 2: Convert to `sf` and Transform CRS
samples_sf <- samples_csv %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 3857) %>%
  st_transform(3857)

# Step 3: Define Timeline and Extract NDVI Values
timeline <- as.Date(c("2014-01-04", "2019-01-02", "2024-01-16"))
samples_ndvi <- terra::extract(NDVI_STACK, st_coordinates(samples_sf), cells = TRUE)

# Step 4: Ensure NDVI Matches Timeline Length
samples_ndvi_strip <- split(as.matrix(samples_ndvi[, -1]), seq_len(nrow(samples_ndvi)))
if (!all(sapply(samples_ndvi_strip, length) == length(timeline))) {
  stop("NDVI wrong timeline!")
}

# Step 5: Create `time_series` Column
time_series <- lapply(samples_ndvi_strip, function(values) {
  tibble(
    Index = timeline,
    NDVI = values
  )
})

# Step 6: Populate `samples_signatures`
samples_signatures <- tibble(
  longitude = samples_csv$longitude,
  latitude = samples_csv$latitude,
  start_date = as.Date("2014-01-04"),
  end_date = as.Date("2024-01-16"),
  label = samples_csv$label,
  cube = "ndvi_cube",
  time_series = time_series
)

# Step 7: Clean and Validate `samples_signatures`
samples_signatures <- samples_signatures %>%
  mutate(
    label = trimws(label),
    label = ifelse(label == "" | is.na(label), "Unknown", label),
    label = as.character(label)
  )

# Step 8: Remove Rows with All NA NDVI in `time_series`
samples_signatures_clean <- samples_signatures %>%
  filter(!sapply(time_series, function(x) all(is.na(x$NDVI))))

stopifnot(
  all(sapply(samples_signatures_clean$time_series, function(x) !all(is.na(x$NDVI)))),
  !any(is.na(samples_signatures_clean$label))
)

# Output
print(str(samples_signatures_clean))





# Step 1: Validate Structure
required_columns <- c("longitude", "latitude", "start_date", "end_date", "label", "cube", "time_series")
missing_columns <- setdiff(required_columns, colnames(samples_signatures_clean))
if (length(missing_columns) > 0) {
  stop("Missing columns in samples_signatures_clean: ", paste(missing_columns, collapse = ", "))
}
colnames(samples_signatures_clean)

# Step 2: Check Column Types
str(samples_signatures_clean)

# Step 3: Ensure No NA Values in time_series
invalid_time_series <- sapply(samples_signatures_clean$time_series, function(x) {
  is.null(x) || any(is.na(x$NDVI)) || any(is.na(x$Index))
})
if (any(invalid_time_series)) {
  stop("Invalid time_series data in rows: ", which(invalid_time_series))
}
print(paste("Invalid time_series data in rows:", which(invalid_time_series)))

invalid_time_series <- sapply(samples_signatures$time_series, function(x) all(is.na(x$NDVI)))
print(paste("Invalid time_series data in rows:", which(invalid_time_series)))

invalid_rows <- c(269, 313)

# Step 2: Remove Invalid Rows
samples_signatures_clean_drop <- samples_signatures_clean[-invalid_rows, ]

samples_signatures_clean <- samples_signatures %>%
  filter(!sapply(time_series, function(x) all(is.na(x$NDVI))))

samples_signatures_clean <- samples_signatures_clean %>%
  filter(!sapply(time_series, function(x) {
    is.null(x) || any(is.na(x$NDVI)) || any(is.na(x$Index))
  }))

samples_signatures <- sits_get_data(
  cube = ndvi_cube,
  samples = samples_csv
)



# reload local rasters
ndvi_cube <- sits::sits_cube(
  type = "GRIDDING",
  source      = "MPC",
  collection  = "LANDSAT-C2-L2",
  data_dir    = "./data/cube_resample",
  parse_info  = c("satellite", "sensor", "tile", "band", "date", "X1"),
  datetime = as.Date("2014-01-04"),
  bands = c("NDVI")
)



#waterways = sf::st_read("./data/HYDRO/waterways.shp")|> rmapshaper::ms_simplify(keep = 0.5)
waterways = sf::st_read("./data/HYDRO/Rivers_Gola_All.shp") |> sf::st_transform(4326)
species_rarity = terra::rast("./data/HABITAT/Combined_RWR_2024.tif")
species_threat = terra::rast("./data/HABITAT/Combined_THR_RWR_2024.tif")
habitat_threat = terra::rast("./data/HABITAT/Basic_Critical_Habitat_Raster.tif")
habitat_class = terra::rast("./data/HABITAT/Drill_Down_Critical_Habitat.tif")
species_rarity = terra::crop(habitat_class, bbox_aoi_2, mask=T)
species_threat = terra::crop(habitat_class, bbox_aoi_2, mask=T)
habitat_class  = terra::crop(habitat_class, bbox_aoi_2, mask=T)
habitat_threat = terra::crop(habitat_threat, bbox_aoi_2, mask=T)




bbox_country_1 = terra::vect(terra::ext(terra::vect(country)) * 1.1) |> sf::st_as_sf()
bbox_country_2 = terra::vect(terra::ext(terra::vect(country)) * 1.8) |> sf::st_as_sf()
bbox_aoi_1   = terra::vect(terra::ext(terra::vect(aoi)) * 1.5) |> sf::st_as_sf()
bbox_aoi_2   = terra::vect(terra::ext(terra::vect(aoi)) * 1.8) |> sf::st_as_sf()




#sf::st_write(slope_leakage_mask, "./data/MASK/slope_leakage_mask.shp", delete_dsn=T)
#sf::st_write(wetlands_leakage_mask, "./data/MASK/wetlands_leakage_mask.shp", delete_dsn=T)

tmap::tm_shape(wetlands) + 
  tm_raster(col.legend = tm_legend(title = "Wetlands"))

roads_mask         = sf::st_read("./data/MASK/roads_mask.shp")
slope_mask = sf::st_read("./data/MASK/slope_leakage_mask.shp")
wetlands_mask = sf::st_read("./data/MASK/wetlands_leakage_mask.shp")


sf::st_write(leakage_mask_a, "./data/MASK/leakage_mask_a.shp", delete_dsn=T)

sf::st_write(leakage_mask_b, "./data/MASK/leakage_mask_b.shp", delete_dsn=T)


tmap::tm_shape(bbox_aoi_1) + tmap::tm_borders(lwd=0) +
  tmap::tm_shape(wetlands_mask) + tmap::tm_raster("label",col.scale=tm_scale_categorical(), title="") +
  tmap::tm_shape(slope_mask) + tmap::tm_polygons(col="violet",fill="violet",fill_alpha=1,lwd=1) + 
  tmap::tm_shape(aoi) + tmap::tm_borders(col="red") +
  tmap::tm_add_legend(type="lines",col="yellow",labels="Leakage Belt (7.5km)") +  
  tmap::tm_shape(roads) + tmap::tm_lines(col="darkorange", lwd=1) + 
  tmap::tm_add_legend(type="lines", col="orange", labels = "Roads") +
  tmap::tm_shape(waterways) + tmap::tm_lines(col = "turquoise", lwd = 0.5) +
  tmap::tm_add_legend(type="lines", col="turquoise", labels = "Waterways") +
  tmap::tm_shape(counties) + tmap::tm_borders(lwd = 1, col="grey") +
  tmap::tm_add_legend(type="lines", col="grey", labels = "County Border") +  
  tmap::tm_shape(aoi) + tm_borders(col="red", lwd=2.5) + 
  tmap::tm_add_legend(type="lines", col="red", labels = "Project Area: UDef PA") +
  tmap::tm_shape(places) + tm_symbols(size=0.35,fill="pink", shape.scale = tm_scale_asis()) +
  tmap::tm_add_legend(type="symbols", col="purple", fill="purple", size=0.6,labels="Communities") +
  tmap::tm_layout(legend.position=c("left", "top"), legend.bg.color = "white", legend.text.size=0.1) +
  tmap::tm_basemap("Esri.WorldImagery")


tmap::tm_shape(aoi) + tmap::tm_borders(lwd=0) +
  tmap::tm_shape(wetlands) + tmap::tm_raster("label", col.scale = tm_scale_categorical(), title="")

tmap::tm_shape(wetlands) + 
  tm_raster(col.legend = tm_legend(title = "Wetlands"))

tmap::tm_shape(roads) + tmap::tm_lines(col="darkorange", lwd=1) + 
  tmap::tm_add_legend(type="lines", col="orange", labels = "Roads") +
  tmap::tm_shape(waterways) + tmap::tm_lines(col = "turquoise", lwd = 0.5) +
  tmap::tm_add_legend(type="lines", col="turquoise", labels = "Waterways") +
  tmap::tm_shape(counties) + tmap::tm_borders(lwd = 1, col="grey") +
  tmap::tm_add_legend(type="lines", col="grey", labels = "County Border") +  
  tmap::tm_shape(aoi) + tm_borders(col="red", lwd=2.5) + 
  tmap::tm_add_legend(type="lines", col="red", labels = "Project Area: UDef PA") +
  tmap::tm_shape(places) + tm_symbols(size=0.35,fill="pink", shape.scale = tm_scale_asis()) +
  tmap::tm_add_legend(type="symbols", col="purple", fill="purple", size=0.6,labels="Communities") +
  tmap::tm_layout(legend.position=c("left", "top"), legend.bg.color = "white", legend.text.size=0.1) +
  tmap::tm_basemap("Esri.WorldImagery")



## Compare LULC Filters

```{r, message=F, warning=F, error=F, comment=NA}
voi = sf::st_transform(aoi, 3857) |> terra::vect()
country_merc = sf::st_transform(country, 3857) |> terra::vect()
LULC_LIBERIA_2017=terra::rast("./data/LULC/IO_LULC_LIBERIA_2017.tif") |> terra::crop(country_merc, mask=T)
LULC_LIBERIA_2023=terra::rast("./data/LULC/IO_LULC_LIBERIA_2023.tif") |> terra::crop(country_merc, mask=T)
code_dict <- data.frame(id = c(1, 2, 4, 5, 7, 10, 11),  
                        label = c("Water", "Forest", "Wetland", "Bareground", "Built", "Cropland", "Rangeland"))
levels(LULC_LIBERIA_2017) <- code_dict
levels(LULC_LIBERIA_2023) <- code_dict
LULC_PROJECT_2017 = LULC_LIBERIA_2017 |> terra::crop(voi, mask=T)
LULC_PROJECT_2023 = LULC_LIBERIA_2023 |> terra::crop(voi, mask=T)
LULC_COUNTY_2017 = LULC_LIBERIA_2017 |> terra::crop(jurisdiction_merc, mask=T)
LULC_COUNTY_2023 = LULC_LIBERIA_2023 |> terra::crop(jurisdiction_merc, mask=T)
LULC_PROJECT_2017 = terra::project(LULC_PROJECT_2017, "EPSG:4326") 
LULC_PROJECT_2023 = terra::project(LULC_PROJECT_2023, "EPSG:4326")
LULC_COUNTY_2017 = terra::project(LULC_COUNTY_2017, "EPSG:4326") 
LULC_COUNTY_2023 = terra::project(LULC_COUNTY_2023, "EPSG:4326")
```




```{r}
places    = sf::st_read("./data/POP/Villages.shp") |> sf::st_transform(4326)
road     = sf::st_read("./data/ROADS/Roads_Gola.shp") |> sf::st_transform(4326)
waterways = sf::st_read("./data/HYDRO/Rivers_Gola_All.shp") |> sf::st_transform(4326)

road_count_whole = sf::st_intersection(road, leakage_belt_whole)
road_count = sf::st_intersection(road, leakage_belt)
road_length_whole = sum(sf::st_length(road_count_whole)) + sum(sf::st_length(road_count_whole))
road_length = sum(sf::st_length(road_count)) + sum(sf::st_length(road_count))
road_length_whole
road_length

waterways_count_whole = sf::st_intersection(waterways, leakage_belt_whole)
waterways_count = sf::st_intersection(waterways, leakage_belt)
waterways_length_whole = sum(sf::st_length(waterways_count_whole))
waterways_length = sum(sf::st_length(waterways_count))
waterways_length_whole
waterways_length

places_count_whole = sf::st_intersection(places, leakage_belt_whole)
places_count = sf::st_intersection(places, leakage_belt)
places_count_whole
places_count
```



Valid themes include default, bootstrap, cerulean, cosmo, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, and yeti. Pass null for no theme (in this case you can use the css parameter to add your own styles).


highlight specifies the syntax highlighting style. Supported styles include default, tango, pygments, kate, monochrome, espresso, zenburn, haddock, breezedark, and textmate. Pass null to prevent syntax highlighting.



---
title: 'VM0048: Leakage Belt Delineation'
author: "Winrock Intl"
date: "2025-05-16"
output: 
  html_document: 
    css: styles.scss
    theme: "united"
    highlight: pygments
    code_folding: show
    toc: true
    toc_depth: 2
    toc_float: 
      collapsed: false
    fig_width: 12
    fig_height: 8

editor_options: 
  markdown: 
    wrap: 120
---