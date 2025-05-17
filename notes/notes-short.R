

library(cols4all)
library(dplyr)
library(knitr)
library(latex2exp)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leafgl)
library(leafem)
library(latticeExtra)
library(maptiles)
library(raster)
library(Rcpp)
library(RcppThread)
library(rmarkdown)
library(sf)
#library(sp)
library(terra)
library(tidyterra)
library(tidyverse)
library(tinytex)
library(tmap)
library(tmaptools)


country = sf::read_sf("./data/AOI/liberia_boundary_national.shp")
counties = sf::st_read("./data/AOI/places_poly_county.shp")
jurisdiction = counties |>dplyr::filter(name=="Grand Cape Mount County"|name=="Gharpolu County")
jurisdiction$name = 'Grand Cape Mount & Gharpolu Counties'
aoi = sf::read_sf("./data/AOI/AllLandscapes_merge_v02.shp") |> 
  sf::st_cast("POLYGON") |> sf::st_cast("MULTIPOLYGON") |> 
  dplyr::select(Name) |>
  dplyr::filter(
    Name == "Gola Forest National Park" | 
      Name == "Norman" | 
      Name == "Tonglay") |>
  sf::st_transform(4326)
bbox_country_1 = terra::vect(terra::ext(terra::vect(country)) * 1.1) |> sf::st_as_sf()
bbox_country_2 = terra::vect(terra::ext(terra::vect(country)) * 1.8) |> sf::st_as_sf()
bbox_aoi_1   = terra::vect(terra::ext(terra::vect(aoi)) * 1.5) |> sf::st_as_sf()
bbox_aoi_2   = terra::vect(terra::ext(terra::vect(aoi)) * 1.8) |> sf::st_as_sf()
aoi$area_ha = round(as.numeric(sf::st_area(aoi) * 0.0001, 4))
aoi |> sf::st_drop_geometry() |> janitor::adorn_totals() 
sf::st_crs(bbox_country_2) = 4326
sf::st_crs(bbox_aoi_2) = 4326
sf::st_crs(bbox_aoi_1) = 4326

tmap::tmap_mode("view")
leakage_belt   = sf::st_read("./data/LEAKAGE/leakage_belt_10km.shp")
leakage_belt$area_ha = round(as.numeric(sf::st_area(leakage_belt) * 0.0001, 4))
leakage_belt |> sf::st_drop_geometry() |> janitor::adorn_totals() 
tmap::tm_shape(aoi) + tmap::tm_borders(lwd=0) +
  tmap::tm_shape(leakage_belt) + tmap::tm_polygons(fill="yellow",fill_alpha=0.3,lwd=1)+
  tmap::tm_add_legend(type="lines",col="yellow",labels="Leakage Belt (10 km)") +  
  tmap::tm_basemap("Esri.WorldImagery")

wetland = terra::rast("./data/HABITAT/GLWD_v2_delta_main_class.tif")
wetland = terra::crop(wetland, bbox_aoi_2, mask=T)|>as.factor()
code_dict_2 <- data.frame(
  id = c(1, 4, 7, 10, 12, 14, 15, 18, 20, 21, 26, 31),
  label = c(
    "Freshwater lake",                         # 1
    "Large river",                             # 4
    "Small streams",                           # 7
    "Riverine, regularly flooded, forested",   # 10
    "Riverine, seasonally flooded, forested",  # 12
    "Riverine, seasonally saturated, forested",# 14
    "Riverine, seasonally saturated, non-forested", # 15
    "Palustrine, seasonally saturated, forested",# 18
    "Ephemeral, forested",                     # 20
    "Ephemeral, non-forested",                 # 21
    "Tropical peatland, forested",             # 26
    "Other coastal wetland"                    # 31
  )
)
levels(wetland) <- code_dict_2
wetland[wetland == 0] <- NA
wetland_mask <- wetland
writeRaster(wetland_mask, "./data/MASK/wetland_mask.tif", overwrite = T)
wetland_mask = terra::rast("./data/MASK/wetland_mask.tif")
wetland_poly = terra::as.polygons(wetland_mask, dissolve=T) |> sf::st_as_sf() |> sf::st_combine() 
sf::st_write(wetland_poly, "./data/MASK/wetland_mask.shp", delete_dsn=T)

DEM = terra::rast("./data/DEM/DEM_Z12.tif") |> terra::project("epsg:3857")
slope_deg  = terra::terrain(DEM, v="slope", unit="degrees")
slope_pc   = tan(slope_deg * (pi / 180)) * 100
slope_pc   = terra::clamp(slope_pc, 0, 100) 
slope_mask = slope_pc > 10
slope_mask[slope_mask == 0] <- NA
slope_mask = terra::project(slope_mask, "EPSG:4326") |> terra::crop(leakage_belt, mask=T)
raster::writeRaster(slope_mask, "./data/MASK/slope_mask.tif", overwrite=T)
slope_poly = terra::as.polygons(slope_mask,dissolve=T)|>sf::st_as_sf()|>sf::st_combine()#|>sf::st_intersection(aoi)
sf::st_write(slope_poly, "./data/MASK/slope_mask.shp", delete_dsn=T)

leakage_merc = sf::st_transform(leakage_belt, 3857)
roads = sf::st_read("./data/ROADS/roads.shp")
roads_merc = sf::st_transform(roads, 3857)
roads_buff = sf::st_buffer(roads_merc, dist=10000)|> sf::st_combine() 
roads_buff = sf::st_intersection(leakage_merc, roads_buff)
roads_mask = sf::st_difference(leakage_merc, road_buffer)
roads_mask = sf::st_transform(roads_mask, 4326)
sf::st_write(roads_mask, "./data/MASK/roads_mask.shp", delete_dsn=T)


wetland_leakage=sf::st_read("./data/MASK/wetland_mask.shp")
slope_leakge =sf::st_read("./data/MASK/slope_mask.shp")

tmap::tm_shape(aoi) + tmap::tm_borders(lwd=0) +
  tmap::tm_shape(wetland_leakage) + tmap::tm_borders(col="blue") +
  tmap::tm_shape(slope_leakge) + tmap::tm_borders(col="green") +
  tmap::tm_shape(aoi) + tmap::tm_borders(col="red") +
  tmap::tm_layout(legend.position=c("left", "top"), legend.text.size=0.1) +
  tmap::tm_basemap("Esri.WorldImagery")
  