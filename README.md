
# REDD+ Feasability Assessment

2024-12-23

## Summary

This initial review of the RSPB REDD+ project provides comparison of 
methodologies for deriving forest area estimates and activity data.
Please consider, this review is an early effort and remains open for
correction or improvement, with the understanding that previous methods
may yet provide the more accurate representation. In the following introduction 
section, headline results were presented from the preliminary feasibility study, 
and from our independent analysis herein. The focus of this report is first on 
estimating baseline deforestation areas, and second on allocating annual 
deforestation at the pixel level based on risk scoring informed by VT0007 and 
VMD0055 guides.

**Forest Area Estimates** (2014--2024):

-   Gola National Park:

    2013: Reported 88,881.98 ha; 2014 Reviewed 88,096.44 ha.

    2019: Reported 87,869.96 ha; Reviewed 87,869.96 ha.

    2024: Reported 88,177.83 ha; Reviewed 88,177.83 ha.

-   Norman:

    2013: Reported 6993.26 ha; 2014 Reviewed 6,988.65 ha.

    2019: Reviewed 6,963.31 ha.

    2024: Reviewed 6,993.08 ha.

-   Tonglay:

    2013: Reported 29,515.3 ha; 2014 Reviewed 29,362.95 ha.

    2019: Reviewed 29,152.85 ha.

    2024: Reviewed 29,345.93 ha.

-   Total Project Area:

    2014: Reported 125,328.36 ha; Reviewed 124,316.7 ha.

    2019: Reviewed 123,854.3 ha.

    2024: Reviewed 124,388 ha.

![](data/tables/Table%201%20Project%20files%20reviewed.png)

##### Table 1: Project files reviewed in this assessment

*Import AOI:*

``` r
set.seed(77777)
# Import project area aoi
aoi = sf::read_sf(".../Winrock_GIS Analysis Gola/ProjectArea.shp") 
aoi = aoi |>
  sf::st_cast("POLYGON") |>
  sf::st_cast("MULTIPOLYGON") |>
  dplyr::filter(
    NAME   == "Gola Forest National Park" | 
      NAME == "Tonglay" |
      NAME == "Normon")
# Import national area aoi
query = osmdata::opq("Liberia") |> # opq = overpass query
  osmdata::add_osm_feature(key = "boundary", value = "administrative") |>
  osmdata::osmdata_sf()
boundaries = query$osm_multipolygons
country  = boundaries |>
  dplyr::filter(admin_level == "2", name == "Liberia") |>
  dplyr::select(name, admin_level, geometry) |>
  sf::st_cast() |> 
  sf::st_transform(4326)
sf::st_write(country, "./data/aoi/liberia_boundary_national.shp", delete_layer=T)
```

``` r
tmap::tmap_mode("plot")
# zoom = 11 = 1:150,000 scale - https://wiki.openstreetmap.org/wiki/Zoom_levels
basemap_aoi = maptiles::get_tiles(
  bbox_aoi_2, 
  zoom      = 10, 
  crop      = T,
  provider  = "Esri.NatGeoWorldMap"
  )

basemap_country = maptiles::get_tiles(
  bbox_country_2, 
  zoom      = 8, 
  crop      = T,
  provider  = "Esri.NatGeoWorldMap"
  )

tmap::tm_shape(bbox_aoi) + tmap::tm_borders(lwd = 0.0) +
  tmap::tm_shape(basemap_aoi) + tmap::tm_rgb() + 
  tmap::tm_shape(aoi) + tmap::tm_borders(lwd = 2, col = "red") +
  tmap::tm_text("NAME", just = "center", col="black", size=0.6) +
  tmap::tm_graticules(lines=T,labels.rot=c(0,90),lwd=0.2) +
  tmap::tm_credits("EPSG:32629", position = c("left", "bottom")) +
  tmap::tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = .5) + 
  tmap::tm_compass(color.dark = "gray60", text.color = "gray60", position = c("left", "top")) -> tm1

tmap::tm_shape(bbox_country) + tmap::tm_borders(lwd = 0.0) +
  tmap::tm_shape(basemap_country) + tmap::tm_rgb() + 
  tmap::tm_shape(country) + tmap::tm_borders(col = "blue", lwd = 2) +
  tmap::tm_shape(aoi) + tmap::tm_borders(col = "red", lwd = 2) +
  tmap::tm_graticules(lines=T,labels.rot=c(0,90),lwd=0.2) +
  tmap::tm_credits("EPSG:4326", position = c("left", "bottom")) +
  tmap::tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = .5) + 
  tmap::tm_compass(color.dark = "gray60", text.color = "gray60", position = c("left", "top")) -> tm2
                   
tmap::tmap_save(tm1, "./data/site_maps/site_map.png", height=10000, asp=0, dpi=2400)
tmap::tmap_save(tm2, "./data/site_maps/locator_map.png", height=10000, asp=0, dpi=2400)
tmap::tmap_arrange(tm1, tm2, ncol=2)
```

``` r
knitr::include_graphics(c("~/repos/rspb-redd-feasability/data/site_maps/site_map.png",
                          "~/repos/rspb-redd-feasability/data/site_maps/locator_map.png"))
```

<img src="data/site_maps/site_map.png" width="50%"/><img src="data/site_maps/locator_map.png" width="50%"/>

## Area check

In Liberia, the official definition of forest land is provided by the
Forestry Development Authority (Liberia 2019), including areas of land
that meet the following criteria:

-   Canopy cover of minimum 30%;
-   Canopy height of minimum 5m or the capacity to reach it;
-   Covering a minimum of 1 hectare of land.

``` r
aoi$area_m2 = round(as.numeric(sf::st_area(aoi) * 0.0001, 4))
aoi_select = aoi |>
  dplyr::select(NAME, DESIG, Areahav2, area_m2) |>
  dplyr::filter(
    NAME == "Gola Forest National Park" | 
      NAME == "Normon"  | 
      NAME == "Tonglay") |>
  sf::st_drop_geometry() |>
  janitor::adorn_totals() 
sf::st_transform(aoi, 4326)  
```


``` r
sf::st_transform(country, 4326) 
```

| name    | admn_lv | geometry                       |
|:--------|:--------|:-------------------------------|
| Liberia | 2       | POLYGON ((-11.60808 6.755, ... |

``` r
slivers = aoi |> dplyr::filter(as.numeric(area_m2) < 1)  # no artefacts found
aoi_select
```

| NAME                      | DESIG            |  Areahav2 | area_m2 |
|:--------------------------|:-----------------|----------:|--------:|
| Gola Forest National Park | National Park    |  88801.80 |   88820 |
| Tonglay                   | Community Forest |  29516.10 |   29515 |
| Normon                    | Community Forest |   6993.87 |    6993 |
| Total                     | \-               | 125311.77 |  125328 |

##### Table 2: Project area estimates verified using `sf` GDAL 3.10 drivers

Results indicate the spatial dataset is free from topological errors,
and forest 'islands' and spatial artefacts smaller than approved area
definition. Results also match Areahav2 values derived previously and
reported in ER_Workbook_Gola_Liberia.xlsx

## Activity data check

This section documents inputs and procedures used to derive national and
project-level deforestation maps. This follows three main steps:

-   Compare land cover training samples for Liberia;
-   Classify & assess accuracy of forest cover maps for 2014, 2019, and
    2024;
-   Spatially map deforestation risk & compare with Liberia's submitted
    FREL estimates.

***Training samples***

Training samples for the country and project area were extracted from
two gold standard global land cover time series datasets: the GLanCE
dataset (Stanimirova et al. 2023) and TimeSpec4LULC dataset. Both
training datasets were processed with continuous change and class
migration algorithms. While Verra is yet to establish requirements for
addressing feature class migration in classification of baseline
activity data (Verra 2023, 2024, 2021), we may recommend incorporating
this remote sensing best practice or showcase its improvements to
accuracy metrics, as follows.

![](data/tables/Table%202%20Training%20samples%20class%20conversions.png)

##### Table 3: Class conversions of training samples

Level-1 classes in the GLanCE and TimeSpec4LULC datasets were recoded
below to match class labels cited in the "Lookups" sheet of
"ER_Workbook_Gola_Liberia.xlsx", and the report titled "Liberia's Forest
Reference Emission Level Submission to the UNFCCC (Woodcock et al.,
n.d.; Liberia 2019).

``` r
# import & tidy samples
samples_raw = read.csv("./data/training_samples/glance_training.csv")
samples_clean = samples_raw |>
  dplyr::select(Lon, Lat, Glance_Class_ID_level1, Start_Year, End_Year)|>
  dplyr::rename(longitude = Lon) |>
  dplyr::rename(latitude = Lat) |>
  dplyr::rename(label_old = Glance_Class_ID_level1) |>
  dplyr::mutate(start_date = as.Date(paste(Start_Year,"01","01",sep = "-")))|>
  dplyr::mutate(end_date = as.Date(paste(End_Year, "01", "01", sep = "-")))|>
  dplyr::select(longitude, latitude, start_date, end_date, label_old)|>
  dplyr::mutate(code = case_when(
    label_old == '4' ~ 0, 
    label_old == '7' ~ 1, 
    label_old == '6' ~ 2, 
    label_old == '5' ~ 3, 
    label_old == '1' ~ 4, 
    label_old == '3' ~ 99, 
    label_old == '2' ~ 88)
    ) |>
  dplyr::mutate(label = case_when(
    code == '0'  ~ "Bareground", 
    code == '1'  ~ "Regrowth", 
    code == '2'  ~ "Farmbush", 
    code == '3'  ~ "Forest", 
    code == '4'  ~ "Water", 
    code == '99' ~ "Urban", 
    code == '88' ~ "Snow")
    ) |> 
  dplyr::mutate(label = as.factor(label)) |>
  dplyr::mutate(id = row_number()) |> 
  dplyr::select(-label_old) |>
  dplyr::select(-code)
# filter to project
samples_sf       = sf::st_as_sf(samples_clean, crs = 4326, coords = c("longitude", "latitude"))
samples_clipped  = sf::st_intersection(samples_sf, country) # n = 364
samples_country  = samples_sf[samples_clipped, ] |> sf::st_transform(4326)
samples          = sf::st_crop(samples_country, st_bbox(country))
sf::st_write(samples, "./data/training_samples/glance_spatial_clip.shp", delete_dsn = T)
write.csv(samples, "./data/training_samples/glance_spatial_clip.csv", row.names = F)
dplyr::count(samples, label)
```

```         
Reading layer `glance_spatial_clip' from data source 
  `/Users/seamus/repos/rspb-redd-feasability/data/training_samples/glance_spatial_clip.shp' 
  using driver `ESRI Shapefile'
Simple feature collection with 364 features and 4 fields
Geometry type: POINT
Dimension:     XY
Bounding box:  xmin: -11.41444 ymin: 4.608361 xmax: -7.579165 ymax: 8.353939
Geodetic CRS:  WGS 84
```

| label    |   n | geometry                       |
|:---------|----:|:-------------------------------|
| Farmbush |   4 | MULTIPOINT ((-11.35816 6.81... |
| Forest   | 311 | MULTIPOINT ((-11.41444 6.96... |
| Regrowth |   9 | MULTIPOINT ((-9.780878 6.16... |
| Urban    |  37 | MULTIPOINT ((-10.78342 6.36... |
| Water    |   3 | MULTIPOINT ((-11.26555 6.76... |


***Raster collection***

The dataset of STAC-formatted Landsat Collection-2-Level-2 was extracted
from the Google Earth Engine Catalog and processed using a cloudless and
pixel quality ranking mask before back-filling with median
normalization. This was implemented in a [Colab python runtime
here](https://colab.research.google.com/github/seamusrobertmurphy/01-data-processing/blob/main/01-data-processing.ipynb).
The collection of unclassified raster outputs was temporarily stored in
[Google Drive
folder](https://drive.google.com/drive/folders/1XMYYhBUAsvuZ02avsZHYHDTArqztLaFI?usp=drive_link)
and the consolidated, resampled and labelled full stack was stored
[here](https://drive.google.com/file/d/1Vn0KDzkFDaBhpdC803IbYVRuu5cHx0SO/view?usp=drive_link).

Landsat data was acquired instead of Sentinel imagery due to start date
of project's 10-year baseline occurring before the launch of the
Sentinel 2 satellite. The following chunk provides an alternative
worflow, though less reliable, R-native workflow for acquiring,
aligning, and processing rasters for the extent of Liberia.

``` r
roi <- st_bbox(country) %>% st_as_sfc()
# cube assembly
cube_2024_mpc <- sits_cube(
  source      = "MPC",
  collection  = "LANDSAT-C2-L2",
  roi         = roi,
  bands       = c("BLUE", "GREEN", "NIR08", "RED", "SWIR16", "SWIR22", "CLOUD"),
  start_date  = "2024-01-01",
  end_date    = "2024-02-01"
  )
# cloud-mask & normalization
cube_2024_reg <- sits_regularize(
  cube        = cube_2024_mpc,
  res         = 30,
  period      = "P60D",
  multicores  = 16,
  output_dir  = "./data/cube_stac")
# Derive NDVI
cube_2024_spectral = sits::sits_apply(
  data        = cube_2024_reg,
  NDVI        = (NIR08 - RED) / (NIR08 + RED), 
  output_dir  = "./data/cube_stac",
  memsize     = 8,
  multicores  = 16,
  progress    = T
  )
# Sequence of raster stack
STACK = brick(NDVI_2014, NDVI_2019, NDVI_2024,
          BLUE_2014, BLUE_2019, BLUE_2024, 
          GREEN_2014, GREEN_2019, GREEN_2024,
          NIR08_2014, NIR08_2019, NIR08_2024, 
          RED_2014, RED_2019, RED_2024, 
          SWIR16_2014, SWIR16_2019, SWIR16_2024, 
          SWIR22_2014, SWIR22_2019, SWIR22_2024,
          DEM)
```

The processes above were repeated for three baseline years of 2014,
2019, and 2024, which were then saved as raster stacked and visualized
below.

``` r
# import
NDVI_2014=terra::rast("~/repos/rspb-redd-feasability/data/STACK/LANDSAT_TM-ETM-OLI_198055_NDVI_2014-01-04.tif")
NDVI_2019=terra::rast("~/repos/rspb-redd-feasability/data/STACK/LANDSAT_TM-ETM-OLI_198055_NDVI_2019-01-02.tif")
NDVI_2024=terra::rast("~/repos/rspb-redd-feasability/data/STACK/LANDSAT_TM-ETM-OLI_198055_NDVI_2024-01-16.tif")
STACK=raster::brick(
  "~/repos/rspb-redd-feasability/data/STACK/LANDSAT_TM-ETM-OLI_198055_STACK-&-DEM_2014-01-04_2024-01-16.tif")

# visualize
hist(NDVI_2014, main = "NDVI Distribution, 2014", col = "springgreen") 
hist(NDVI_2019, main = "NDVI Distribution, 2019", col = "springgreen")
hist(NDVI_2024, main = "NDVI Distribution, 2024", col = "springgreen")
plot(NDVI_2014,main="NDVI, 2014",xlim=c(-11.5,-7.5),ylim=c(4.1,8.6),border="gray")
plot(st_geometry(samples), add=T)
plot(NDVI_2019,main="NDVI, 2014",xlim=c(-11.5,-7.5),ylim=c(4.1,8.6),border="gray")
plot(st_geometry(samples), add=T)
plot(NDVI_2024,main="NDVI, 2014",xlim=c(-11.5,-7.5),ylim=c(4.1,8.6),border="gray")
plot(st_geometry(samples), add=T)
```

<img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-13-1.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-13-2.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-13-3.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-13-4.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-13-5.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-13-6.png" width="33%"/>

***Image classification***

We trained a Random Forest model fitted with 500 decision trees.
Training/test split partitioned the dataset using a 70:30 ratio.
Accuracy assessments were reported using a confusion matrix for full
model and cross-validation estimates. Uncertainty metrics were used to
select best subset of variables according to magnitude and performance.
Models were then calibrated in number regression trees and architecture
rules, with cross-validation reported to assess internal bias from true
population estimates before and to improve uncertainty of final model
deployment.

``` r
# extract yearly layers
STACK_2014=subset(STACK, c("NDVI_2014","BLUE_2014","GREEN_2014","NIR08_2014",
                           "RED_2014","SWIR16_2014","SWIR22_2014","DEM"))
# extract signatures
signatures_2014 = raster::extract(STACK_2014, samples ,df=T) # watch for data formats
samples_signatures_2014 <- dplyr::inner_join(signatures_2014, samples, by=c("ID"="id"))
samples_signatures_2014$geometry <- NULL # set geometry to NULL for model training

# training-test split, p=0.7 -> 70% split
trainIndex_2014 <- caret::createDataPartition(samples_signatures_2014$ID,list=F,p=0.7)
trainData_2014  <- samples_signatures_2014[trainIndex_2014,]  
testData_2014   <- samples_signatures_2014[-trainIndex_2014,] 

# interpolate NAs with class-median-normalization (NAs -> missing cloud pixels)
trainData_2014 <- trainData_2014 |> group_by(label) |> mutate(across(where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |> ungroup()
testData_2014 <- testData_2014 |> group_by(label) |> mutate(across(where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |> ungroup()

# assign model variables
response  <- c("label")
predictors_2014 <- c(
  "NDVI_2014", "BLUE_2014", "GREEN_2014", "RED_2014", 
  "NIR08_2014", "SWIR16_2014", "SWIR22_2014", "DEM"
  )
# set training parameters
cv_regime <- caret::trainControl(
  method          = 'cv',
  number          = 10,
  savePredictions = T,
  verboseIter     = F
  )

# train classifier
rf_model_2014 <- caret::train(
  label~.,
  data = trainData_2014[, c(predictors_2014, "label")], # drop ID var
  trControl = cv_regime,
  method    = "rf", 
  metric    = 'Kappa', 
  ntree     = 500,
  tuneLength= 6,
  importance= T
  )
```

***Accuracy assessments** (note metrics change each run)*

Results from the updated Random Forest model indicate robust predictive
performance during both cross-validation and testing phases.

**Model Training and Cross-Validation Results (2014)**:

-   **Kappa Index:** 0.753 (Moderate agreement).
-   **Accuracy:** 94.44%.
-   **Optimal mtry:** 8 (selected based on highest Kappa).
-   Resampling metrics:
-   Accuracy ranged from 93.85% to 94.97%, with corresponding Kappa
    values between 0.748 and 0.788.

The model achieved high sensitivity for key classes such as **Forest**
(Sensitivity: 0.9892, Specificity: 0.7333) and **Urban** (Sensitivity:
0.8182, Specificity: 0.9897). However, no predictions were made for
**Farmbush** and **Water**, indicating challenges with class imbalance.

**Blind Test Results**:

-   **Accuracy:** 94.44% (95% CI: 88.3%--97.9%).
-   **No Information Rate (NIR):** 86.11%, indicating significant
    predictive power.
-   **Kappa Index:** 0.753 (moderate agreement).
-   Class-specific performance:
-   **Forest:** Sensitivity: 0.9892; Specificity: 0.7333.
-   **Urban:** Sensitivity: 0.8182; Specificity: 0.9897.
-   **Regrowth:** Sensitivity: 1.0000; Specificity: 0.9907.
-   **Farmbush and Water:** Sensitivity and Specificity: Undefined due
    to lack of predictions.

``` r
rf_test_2014 <- predict(rf_model_2014, testData_2014)
print(rf_model_2014) # cv results
```

```         
Random Forest 

256 samples
  8 predictor
  5 classes: 'Farmbush', 'Forest', 'Regrowth', 'Urban', 'Water' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 230, 232, 229, 232, 229, 229, ... 
Resampling results across tuning parameters:

  mtry  Accuracy   Kappa    
  2     0.9384858  0.7484736
  3     0.9425100  0.7731017
  4     0.9425100  0.7731017
  5     0.9425100  0.7809018
  6     0.9425100  0.7809018
  8     0.9497393  0.7882299

Kappa was used to select the optimal model using the largest value.
The final value used for the model was mtry = 8.
```

``` r
caret::confusionMatrix(rf_test_2014,testData_2014$label) # blind test results
```

```         
Confusion Matrix and Statistics

          Reference
Prediction Farmbush Forest Regrowth Urban Water
  Farmbush        0      0        0     0     0
  Forest          1     92        0     2     1
  Regrowth        0      1        1     0     0
  Urban           1      0        0     9     0
  Water           0      0        0     0     0

Overall Statistics
                                         
               Accuracy : 0.9444         
                 95% CI : (0.883, 0.9793)
    No Information Rate : 0.8611         
    P-Value [Acc > NIR] : 0.004895       
                                         
                  Kappa : 0.753          
                                         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: Farmbush Class: Forest Class: Regrowth Class: Urban
Sensitivity                  0.00000        0.9892        1.000000      0.81818
Specificity                  1.00000        0.7333        0.990654      0.98969
Pos Pred Value                   NaN        0.9583        0.500000      0.90000
Neg Pred Value               0.98148        0.9167        1.000000      0.97959
Prevalence                   0.01852        0.8611        0.009259      0.10185
Detection Rate               0.00000        0.8519        0.009259      0.08333
Detection Prevalence         0.00000        0.8889        0.018519      0.09259
Balanced Accuracy            0.50000        0.8613        0.995327      0.90394
                     Class: Water
Sensitivity              0.000000
Specificity              1.000000
Pos Pred Value                NaN
Neg Pred Value           0.990741
Prevalence               0.009259
Detection Rate           0.000000
Detection Prevalence     0.000000
Balanced Accuracy        0.500000
```

***Model calibration***

-   A subsetted model using recursive feature elimination (RFE)
    achieved:
-   **Accuracy:** 96.30% (95% CI: 90.79%--98.98%).
-   **Kappa Index:** 0.8456 (strong agreement).
-   Top three predictors identified: **SWIR16_2014**, **SWIR22_2014**,
    and **NDVI_2014**.

Subsetting slightly improved class-specific sensitivity and specificity
for underrepresented classes such as **Water** (Sensitivity: 1.000;
Specificity: 1.000). However, due to negligible benefits in complexity
reduction, the original model is recommended for further spatial
predictions.

#### Model Weaknesses and Recommendations

While the Random Forest model performed well overall, specific
challenges remain:

-   **Class Imbalance**: No predictions were made for **Farmbush** and
    **Water** in the original model. This could result from insufficient
    representation of these classes in the training data.
-   **Predictor Optimization**: While recursive feature selection
    demonstrated potential for reducing model complexity, the trade-off
    between performance and simplicity was minimal.

To address these issues:

**Class Imbalance**:

-   Experiment with weighted Random Forest models or alternative machine
    learning algorithms such as Gradient Boosting or Support Vector
    Machines (SVM).
-   Increase the representation of underrepresented classes in the
    training dataset, potentially through synthetic data generation or
    focused sampling.

**Feature Selection**:

-   If reducing complexity is a priority, the subsetted model with three
    predictors can be considered for future spatial predictions.

``` r
index_feature_2014 <- createMultiFolds(trainData_2014$label, times=5) 
predictor_seq_2014 <-seq(from=1, to=length(predictors_2014),by=2)

subset_regime_2014 <- rfeControl(
  method="cv",
  number = 10,
  verbose=FALSE,
  functions=rfFuncs,
  index=index_feature_2014
  )

rf_model_subset_2014 <- caret::rfe(
  label~.,
  data = trainData_2014[, c(predictors_2014, "label")], 
  sizes = predictor_seq_2014,
  metric = "Kappa",
  ntree=500,
  method="rf",
  rfeControl = subset_regime_2014
  )

rf_subset_test_2014 <- predict(rf_model_subset_2014,testData_2014)
print(rf_model_subset_2014)
```

```         
Recursive feature selection

Outer resampling method: Cross-Validated (10 fold) 

Resampling performance over subset size:

 Variables Accuracy  Kappa AccuracySD KappaSD Selected
         1   0.9057 0.6353    0.04318  0.1587         
         3   0.9509 0.7992    0.03979  0.1645        *
         5   0.9439 0.7701    0.04098  0.1764         
         7   0.9400 0.7469    0.04705  0.2084         
         8   0.9384 0.7471    0.04514  0.1890         

The top 3 variables (out of 3):
   SWIR16_2014, SWIR22_2014, NDVI_2014
```

``` r
caret::confusionMatrix(rf_subset_test_2014$pred,testData_2014$label)
```

```         
Confusion Matrix and Statistics

          Reference
Prediction Farmbush Forest Regrowth Urban Water
  Farmbush        0      0        0     0     0
  Forest          0     92        0     1     0
  Regrowth        0      1        1     0     0
  Urban           2      0        0    10     0
  Water           0      0        0     0     1

Overall Statistics
                                          
               Accuracy : 0.963           
                 95% CI : (0.9079, 0.9898)
    No Information Rate : 0.8611          
    P-Value [Acc > NIR] : 0.0004509       
                                          
                  Kappa : 0.85            
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: Farmbush Class: Forest Class: Regrowth Class: Urban
Sensitivity                  0.00000        0.9892        1.000000      0.90909
Specificity                  1.00000        0.9333        0.990654      0.97938
Pos Pred Value                   NaN        0.9892        0.500000      0.83333
Neg Pred Value               0.98148        0.9333        1.000000      0.98958
Prevalence                   0.01852        0.8611        0.009259      0.10185
Detection Rate               0.00000        0.8519        0.009259      0.09259
Detection Prevalence         0.00000        0.8611        0.018519      0.11111
Balanced Accuracy            0.50000        0.9613        0.995327      0.94424
                     Class: Water
Sensitivity              1.000000
Specificity              1.000000
Pos Pred Value           1.000000
Neg Pred Value           1.000000
Prevalence               0.009259
Detection Rate           0.009259
Detection Prevalence     0.009259
Balanced Accuracy        1.000000
```

#### Spatial Predictions and Outputs

Models for 2019 and 2024 were trained similarly and achieved comparable
performance metrics. Classified LULC rasters for all years (2014, 2019,
2024) were generated and saved in the project's designated Google Drive
folder for further analysis.

This section highlights the robustness of the Random Forest approach for
predicting land use and land cover while addressing areas for
methodological improvement to enhance future modeling efforts.

``` r
# predict lulc rasters
LULC_LIBERIA_2014 <- raster::predict(STACK_2014,rf_model_2014) 
raster::writeRaster(LULC_LIBERIA_2014,"./data/LULC/LULC_LIBERIA_2014-01-04.tif",
  format = "GTiff",overwrite = T)
LULC_LIBERIA_2019 <- raster::predict(STACK_2019,rf_model_2019) 
raster::writeRaster(LULC_LIBERIA_2019,"./data/LULC/LULC_LIBERIA_2019-01-02.tif",
  format = "GTiff",overwrite = T)
LULC_LIBERIA_2024 <- raster::predict(STACK_2024,rf_model_2024) 
raster::writeRaster(LULC_LIBERIA_2024,"./data/LULC/LULC_LIBERIA_2024-01-16.tif",
  format = "GTiff",overwrite = T)

# visualise
LULC_LIBERIA_2014 = raster::raster("~/repos/rspb-redd-feasability/data/LULC/LULC_LIBERIA_2014-01-04.tif")
LULC_LIBERIA_2019 = raster::raster("~/repos/rspb-redd-feasability/data/LULC/LULC_LIBERIA_2019-01-02.tif")
LULC_LIBERIA_2024 = raster::raster("~/repos/rspb-redd-feasability/data/LULC/LULC_LIBERIA_2024-01-16.tif")
terra::plot(LULC_LIBERIA_2014, main="Land Cover Classification, 2014")
terra::plot(LULC_LIBERIA_2019, main="Land Cover Classification, 2019")
terra::plot(LULC_LIBERIA_2024, main="Land Cover Classification, 2024")
```

<img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-19-1.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-19-2.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-19-3.png" width="33%"/>

### Forest cover mapping

``` r
forest_class = 3
forest_2014 <- LULC_LIBERIA_2014 == forest_class
forest_2019 <- LULC_LIBERIA_2019 == forest_class
forest_2024 <- LULC_LIBERIA_2024 == forest_class
# Forest loss
forest_loss_2014_2019 <- forest_2014 & !forest_2019
forest_loss_2019_2024 <- forest_2019 & !forest_2024
forest_loss_2014_2024 <- forest_2014 & !forest_2024

terra::plot(forest_loss_2014_2019, main="Forest Loss 2014-2019")
terra::plot(forest_loss_2019_2024, main="Forest Loss 2019-2024")
terra::plot(forest_loss_2014_2024, main="Forest Loss 2014-2024")
terra::plot(forest_2014, main="Binary Forest Cover Map, 2014")
terra::plot(forest_2019, main="Binary Forest Cover Map, 2019")
terra::plot(forest_2024, main="Binary Forest Cover Map, 2024")

# Save the binary forest maps
raster::writeRaster(forest_2014, "~/repos/rspb-redd-feasability/data/LULC/forest_2014.tif",overwrite=T)
raster::writeRaster(forest_2019, "~/repos/rspb-redd-feasability/data/LULC/forest_2019.tif",overwrite=T)
raster::writeRaster(forest_2024, "~/repos/rspb-redd-feasability/data/LULC/forest_2024.tif",overwrite=T)
raster::writeRaster(forest_loss_2014_2019, "~/repos/rspb-redd-feasability/data/LULC/forest_loss_2014_2019.tif",overwrite=T)
raster::writeRaster(forest_loss_2019_2024, "~/repos/rspb-redd-feasability/data/LULC/forest_loss_2019_2024.tif",overwrite=T)
raster::writeRaster(forest_loss_2014_2024, "~/repos/rspb-redd-feasability/data/LULC/forest_loss_2014_2024.tif",overwrite=T)
```

<img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-21-1.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-21-2.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-21-3.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-21-4.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-21-5.png" width="33%"/><img src="rspb-gola-redd-review_files/figure-gfm/unnamed-chunk-21-6.png" width="33%"/>
\### Forest loss mapping

```         
|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          

|---------|---------|---------|---------|=========================================                                          
```

``` r
# Calculate total number of forest pixels for each year
resolution <- res(forest_2014)[1]
forest_2014_estimate <- sum(forest_2014[], na.rm = TRUE) * resolution^2 / 10000
forest_2019_estimate <- sum(forest_2019[], na.rm = TRUE) * resolution^2 / 10000
forest_2024_estimate <- sum(forest_2024[], na.rm = TRUE) * resolution^2 / 10000
cat("Forest area in 2014:", forest_2014_estimate, "hectares\n")
```

```         
Forest area in 2014: 9146913 hectares
```

``` r
cat("Forest area in 2019:", forest_2019_estimate, "hectares\n")
```

```         
Forest area in 2019: 8675144 hectares
```

``` r
cat("Forest area in 2024:", forest_2024_estimate, "hectares\n")
```

```         
Forest area in 2024: 9112944 hectares
```

``` r
# Calculate forest area for each sub-region (in hectares) for all years
forest_2014_gola_estimate <- sum(forest_2014_gola[], na.rm = TRUE) * resolution^2 / 10000
forest_2014_tong_estimate <- sum(forest_2014_tong[], na.rm = TRUE) * resolution^2 / 10000
forest_2014_norm_estimate <- sum(forest_2014_norm[], na.rm = TRUE) * resolution^2 / 10000
forest_2014_project_estimate <- sum(forest_2014_project[], na.rm = TRUE) * resolution^2 / 10000
forest_2019_gola_estimate <- sum(forest_2019_gola[], na.rm = TRUE) * resolution^2 / 10000
forest_2019_tong_estimate <- sum(forest_2019_tong[], na.rm = TRUE) * resolution^2 / 10000
forest_2019_norm_estimate <- sum(forest_2019_norm[], na.rm = TRUE) * resolution^2 / 10000
forest_2019_project_estimate <- sum(forest_2019_project[], na.rm = TRUE) * resolution^2 / 10000
forest_2024_gola_estimate <- sum(forest_2024_gola[], na.rm = TRUE) * resolution^2 / 10000
forest_2024_tong_estimate <- sum(forest_2024_tong[], na.rm = TRUE) * resolution^2 / 10000
forest_2024_norm_estimate <- sum(forest_2024_norm[], na.rm = TRUE) * resolution^2 / 10000
forest_2024_project_estimate <- sum(forest_2024_project[], na.rm = TRUE) * resolution^2 / 10000

# Calculate forest loss for each sub-region (in hectares) for all periods
forest_loss_2014_2019_gola_estimate <- sum(forest_loss_2014_2019_gola[],na.rm=T)*resolution^2/10000
forest_loss_2014_2019_tong_estimate <- sum(forest_loss_2014_2019_tong[],na.rm=T)*resolution^2/10000
forest_loss_2014_2019_norm_estimate <- sum(forest_loss_2014_2019_norm[],na.rm=T)*resolution^2/10000
forest_loss_2014_2019_project_estimate <- sum(forest_loss_2014_2019_project[],na.rm=T)*resolution^2/10000
forest_loss_2019_2024_gola_estimate<- sum(forest_loss_2019_2024_gola[],na.rm=T)*resolution^2/10000
forest_loss_2019_2024_tong_estimate <- sum(forest_loss_2019_2024_tong[],na.rm=T)*resolution^2/10000
forest_loss_2019_2024_norm_estimate <- sum(forest_loss_2019_2024_norm[],na.rm=T)*resolution^2/10000
forest_loss_2019_2024_project_estimate <- sum(forest_loss_2019_2024_project[],na.rm=T)*resolution^2/10000
forest_loss_2014_2024_gola_estimate <- sum(forest_loss_2014_2024_gola[],na.rm=T)*resolution^2/10000
forest_loss_2014_2024_tong_estimate <- sum(forest_loss_2014_2024_tong[],na.rm=T)*resolution^2/10000
forest_loss_2014_2024_norm_estimate <- sum(forest_loss_2014_2024_norm[],na.rm=T)*resolution^2/10000
forest_loss_2014_2024_project_estimate <- sum(forest_loss_2014_2024_project[],na.rm=T)*resolution^2/10000

# Print out the forest area and loss estimates for each region and year
cat("Forest Area in Gola NP (2014):", forest_2014_gola_estimate, "hectares\n")
```

```         
Forest Area in Gola NP (2014): 88096.44 hectares
```

``` r
cat("Forest Area in Tonglay (2014):", forest_2014_norm_estimate, "hectares\n")
```

```         
Forest Area in Tonglay (2014): 6988.652 hectares
```

``` r
cat("Forest Area in Norman (2014):", forest_2014_tong_estimate, "hectares\n")
```

```         
Forest Area in Norman (2014): 29362.95 hectares
```

``` r
cat("Forest Area in Project (2014):", forest_2014_project_estimate, "hectares\n")
```

```         
Forest Area in Project (2014): 124316.7 hectares
```

``` r
cat("Forest Area in Gola NP (2019):", forest_2019_gola_estimate, "hectares\n")
```

```         
Forest Area in Gola NP (2019): 87869.96 hectares
```

``` r
cat("Forest Area in Tonglay (2019):", forest_2019_tong_estimate, "hectares\n")
```

```         
Forest Area in Tonglay (2019): 29152.85 hectares
```

``` r
cat("Forest Area in Norman (2019):", forest_2019_norm_estimate, "hectares\n")
```

```         
Forest Area in Norman (2019): 6963.308 hectares
```

``` r
cat("Forest Area in Project (2019):", forest_2019_project_estimate, "hectares\n")
```

```         
Forest Area in Project (2019): 123854.3 hectares
```

``` r
cat("Forest Area in Gola NP (2024):", forest_2024_gola_estimate, "hectares\n")
```

```         
Forest Area in Gola NP (2024): 88177.83 hectares
```

``` r
cat("Forest Area in Tonglay (2024):", forest_2024_tong_estimate, "hectares\n")
```

```         
Forest Area in Tonglay (2024): 29345.93 hectares
```

``` r
cat("Forest Area in Norman (2024):", forest_2024_norm_estimate, "hectares\n")
```

```         
Forest Area in Norman (2024): 6993.084 hectares
```

``` r
cat("Forest Area in Project (2024):", forest_2024_project_estimate, "hectares\n")
```

```         
Forest Area in Project (2024): 124388 hectares
```

``` r
cat("Forest Loss in Gola NP (2014-2019):", forest_loss_2014_2019_gola_estimate, "hectares\n")
```

```         
Forest Loss in Gola NP (2014-2019): 290.468 hectares
```

``` r
cat("Forest Loss in Tonglay (2014-2019):", forest_loss_2014_2019_tong_estimate, "hectares\n")
```

```         
Forest Loss in Tonglay (2014-2019): 235.4777 hectares
```

``` r
cat("Forest Loss in Norman (2014-2019):", forest_loss_2014_2019_norm_estimate, "hectares\n")
```

```         
Forest Loss in Norman (2014-2019): 30.12628 hectares
```

``` r
cat("Forest Loss in Project (2014-2019):", forest_loss_2014_2019_project_estimate, "hectares\n")
```

```         
Forest Loss in Project (2014-2019): 557.3676 hectares
```

``` r
cat("Forest Loss in Gola NP (2019-2024):", forest_loss_2019_2024_gola_estimate, "hectares\n")
```

```         
Forest Loss in Gola NP (2019-2024): 44.25972 hectares
```

``` r
cat("Forest Loss in Tonglay (2019-2024):", forest_loss_2019_2024_tong_estimate, "hectares\n")
```

```         
Forest Loss in Tonglay (2019-2024): 44.27696 hectares
```

``` r
cat("Forest Loss in Norman (2019-2024):", forest_loss_2019_2024_norm_estimate, "hectares\n")
```

```         
Forest Loss in Norman (2019-2024): 2.112028 hectares
```

``` r
cat("Forest Loss in Project (2019-2024):", forest_loss_2019_2024_project_estimate, "hectares\n")
```

```         
Forest Loss in Project (2019-2024): 91.06986 hectares
```

``` r
cat("Forest Loss in Gola NP (2014-2024):", forest_loss_2014_2024_gola_estimate, "hectares\n")
```

```         
Forest Loss in Gola NP (2014-2024): 54.93414 hectares
```

``` r
cat("Forest Loss in Tonglay (2014-2024):", forest_loss_2014_2024_tong_estimate, "hectares\n")
```

```         
Forest Loss in Tonglay (2014-2024): 60.44355 hectares
```

``` r
cat("Forest Loss in Norman (2014-2024):", forest_loss_2014_2024_norm_estimate, "hectares\n")
```

```         
Forest Loss in Norman (2014-2024): 4.31716 hectares
```

``` r
cat("Forest Loss in Project (2014-2024):", forest_loss_2014_2024_project_estimate, "hectares\n")
```

```         
Forest Loss in Project (2014-2024): 118.906 hectares
```

### Benchmarking & thresholding

In the following section, we calibrate NDVI thresholds to compare
outputs with unsupervised K-means clustering and the European Space
Agency's Dynamic Global land cover dataset.

``` r
# thresholding
ndvi_float = ndvi_stack * 0.0001
ndvi_thresholds <- classify(
  ndvi_float, c(-1.0, 0.1, 0.4, 0.7, 1), 
  include.lowest=TRUE, 
  brackets=TRUE)
# kmeans clustering
ndvi_raster = raster::raster(ndvi_float)
nr = raster::getValues(ndvi_raster)
i <- !is.na(nr)
kmncluster <- kmeans(
  nr[i], 
  centers = 10, 
  iter.max = 500, 
  nstart = 5, 
  algorithm="Lloyd")
nr[i] <- kmncluster$cluster
kmeans <- setValues(ndvi_raster, nr)
writeRaster(kmeans, "./data/kmeans/NDVI_STACK_KMEANS.tif", overwrite=T)
writeRaster(ndvi_thresholds, "./data/thresholds/NDVI_STACK_THRESHOLDS.tif", overwrite=T)
```

### Appendix I: 2019 and 2024 classifiers

To run these, you may change eval=F to eval=T at the top of chunk in the
.Rmd or .R file saved in the OneDrive folder.

``` r
########################### 2019
# extract signatures
signatures_2019 = raster::extract(STACK_2019, samples ,df=T) # watch for data formats
samples_signatures_2019 <- dplyr::inner_join(signatures_2019, samples, by=c("ID"="id"))
samples_signatures_2019$geometry <- NULL # set geometry to NULL for model training

# training-test split, p=0.7 -> 70% split
trainIndex_2019 <- caret::createDataPartition(samples_signatures_2019$ID,list=F,p=0.7)
trainData_2019  <- samples_signatures_2019[trainIndex_2019,]  
testData_2019   <- samples_signatures_2019[-trainIndex_2019,] 

# interpolate NAs with class-median-normalization (NAs -> missing cloud pixels)
trainData_2019 <- trainData_2019 |> group_by(label) |> mutate(across(where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |> ungroup()
testData_2019 <- testData_2019 |> group_by(label) |> mutate(across(where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |> ungroup()

# assign model variables
response  <- c("label")
predictors_2019 <- c(
  "NDVI_2019", "BLUE_2019", "GREEN_2019", "RED_2019", 
  "NIR08_2019", "SWIR16_2019", "SWIR22_2019", "DEM"
  )

# train classifier
rf_model_2019 <- caret::train(
  label~.,
  data = trainData_2019[, c(predictors_2019, "label")], # drop ID var
  trControl = cv_regime,
  method    = "rf", 
  metric    = 'Kappa', 
  ntree     = 500,
  tuneLength= 6,
  importance= T
  )

rf_test_2019 <- predict(rf_model_2019, testData_2019)
print(rf_model_2019) # cv results
confusionMatrix(rf_test_2019,testData_2019$label) # blind test results

index_feature_2019 <- createMultiFolds(trainData_2019$label, times=5) 
predictor_seq_2019 <-seq(from=1, to=length(predictors_2019),by=2)

subset_regime_2019 <- rfeControl(
  method="cv",
  number = 10,
  verbose=FALSE,
  functions=rfFuncs,
  index=index_feature_2019
  )

rf_model_subset_2019 <- caret::rfe(
  label~.,
  data = trainData_2019[, c(predictors_2019, "label")], 
  sizes = predictor_seq_2019,
  metric = "Kappa",
  ntree=500,
  method="rf",
  rfeControl = subset_regime_2019
  )

rf_subset_test_2019 <- predict(rf_model_subset_2019,testData_2019)
print(rf_model_subset_2019)
confusionMatrix(rf_subset_test_2019$pred,testData_2019$label)

######################### 2024
# extract signatures
signatures_2024 = raster::extract(STACK_2024, samples ,df=T) # watch for data formats
samples_signatures_2024 <- dplyr::inner_join(signatures_2024, samples, by=c("ID"="id"))
samples_signatures_2024$geometry <- NULL # set geometry to NULL for model training

# training-test split, p=0.7 -> 70% split
trainIndex_2024 <- caret::createDataPartition(samples_signatures_2024$ID,list=F,p=0.7)
trainData_2024  <- samples_signatures_2024[trainIndex_2024,]  
testData_2024   <- samples_signatures_2024[-trainIndex_2024,] 

# interpolate NAs with class-median-normalization (NAs -> missing cloud pixels)
trainData_2024 <- trainData_2024 |> group_by(label) |> mutate(across(where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |> ungroup()
testData_2024 <- testData_2024 |> group_by(label) |> mutate(across(where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |> ungroup()
water_2014 <- trainData_2014[trainData_2014$label == "Water", ]
water_ids <- water_2014$ID
water_2024 <- samples_signatures_2024[samples_signatures_2024$ID %in% water_ids, ]
trainData_2024 <- rbind(trainData_2024, water_2024)
table(trainData_2024$label)

# assign model variables
response  <- c("label")
predictors_2024 <- c("NDVI_2024", "BLUE_2024", "GREEN_2024", "RED_2024", 
  "NIR08_2024", "SWIR16_2024", "SWIR22_2024", "DEM"
  )

# train classifier
rf_model_2024 <- caret::train(
  label~.,
  data = trainData_2024[, c(predictors_2024, "label")], # drop ID var
  trControl = cv_regime,
  method    = "rf", 
  metric    = 'Kappa', 
  ntree     = 500,
  tuneLength= 6,
  importance= T
  )

rf_test_2024 <- predict(rf_model_2024, testData_2024)
print(rf_model_2024) # cv results
confusionMatrix(rf_test_2024,testData_2024$label) # blind test results

index_feature_2024 <- createMultiFolds(trainData_2024$label, times=5) 
predictor_seq_2024 <-seq(from=1, to=length(predictors_2024),by=2)

subset_regime_2024 <- rfeControl(
  method="cv",
  number = 10,
  verbose=FALSE,
  functions=rfFuncs,
  index=index_feature_2024
  )

rf_model_subset_2024 <- caret::rfe(
  label~.,
  data = trainData_2024[, c(predictors_2024, "label")], 
  sizes = predictor_seq_2024,
  metric = "Kappa",
  ntree=500,
  method="rf",
  rfeControl = subset_regime_2024
  )

rf_subset_test_2024 <- predict(rf_model_subset_2024,testData_2024)
print(rf_model_subset_2024)
confusionMatrix(rf_subset_test_2024$pred,testData_2024$label)
```

::: {#refs .references .csl-bib-body .hanging-indent entry-spacing="0"}
::: {#ref-governmentofliberiaLiberiasForestReference2019 .csl-entry}
Liberia, Government of. 2019. *Liberia's Forest Reference Emission Level
Submission to the UNFCCC*. 1. Forestry Development Authority.
:::

::: {#ref-stanimirovaGlobalLandCover2023 .csl-entry}
Stanimirova, Radost, Katelyn Tarrio, Konrad Turlej, Kristina McAvoy,
Sophia Stonebrook, Kai-Ting Hu, Paulo Arévalo, et al. 2023. "A Global
Land Cover Training Dataset from 1984 to 2020." *Scientific Data* 10
(1): 879. <https://doi.org/10.1038/s41597-023-02798-5>.
:::

::: {#ref-verraVT0007UnplannedDeforestation2021 .csl-entry}
Verra. 2021. *VT0007: Unplanned Deforestation Allocation Tool*. 0.1.
Verra.
<https://verra.org/wp-content/uploads/2024/02/VT0007-Unplanned-Deforestation-Allocation-v1.0.pdf>.
:::

::: {#ref-verraVM0048ReducingEmissions2023a .csl-entry}
---------. 2023. *VM0048: Reducing Emissions from Deforestation and
Forest Degradation*. 1.0. Verra.
<https://verra.org/wp-content/uploads/2023/11/VM0048-Reducing-Emissions-from-Deforestation-and-Forest-Degradation-v1.0-1-1.pdf>.
:::

::: {#ref-verraVMD0055EstimationEmission2024 .csl-entry}
---------. 2024. *VMD0055: Estimation of Emission Reductions from
Avoiding Unplanned Deforestation*. VCS Module 1.1. Verra.
<https://verra.org/wp-content/uploads/2024/10/VMD0055-Estimation-of-Emission-Reductions-from-Avoiding-Unplanned-Deforestation-v1.1-CLEAN-2024.10.21.24.pdf>.
:::

::: {#ref-woodcockGlobalLandCover .csl-entry}
Woodcock, Curtis, Pontus Olofsson, Thomas Loveland, Chris Barber, and
Zhe Zhu. n.d. "Global Land Cover Estimation (GLanCE) Product User Guide
Version 1.0 August 2022."
:::
:::
