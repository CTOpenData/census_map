# load libraries
library(dplyr)
library(leaflet)
library(htmltools)
library(mapview)
library(scales)
library(sf)

# set working directory to Census folder on share drive
setwd("N:/Data Team/Census/Map/Data")

# import tract geojson files
all_ct_tracts <- geojsonio::geojson_read("N:/Data Team/Census/Map/Data/all_ct_tracts.geojson", what = "sp")
htc_ct_tracts <- geojsonio::geojson_read("N:/Data Team/Census/Map/Data/htc_ct_tracts.geojson", what = "sp")

# import geojson town and county boundary files
ct_towns <- geojsonio::geojson_read("N:/Data Team/Census/Map/Data/ct_towns.geojson", what = "sp")
ct_counties <- geojsonio::geojson_read("N:/Data Team/Census/Map/Data/ct_counties.geojson", what = "sp")

# import schools
data_url <- "https://data.ct.gov/resource/v4tt-nt9n.geojson"
data_file <- "v4tt-nt9n.geojson"
download.file(data_url, data_file)
ct_schools <- geojsonio::geojson_read(data_file, what = "sp")
ct_schools_sf <- st_as_sf(ct_schools)
school_type <- c("Public Schools", 
                 "Regional Schools", 
                 "Regional Education Service Center Schools", 
                 "Public Charter Schools",
                 "Endowed and Incorporated Academies Schools",
                 "CT Technical Education and Career Schools")
ct_schools_filtered <- ct_schools_sf %>% dplyr::filter(ct_schools_sf$organization_type %in% school_type)
ct_schools_sp <- as(ct_schools_filtered, "Spatial")

# import libraries
data_url2 <- "https://data.imls.gov/resource/3qh2-qqv7.geojson?state=CT"
data_file2 <- "3qh2-qqv7.geojson"
download.file(data_url2, data_file2)
ct_libraries <- geojsonio::geojson_read(data_file2, what = "sp")
ct_libraries_df <- as.data.frame(ct_libraries)

# format library name text for popups
ct_libraries_df$library_name <- tolower(ct_libraries_df$library_name)
ct_libraries_df$library_name <- tools::toTitleCase(ct_libraries_df$library_name)

# recode internet access variable
all_ct_tracts$pcat_all <- dplyr::recode(all_ct_tracts$pcat_all, 
  "0" = "Zero", 
  "1" = "Less than 200 per 1,000 households", 
  "2" = "Between 200 and 400 per 1,000 households", 
  "3" = "Between 400 and 600 per 1,000 households", 
  "4" = " Between 600 and 800 per 1,000 households", 
  "5" = "More than 800 per 1,000 households")

# format variables for tract popup 
all_ct_tracts$Tot_Population_ACS_12_16 <- format(all_ct_tracts$Tot_Population_ACS_12_16, big.mark = ",")
all_ct_tracts$Med_HHD_Inc_ACS_12_16 <- as.numeric(sub("\\$","",all_ct_tracts$Med_HHD_Inc_ACS_12_16))
all_ct_tracts$Med_HHD_Inc_ACS_12_16 <- format(all_ct_tracts$Med_HHD_Inc_ACS_12_16, big.mark = ",")
all_ct_tracts$Med_HHD_Inc_ACS_12_16 <- paste0("$",all_ct_tracts$Med_HHD_Inc_ACS_12_16)
all_ct_tracts$Tot_Housing_Units_ACS_12_16 <- format(all_ct_tracts$Tot_Housing_Units_ACS_12_16, big.mark = ",")
all_ct_tracts$Tot_Occp_Units_ACS_12_16 <- format(all_ct_tracts$Tot_Occp_Units_ACS_12_16, big.mark = ",")

# create bins for map of hard to count tracts
bins <- c(0, 60, 65, 70, 73)
binpal <- colorBin("YlOrRd", htc_ct_tracts$Mail_Return_Rate_CEN_2010, bins = bins, na.color = "transparent", reverse = TRUE)

# format labels for tracts
labels <- sprintf(
  "Tract <strong>%s</strong> in <strong>%s</strong> had a mail<br>return rate of <strong>%g percent</strong> in Census 2010.",
  all_ct_tracts$Tract, all_ct_tracts$County_name, all_ct_tracts$Mail_Return_Rate_CEN_2010
) %>% lapply(htmltools::HTML)

# format popup for tracts 
htc_popups <- sprintf(
  "<div class='leaflet-popup-scrolled' style='width: 400px;max-height: 200px'>
  <strong><h3>Census Tract %s, %s</strong></h3>
  2010 Census Mail Return Rate:    <b>%g%%</b><br>
  Low Response Score:    <b>%g%%</b><br>
  </br>
  <strong><ins>Population Characteristics (2012-2016 ACS 5-Year Estimates)</ins></strong><br>
  Total Population:     <b>%s</b><br>
  Median Household Income:    <b>%s</b><br>
  Below Poverty Level:    <b>%g%%</b><br>
  Population Under 5:    <b>%g%%</b><br>
  Not High School Graduate:    <b>%g%%</b><br>
  Nonwhite:    <b>%g%%</b><br>
  Foreign Born:    <b>%g%%</b><br>
  No One in Household Age 14+ Speaks English 'Very Well':    <b>%g%%</b><br>
  </br>
  <strong><ins>Housing Unit Characteristics (2012-2016 ACS 5-Year Estimates)</ins></strong><br>
  Total Housing Units:     <b>%s</b><br>
  Total Occupied Housing Units:     <b>%s</b><br>
  Vacant Housing Units:    <b>%g%%</b><br>
  Renter Occupied Housing Units:    <b>%g%%</b><br>
  Multi-Unit Housing Units:    <b>%g%%</b><br>
  Crowded Housing Units:    <b>%g%%</b><br>
  </br>
  <strong><ins>Internet Access</ins></strong><br>
  Residential fixed high-speed connections over 200 kbps in at least one direction per 1,000 households:    <b>%s</b><br>",
  all_ct_tracts$Tract, all_ct_tracts$County_name, all_ct_tracts$Mail_Return_Rate_CEN_2010, all_ct_tracts$Low_Response_Score,
  all_ct_tracts$Tot_Population_ACS_12_16, all_ct_tracts$Med_HHD_Inc_ACS_12_16, all_ct_tracts$pct_Prs_Blw_Pov_Lev_ACS_12_16,
  all_ct_tracts$pct_Pop_under_5_ACS_12_16, all_ct_tracts$pct_Not_HS_Grad_ACS_12_16, all_ct_tracts$pct_minority, all_ct_tracts$pct_Born_foreign_ACS_12_16,
  all_ct_tracts$pct_ENG_VW_ACS_12_16, all_ct_tracts$Tot_Housing_Units_ACS_12_16, all_ct_tracts$Tot_Occp_Units_ACS_12_16,
  all_ct_tracts$pct_Vacant_Units_ACS_12_16, all_ct_tracts$pct_Renter_Occp_HU_ACS_12_16, all_ct_tracts$pct_multiunit, all_ct_tracts$pct_Crowd_Occp_U_ACS_12_16,
  all_ct_tracts$pcat_all, "</div>"
)

# create library icon
libraryIcon <- makeIcon(
  iconUrl = "\\\\opm-fs102/UserRedirections/zaldonisp/Desktop/Census/Planning Database/_ionicons_svg_md-book.svg",
  iconWidth = 30, iconHeight = 30
)

# create school icon
schoolIcon <- makeIcon(
  iconUrl = "\\\\opm-fs102/UserRedirections/zaldonisp/Desktop/Census/Planning Database/_ionicons_svg_md-school.svg",
  iconWidth = 30, iconHeight = 30
)

# set up leaflet map
ct_htc_map <- leaflet(options = leafletOptions(minZoom = 8)) %>%
  setView(-73, 41.5, zoom=9) %>% 
  setMaxBounds(-74.5, 40.5, -70.75, 42.5) %>% 
  addMapPane("background_map", zIndex = 410) %>%
  addMapPane("polygons", zIndex = 420) %>%
  addMapPane("labels", zIndex = 430) %>%
  addMapPane("points", zIndex = 440) %>%
  addProviderTiles(
    providers$Esri.WorldGrayCanvas,
    options = pathOptions(pane = "background_map")
  ) %>%
  addPolygons(
    data = htc_ct_tracts,
    fillColor = ~ binpal(Mail_Return_Rate_CEN_2010),
    color = "#b2aeae",
    fillOpacity = 0.7,
    weight = 0,
    smoothFactor = 0.2,
    group = "Census tract boundaries",
    options = pathOptions(pane = "polygons")
  ) %>%
  addPolygons(
    data = all_ct_tracts,
    color = "#b2aeae",
    fillOpacity = 0,
    weight = .75,
    smoothFactor = 0.2,
    group = "Census tracts",
    options = pathOptions(pane = "polygons")
  ) %>%
  addPolygons(
    data = ct_towns,
    color = "#b2aeae",
    fillOpacity = 0,
    weight = 1.5,
    group = "Towns",
    options = pathOptions(pane = "polygons")
  ) %>%
  addPolygons(
    data = ct_towns,
    color = "transparent",
    fillOpacity = 0,
    options = pathOptions(pane = "polygons"),
    label = ct_towns$NAME,
    labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, textsize = "12px", style = list(
      "color" = "gray",
      "text-shadow" = "1px 1px 8px #FFFFFF")
      ),
    group = "Town labels"
  ) %>% 
  groupOptions(
    "Town labels", zoomLevels = 11:20
      ) %>% 
  addPolygons(
    data = ct_counties,
    color = "#636363",
    dashArray = "6",
    fillOpacity = 0,
    weight = 3,
    group = "Counties",
    options = pathOptions(pane = "polygons"),
    label = ct_counties$NAME,
    labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, textsize = "14px", style = list(
      "color" = "dimgray",
      "font-weight" = "bold")
    )
  ) %>%
  addPolygons(
    data = all_ct_tracts,
    color = "transparent",
    fillOpacity = 0,
    weight = 0,
    options = pathOptions(pane = "labels"),
    highlight = highlightOptions(
      weight = 5,
      color = "#636363",
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    popup = ~htc_popups,
    popupOptions = popupOptions(
      maxWidth = 400
    )
  ) %>%
  addMarkers(
    data = ct_libraries,
    group = "Libraries",
    options = pathOptions(pane = "points"),
    icon = libraryIcon,
    popup = paste0("<font size = 2>", ct_libraries_df$library_name)
  ) %>%
  addMarkers(
    data = ct_schools_sp,
    group = "Schools",
    options = pathOptions(pane = "points"),
    icon = schoolIcon,
    popup = paste0("<font size = 2>", ct_schools_sp$name)
  ) %>%
  hideGroup("Towns") %>%
  hideGroup("Counties") %>%
  hideGroup("Schools") %>%
  hideGroup("Libraries") %>%
  addLegend(
    pal = binpal,
    values = htc_ct_tracts$Mail_Return_Rate_CEN_2010,
    position = "bottomright",
    title = "Census 2010 Mail Return Rate<br>Hard to Count Census Tracts",
    labFormat = labelFormat(suffix = "%")
  ) %>%
  addLayersControl(
    overlayGroups = c("Census tracts", "Towns", "Counties", "Libraries", "Schools"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  layersControlOptions(autoZIndex = TRUE)
ct_htc_map
