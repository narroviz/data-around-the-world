# Source file dependencies
LOCAL_DIRECTORY_PATH <- Sys.getenv("LOCAL_DIRECTORY")
source(paste(LOCAL_DIRECTORY_PATH, "get_intersections.R", sep=""))
setwd(paste(LOCAL_DIRECTORY_PATH, "data/shapefiles", sep=""))

Sys.setenv(
  "AWS_ACCESS_KEY_ID" =  Sys.getenv("AWS_ACCESS_KEY"),
  "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_KEY"),
  "AWS_DEFAULT_REGION" = "us-east-2"
)

# Load shapefiles for maritime economic zones and country shapefiles
countries <- readOGR("ne_10m_admin_0_countries", "ne_10m_admin_0_countries")
maritime <- readOGR("World_EEZ_v11_20191118_LR", "eez_v11_lowres")
oceans <- readOGR("World_Seas_IHO_v1", "World_Seas")

setwd(paste(LOCAL_DIRECTORY_PATH, "data/geojson", sep=""))
indigenous_nations <- readOGR("indigenousTerritories.json", "indigenousTerritories")


# Setup retry when attempting output
output_to_s3 <- function(json, index) {
  possibleError <- tryCatch(
    output_json_object_to_s3_by_index(json, index, local=TRUE),
    error=function(e) e
  )
  if(inherits(possibleError, "error")) {
    output_json_object_to_s3_by_index(json, index, local=TRUE)
  }
}


# Removes geometric-issue polygons
indigenous_nations$Slug <- as.character(indigenous_nations$Slug)
indigenous_nations$description <- as.character(indigenous_nations$description)
indigenous_nations$Slug[which(indigenous_nations$Name == "Akokisa")] <- "akokisa" 
indigenous_nations$description[which(indigenous_nations$Name == "Akokisa")] <- "https://en.wikipedia.org/wiki/Akokisa" 
self_intersecting_polygon_slugs = c(
  "atfalati",
  "ngati-maru-hauraki",
  "southern-paiute",
  "yawarawarka",
  "ngapuhi-ngati-kahu-ki-whaingaroa",
  "ngati-paoa",
  "yinggarda",
  "southern-paiute",
  "ngati-kahu"
)
indices_to_remove <- c()
for (i in 1:length(self_intersecting_polygon_slugs)) {
  indices_to_remove <- c(indices_to_remove, which(self_intersecting_polygon_slugs[i] == indigenous_nations$Slug))
}
indices_to_remove <- c(indices_to_remove, which(is.na(indigenous_nations$Name)))
indigenous_nations_reduced <- indigenous_nations[-indices_to_remove,]
# Resetting ids after removing shapes
for (i in 0:(length(indigenous_nations_reduced)-1)) {
  indigenous_nations_reduced@polygons[[i+1]]@ID = paste(i,sep="")
}


self_intersecting_ocean_names = c(
  "Kattegat",
  "Baffin Bay",
  "Baltic Sea",
  "Ceram Sea",
  "Banda Sea",
  "Java Sea"
)
ocean_indices_to_remove <- c()
for (i in 1:length(self_intersecting_ocean_names)) {
  ocean_indices_to_remove <- c(ocean_indices_to_remove, which(self_intersecting_ocean_names[i] == oceans$NAME))
}
oceans_reduced <- oceans[-ocean_indices_to_remove,]
# Resetting Ocean Ids
for (i in 0:(length(oceans_reduced)-1)) {
  oceans_reduced@polygons[[i+1]]@ID = paste(i,sep="")
}


# Parse arguments
args <- commandArgs(TRUE)
min_index <- 0   # args[1]
max_index <- 10801 # args[2]
use_existing_countries = FALSE

# Add and output maritime/country intersection information into s3 JSON
for(i in min_index:max_index) {
  # TODO: This index fails geometric clipping
  if (i == 7781) {
    i = 7780
  }
  print(paste("Adding maritime/country intersection data for index: ", i, sep=""))
  ocean_intersections <- get_ocean_intersections_by_index(i, oceans_reduced)
  indigenous_intersections <- get_indigenous_intersections_by_index(i, indigenous_nations_reduced)
  if (use_existing_countries) {
    country_intersections = ""
  } else {
    country_intersections <- get_intersections_by_index(i, maritime, countries)
  }
  elevation_distance <- get_elevation_distance_by_index(i)
  combined_json <- combine_json_files(
    ocean_intersections,
    indigenous_intersections,
    country_intersections,
    elevation_distance,
    use_existing_countries
  )
  output_json_object_to_s3_by_index(combined_json, i, TRUE)
}

