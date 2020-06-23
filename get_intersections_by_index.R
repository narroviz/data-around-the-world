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

# Parse arguments
args <- commandArgs(TRUE)
min_index <- 0   # args[1]
max_index <- 10800 # args[2]

# Add and output maritime/country intersection information into s3 JSON
for(i in min_index:max_index) {
  # TODO: This index fails geometric clipping
  if (i == 7781) {
    i = 7780
  }
  print(paste("Adding maritime/country intersection data for index: ", i, sep=""))
  intersections <- get_intersections_by_index(i, maritime, countries)
  elevation_distance <- get_elevation_distance_by_index(i)
  combined_json <- combine_json_files(intersections, elevation_distance)
  output_to_s3(combined_json, i)
}
