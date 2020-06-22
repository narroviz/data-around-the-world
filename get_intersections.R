library(shp2graph)
library(RCurl)
library(geosphere)
library(lubridate)
library(GISTools)
library(rgdal)
library(rjson)
library(raster)
library(sp)
library(RCurl)
library(jsonlite)
library("aws.s3")

IS_NEARBY_THRESHOLD = 20000 # 20 km
LONGITUDE_INCREMENT = 0.0166651236
LOCAL_DIRECTORY_PATH <- Sys.getenv("LOCAL_DIRECTORY")
AWS_ACCESS_KEY <-
AWS_SECRET_KEY <- 
S3_BASE_URL <- "https://article-data.s3.us-east-2.amazonaws.com/"
S3_BUCKET <- 'article-data'
S3_FOLDER <- 'around-the-world'

setwd(LOCAL_DIRECTORY_PATH)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

get_antipode_longitude <- function(longitude) {
  antipode_longitude <- (180 - abs(longitude))
  if (longitude > 0) {
    antipode_longitude <- -1 * antipode_longitude
  }
  return(antipode_longitude)
}

get_clipped_shp <- function(shp, longitude_start, longitude_end, latitude_start=-90, latitude_end=90) {
  clip_bbox <- data.frame(x=c(longitude_start, longitude_end), y=c(latitude_start, latitude_end))
  clipped_shp <- gClip(shp, clip_bbox)
  return(clipped_shp)
}

get_shapefile_segments <- function(shp, names, territories, countries, types, longitude) {
  segment_names <- c()
  segment_territories <- c()
  segment_countries <- c()
  segment_types <- c()
  start_pts <- c()
  end_pts <- c()
  longitudes <- c()
  
  for(i in shp@plotOrder) {
    polygon <- shp@polygons[[i]]
    subpolygons <- polygon@Polygons
    id <- strsplit(polygon@ID, ' ')[[1]][1]
    id_row_index <- as.numeric(id) + 1
    name <- as.character(names[id_row_index])
    territory <- as.character(territories[id_row_index])
    country <- as.character(countries[id_row_index])
    # Convert codes to proper ISO3 (Natural Earth has strange, unexplained, other codes)
    if (country == "CH1") {
      country <- 'CHN'
    } else if (country == "IS1") {
      country <- 'ISR'
    } else if (country == "FR1") {
      country <- 'FRA'
    } else if (country == "NL1") {
      country <- 'NLD'
    } else if (country == "FI1") {
      country <- 'FIN'
    } else if (country == "DN1") {
      country <- 'DNK'
    } else if (country == "GB1") {
      country <- 'GBR'
    } else if (country == "US1") {
      country <- 'USA'
    } else if (country == "AU1") {
      country <- 'AUS'
    } else if (country == "NZ1") {
      country <- 'NZL'
    }
    type <- as.character(types[id_row_index])
    
    for (j in 1:length(subpolygons)) {
      subpolygon <- subpolygons[[j]]
      subpolygon_latitudes <- subpolygon@coords[,2]
      start <- min(subpolygon_latitudes)
      end <- max(subpolygon_latitudes)
      segment_names <- c(segment_names, name)
      segment_territories <- c(segment_territories, territory)
      segment_countries <- c(segment_countries, country)
      segment_types <- c(segment_types, type)
      start_pts <- c(start_pts, start)
      end_pts <- c(end_pts, end)
      longitudes <- c(longitudes, longitude)
    }
  }
  shapefile_segment_df <- data.frame(
    name=segment_names,
    territory=segment_territories,
    country=segment_countries,
    type=segment_types,
    start_latitude=start_pts,
    end_latitude=end_pts,
    longitude=longitudes
  )
  return(shapefile_segment_df)
}

filter_segments <- function(segment_df) {
  filtered_df <- segment_df
  unacceptable_types <- c("Joint regime", "Overlapping claim", "Disputed", "Lease", "Indeterminate")
  unacceptable_indices <- which(segment_df$type %in% unacceptable_types & segment_df$country != "ATA")
  if (length(unacceptable_indices) > 0) {
    filtered_df <- segment_df[-unacceptable_indices,]
  }
  return(filtered_df)
}

are_points_nearby <- function(p1, p2) {
  # If points are within 20km (globally nearby)
  return(distHaversine(p1, p2) < IS_NEARBY_THRESHOLD)
}

merge_segments <- function(segment_df, longitude) {
  # Create a faux first row to ease rbind
  merged_df <- segment_df[1,]
  countries <- unique(segment_df$country)
  for(i in 1:length(countries)) {
    country <- countries[i]
    country_indices <- which(segment_df$country == country)
    country_df <- segment_df[country_indices,]
    country_df <- country_df[order(country_df$start_latitude),]
    if (nrow(country_df) == 1) {
      merged_df <- rbind(merged_df, country_df)
    } else {
      merged_country_df <- country_df[1,]
      num_merged_rows <- 0
      current_merge_row <- country_df[1,]
      for(j in 1:nrow(country_df)) {
        if (j < nrow(country_df)) {
          current_end <- current_merge_row$end_latitude
          next_start <- country_df[j+1,]$start_latitude
          next_end <- country_df[j+1,]$end_latitude

          rows_are_overlapping <- current_end > next_start
          rows_are_nearby <- are_points_nearby(c(longitude, current_end), c(longitude, next_start))

          if (rows_are_overlapping | rows_are_nearby) {
            if (next_end > current_end) {
              current_merge_row$end_latitude <- next_end
            }
          } else {
            num_merged_rows = num_merged_rows + 1
            merged_country_df[num_merged_rows,] <- current_merge_row
            current_merge_row <- country_df[j+1,]
          }
        } else {
          num_merged_rows = num_merged_rows + 1
          merged_country_df[num_merged_rows,] <- current_merge_row
        }
        # Add in the final "current merge row"
      }
      merged_df <- rbind(merged_df, merged_country_df)
    }
  }
  # Delete the faux first row
  merged_df <- merged_df[-1,]
  merged_df <- merged_df[order(merged_df$start_latitude),]
  return(merged_df)
}

get_intersection_segments <- function(longitude, longitude_end, water, land) {
  clipped_water <- get_clipped_shp(water, longitude, longitude_end)
  clipped_land <- get_clipped_shp(land, longitude, longitude_end)

  water_segments <- get_shapefile_segments(clipped_water, water@data$TERRITORY1, water@data$ISO_TER1, water@data$ISO_SOV1, water@data$POL_TYPE, longitude)
  land_segments <- get_shapefile_segments(clipped_land, land@data$ADMIN, land@data$ISO_A3, land@data$SOV_A3, land@data$TYPE, longitude)
  all <- rbind(water_segments, land_segments)
  filtered <- filter_segments(all)
  ordered <- filtered[order(filtered$start_latitude),]
  merged <- merge_segments(ordered, longitude)
  return(merged)
}

get_intersections_by_index <- function(index, water, land) {
  lon = -180 + (index)*LONGITUDE_INCREMENT
  lon_end = -180 + (index+1)*LONGITUDE_INCREMENT
  lon_intersections <- get_intersection_segments(lon, lon_end, water, land)
  
  inv_lon = get_antipode_longitude(lon)
  inv_lon_end = get_antipode_longitude(lon_end)
  inv_intersections <- get_intersection_segments(inv_lon, inv_lon_end, water, land)
  
  all_intersections <- rbind(lon_intersections, inv_intersections[order(inv_intersections$start_latitude, decreasing=TRUE),])
  return(all_intersections)
}

get_elevation_distance_by_index <- function(index) {
  filename = paste(index, ".json", sep="")
  longitude_url <- paste(S3_BASE_URL, S3_FOLDER, "/", filename, sep="")
  longitude_json <- fromJSON(longitude_url)
  return(longitude_json)
}

combine_json_files <- function(all_intersections, longitude_json) {
  countries_json <- toJSON(all_intersections)
  distance_json <- toJSON(longitude_json$distance)
  elevation_json <- toJSON(longitude_json$elevation)
  combined_json <- paste0('{"elevation":', elevation_json, ',"distance":', distance_json, ',"countries":', countries_json, '}')
  return(combined_json)
}

output_json_object_to_s3_by_index <- function(json, index, local=FALSE) {
  output_filename = paste(index, ".json", sep="")
  output_filepath = paste(S3_FOLDER, '/', output_filename, sep="")
  if (local) {
    local_filepath = paste(LOCAL_DIRECTORY_PATH, "/data/json/", output_filename, sep="")
    write(json, file=local_filepath)
  }
  s3write_using(
    x = json, 
    FUN = write, #jsonlite function "write"
    bucket = S3_BUCKET,
    object = output_filepath,
    opts = list(acl = "public-read", multipart = FALSE, verbose = T, show_progress = T)
  )
}
