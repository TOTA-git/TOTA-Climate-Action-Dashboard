# Data refresh job.
#
# Fetches every externally-sourced dataset the app depends on and writes
# each one to data/cache/. Each dataset is fetched and written
# independently: if one source fails (site down, API change, network
# blip), that dataset's existing cache file is left untouched and the
# rest of the refresh continues. global.R only ever reads from
# data/cache/, so the app always has the most recent successful pull
# of each dataset, even if today's refresh partially failed.
#
# Run manually with:
#   Rscript data_refresh.R
#
# To run this once a day.
# (e.g.GitHub Actions)

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(geojsonsf)
library(rvest)
library(httr2)
library(stringr)
library(purrr)
library(weathercan)
library(lutz)

source("R/data_helpers.R")

clear_refresh_log()

refresh_dataset <- function(name, fetch, write) {
  message("Refreshing ", name, "...")
  tryCatch(
    {
      value <- fetch()
      write(value)
      log_refresh(name, "success", n_rows = NROW(value))
      message("  OK (", NROW(value), " rows)")
      TRUE
    },
    error = function(e) {
      log_refresh(name, "failure", message = conditionMessage(e))
      message("  FAILED - keeping previous cache: ", conditionMessage(e))
      FALSE
    }
  )
}

# DROUGHT LEVELS (ArcGIS FeatureServer, current snapshot) -------------------
refresh_dataset(
  "drought",
  function() {
    drought_url <- "https://services1.arcgis.com/xeMpV7tU1t4KD3Ei/arcgis/rest/services/British_Columbia_Drought_Levels_(Edit)_view/FeatureServer/27/query?f=geojson&where=(BasinName%20IN%20('Okanagan'%2C%20'South%20Thompson'%2C%20'North%20Thompson'%2C%20'Lower%20Thompson'%2C%20'Kettle%20River'%2C%20'Nicola'%2C%20'Similkameen'))&outFields=*"
    resp <- GET(drought_url)
    if (httr::http_error(resp)) {
      stop("Drought FeatureServer returned HTTP ", status_code(resp))
    }
    geojson <- content(resp, "text", encoding = "UTF-8")
    df <- geojson_sf(geojson)
    df$Date_Modified <- as.POSIXct(
      df$Date_Modified / 1000,
      origin = "1970-01-01",
      tz = "America/Vancouver"
    )
    df
  },
  function(df) atomic_write_rds(df, file.path(CACHE_DIR, "drought.rds"))
)

# HISTORICAL DROUGHT LEVELS (Google Sheet is manually updated but automatically refreshed) ----------------------------------
refresh_dataset(
  "drought_hist",
  function() {
    read_csv(
      Sys.getenv("GOOGLE_SHEET_DROUGHT_URL"),
      show_col_types = FALSE
    ) %>%
      mutate(
        Start_Date = as.Date(
          substr(Start_Date, 1, 10),
          format = "%Y/%m/%d"
        ),
        DroughtLevel = case_when(
          DroughtLevel == "Not updated outside of core drought season" ~ 6,
          TRUE ~ as.numeric(DroughtLevel)
        )
      ) %>%
      select(BasinName, Start_Date, DroughtLevel) %>%
      arrange(BasinName, Start_Date)
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "drought_hist.csv"))
)

# WILDFIRE LOCATIONS 2022-2025 ------------------------------------------------------------------------------------
refresh_dataset(
  "wildfire",
  function() {
    bucket_id <- "9d097106-f945-4461-ac48-494671dfe5e0"
    api <- "https://coms.api.gov.bc.ca/api/v1"
    
    # 1. List available public files
    files <- request(paste0(api, "/object")) |>
      req_url_query(
        bucketId = bucket_id,
        public = "true",
        active = "true",
        limit = 100,
        page = 1
      ) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE)
    
    files <- as.data.frame(files)
    
    # 2. Keep point KMZ files only
    points <- files |>
      filter(grepl("^BC Fire Points .*\\.kmz$", name, ignore.case = TRUE))
    
    if (nrow(points) == 0) {
      stop("No BC Fire Points KMZ files found.")
    }
    
    # 3. Download and read each file
    dir.create("data/cache/wildfires", recursive = TRUE, showWarnings = FALSE)
    
    fire_data <- lapply(seq_len(nrow(points)), function(i) {
      
      file_name <- points$name[i]
      file_id <- points$id[i]
      
      kmz_path <- file.path("data/cache/wildfires", file_name)
      extract_dir <- file.path(
        "data/cache/wildfires",
        tools::file_path_sans_ext(file_name)
      )
      
      # Download
      download.file(
        paste0(api, "/object/", file_id, "?download=proxy"),
        kmz_path,
        mode = "wb",
        quiet = TRUE
      )
      
      # Extract KML
      dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
      unzip(kmz_path, exdir = extract_dir)
      
      kml <- list.files(
        extract_dir,
        pattern = "\\.kml$",
        full.names = TRUE,
        recursive = TRUE
      )
      
      if (length(kml) == 0) stop("No KML found in ", file_name)
      
      st_read(kml[1], quiet = TRUE)
    })
    
    # 4. Combine all point files
    wildfires <- bind_rows(fire_data)
    
    #filter for only fires in TO
    boundary <- st_read(
      "data/thompson_okanagan_boundary.geojson",
      quiet = TRUE
    ) |>
      st_transform(st_crs(wildfires))

    wildfires_TO <- st_filter(
      wildfires,
      boundary,
      .predicate = st_intersects
    )
    
    if (!"Description" %in% names(wildfires_TO)) {
      stop("KMZ files do not contain a Description field.")
    }
    
    wildfire_details <- map_dfr(
      seq_len(nrow(wildfires_TO)),
      function(i) {
        
        x <- wildfires_TO$Description[i]
        
        if (is.na(x) || !nzchar(x)) {
          return(tibble())
        }
        
        rows <- read_html(x) |>
          html_elements("tr")
        
        details <- map_dfr(rows, function(row) {
          
          cells <- html_elements(row, "td") |>
            html_text2()
          
          if (length(cells) == 2) {
            tibble(
              field = cells[1],
              value = cells[2]
            )
          }
        })
        
        if (nrow(details) == 0) {
          return(tibble())
        }
        
        details |>
          pivot_wider(
            names_from = field,
            values_from = value,
            values_fn = first
          )
      }
    )
    
    # 7. Combine spatial data and extracted attributes
    if (nrow(wildfire_details) != nrow(wildfires_TO)) {
      stop("Wildfire detail extraction produced an unexpected number of rows.")
    }
    
    detail_cols <- setdiff(
      names(wildfire_details),
      names(wildfires_TO)
    )
    
    wildfires_clean <- bind_cols(
      wildfires_TO |> select(-Description),
      wildfire_details |> select(all_of(detail_cols))
    )
    
    # 8. Check required fields
    required_fields <- c(
      "WILDFIRE_YEAR",
      "FIRE_SIZE_HA",
      "IGNITION DATE",
      "LATITUDE",
      "LONGITUDE"
    )
    
    missing_fields <- setdiff(
      required_fields,
      names(wildfires_clean)
    )
    
    if (length(missing_fields) > 0) {
      stop(
        "Missing wildfire fields: ",
        paste(missing_fields, collapse = ", ")
      )
    }
    
    # 9. Standardize data types
    wildfires_clean <- wildfires_clean |>
      mutate(
        WILDFIRE_YEAR = as.integer(WILDFIRE_YEAR),
        FIRE_SIZE_HA = as.numeric(FIRE_SIZE_HA),
        LATITUDE = as.numeric(LATITUDE),
        LONGITUDE = as.numeric(LONGITUDE),
        `IGNITION DATE` = as.POSIXct(
          `IGNITION DATE`,
          format = "%Y-%m-%d %I:%M:%S %p",
          tz = "America/Vancouver"
        )
      )
    
    message("Successfully processed ", nrow(wildfires_clean), " wildfires.")
    
    wildfires_clean
  },
  function(df) atomic_write_rds(df, file.path(CACHE_DIR, "wildfire.rds"))
)

refresh_dataset(
  "temperature",
  function() {
    stations_dl()
    
    df <- weather_dl(
      station_ids = c(48369, 50269, 46987, 51423),
      start = "2013-01-01",
      end = Sys.Date(),
      interval = "day"
    )
    
    df <- df %>%
      select(station_name, station_id, station_operator, lat, lon, date, year, month, day, max_temp, min_temp, mean_temp, total_precip)
    
    df$station_name <- str_to_title(df$station_name)
    df$station_name <- word(df$station_name, 1)
    df
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "temperature.csv"))
)

message("Data refresh complete. See ", file.path(CACHE_DIR, "refresh_log.csv"), " for a run history.")