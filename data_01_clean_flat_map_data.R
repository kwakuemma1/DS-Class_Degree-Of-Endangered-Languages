#' Kwaku Adu
#' 6/30/2023
#' 
#' Purpose: Scripts to clean data for the project
#' Project: Interactively visualize endangered languages


# Data Manipulation
library(tidyverse) # For data manipulation
library(janitor)   # For clean column names, and other utilities
library(glue)      # String manipulation



# Read raw data
# https://www.endangeredlanguages.com/
elp_data <- vroom::vroom('database_file.csv', delim = ',') |>
  # rename the column, other name, to `name in english`
  rename(`name in english`  = `Other name` ) |>
  
  # clean column names: remove special characters, all lower case, etc.
  janitor::clean_names()


# Separate degree endangerment from the original:
# e.g. Endangered (20 percent cetain) in to 2 columns as follows:
# endangerment_degree = 'Endangered'
# confidence_in_degree = 20
elp_data <- elp_data |>
  extract(degree_of_endangerment, into = c("endangerment_degree", "confidence_in_degree"),
          regex = "(.*)\\((\\d+) percent.*", remove = FALSE) |>
  mutate(
    endangerment_degree = str_squish(endangerment_degree),
    endangerment_degree =ifelse(is.na(endangerment_degree), degree_of_endangerment, endangerment_degree),
    
    # remove all special characters like ()+?^$%#@&*; and whitespaces
    endangerment_degree = str_squish(str_replace_all(endangerment_degree, "[^A-Za-z0-9 ]", "")),
    endangerment_degree = ifelse(is.na(endangerment_degree), 'Vitality Unknown', endangerment_degree),
    endangerment_degree = ifelse(endangerment_degree == 'Dormant', 'Dormant or Extinct', endangerment_degree),
    confidence_in_degree = as.numeric(confidence_in_degree),
    confidence_in_degree = case_when(
      endangerment_degree == "Dormant or Extinct" ~ 100,
      TRUE ~ confidence_in_degree
    )
  )



# For some languages there are multiple geog coordinates seperated by ';'
# Separate those duplicates to new rows
# Then create new columns to host the latitude and longitude
elp_data <- elp_data |>
  # separate multiple geog coordinates to independent rows
  separate_rows(geographic_location, sep = ";") |>
  
  # remove trailing and leading whitespaces
  mutate(coordinates = str_trim(geographic_location)) |>
  
  # separate the geog cordinates column into 2 columns: lat and long
  separate(coordinates, into = c("latitude", "longitude"), sep = ",") |>
  
  # Remove trailing and leading white spaces and convert lat and long to numbers
  mutate(
    latitude = as.numeric(str_squish(latitude)),
    longitude = as.numeric(str_squish(longitude))
  )

# Duplicate IDs are created due to the separation of the multiple geog coordinates
# to new rows
# create a new ID column that is unique using the origina id + duplicate ordering
# using alphabetical letters
elp_data <- elp_data |>
  mutate(id = as.character(id)) |>
  group_by(id) |>
  mutate(id_no_duplicates = ifelse(duplicated(id) | duplicated(id, fromLast = TRUE), 
                                   paste0(id, "_", letters[row_number()]), id)) |>
  ungroup



elp_data_order_of_severity = c(
  "At risk",
  "Vulnerable",
  "Endangered",
  "Severely Endangered",
  "Critically Endangered",
  "Threatened",
  "Dormant or Extinct",
  "Awakening",
  "Vitality Unknown"
)

map_data <- elp_data |> 
  dplyr::select(latitude, longitude, endangerment_degree, confidence_in_degree, name_in_the_language) |>
  mutate(endangerment_degree = factor(endangerment_degree, levels = elp_data_order_of_severity)) |>
  mutate(confidence_in_degree = rescale(confidence_in_degree, to = c(5, 10)))


# rm(list = ls()[!ls() %in% c("combined_data", "country_doe", 
#                             "covariates", "getMapGepProp",
#                             "map_data")])

rm(list = ls()[!ls() %in% c("map_data")])

saveRDS(map_data, "map_data.Rds")
