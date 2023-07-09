
library(tidyverse) # for data manipulation


# Read Endangered Languages Data ------------------------------------------

# Collect/read the data
source('data_01_clean_languages.R')



# Read Country-Level Statistics Data --------------------------------------


raw_data_countries <- read_csv('https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/Countries.csv') |>
  # clean the column names: remove spacing and special characters
  janitor::clean_names()  |>
  
  # convert values of all character columns to lower case
  mutate_if(is.character, tolower) |>
  
  # remove white-spaces from the values of all character columns
  mutate_if(is.character, str_squish)


# https://www.nature.com/articles/s41559-021-01604-y
raw_data_soc_economic <- read_csv('https://raw.githubusercontent.com/rdinnager/language_endangerment/main/data/SocEco/LangEndang_SoEcData_v4.csv') |>
  # clean the column names: remove spacing and special characters
  janitor::clean_names()  |>
  
  # convert values of all character columns to lower case
  mutate_if(is.character, tolower) |>
  
  # remove white-spaces from the values of all character columns
  mutate_if(is.character, str_squish) |>
  
  dplyr::select(
    iso_a3, 
    mean_yr_school_10yr_median,
    minority_ed_policy,
    ed_sp_prm_pc_gdp_10yrmed,
    ed_sp_sec_pc_gdp_10yrmed,
    ed_sp_tert_pc_gdp_10yrmed,
    urban_pop_pc_change_1960_2017
  ) |> drop_na(iso_a3)





combined_data <- raw_data_languages |>
  left_join(
    raw_data_countries,
    by = c("countries" = "name")
  ) |>
  left_join(
    raw_data_soc_economic,
    by = c("iso639_3_codes" = "iso_a3")
  )



locations_data <- combined_data |>
  dplyr::select(id, latitude, longitude) |>
  distinct() |>
  drop_na()


# https://gist.github.com/curran/13d30e855d48cdd6f22acdf0afe27286

path_major_cities <- "https://gist.githubusercontent.com/curran/13d30e855d48cdd6f22acdf0afe27286/raw/0635f14817ec634833bb904a47594cc2f5f9dbf8/worldcities_clean.csv"

raw_major_city <- read_csv(path_major_cities) |>
  dplyr::select(lat, lng) %>%
  distinct() |>
  drop_na()

colnames(raw_major_city) <- c("lat", "lon")


path_capital_city <- "https://gist.githubusercontent.com/ofou/df09a6834a8421b4f376c875194915c9/raw/355eb56e164ddc3cd1a9467c524422cb674e71a9/country-capital-lat-long-population.csv"

raw_captial_city <- read_csv(path_capital_city) |>
  dplyr::select(Latitude,Longitude) %>%
  distinct() |>
  drop_na()

colnames(raw_captial_city) <- c("lat", "lon")


library(sf)
library(geosphere)

# Function to compute proximity to the nearest major city
compute_proximity <- function(df_major_city, latitude, longitude) {
  
  # Convert the input coordinates to a spatial point
  input_point <- data.frame(lat = latitude, lon = longitude)
  input_point <- st_as_sf(input_point, coords = c("lon", "lat"), crs = 4326)
  
  # Convert the cities dataset to spatial points
  cities_points <- st_as_sf(df_major_city, coords = c("lon", "lat"), crs = 4326)
  
  # Compute the distances between the input point and all cities
  distances <- st_distance(input_point, cities_points)
  
  # Find the index of the nearest city
  nearest_city_index <- which.min(distances)
  
  # Return the nearest distance in km
  return(as.numeric(distances[nearest_city_index]))
}

# Example usage
# compute_proximity(df_major_city = raw_major_city,latitude = 40.7128, longitude = -74.0060)


# Apply function on location data

# locations_data <- locations_data |>
#   mutate(
#     proximity_to_major_city = pmap_dbl(
#       .l = list(
#         df_major_city = list(raw_major_city),
#         latitude = latitude, 
#         longitude = longitude
#       ),
#       .f = compute_proximity
#     )
#   )


locations_data <- locations_data |>
  mutate(
    proximity_to_capital_city = pmap_dbl(
      .l = list(
        df_major_city = list(raw_captial_city),
        latitude = latitude, 
        longitude = longitude
      ),
      .f = compute_proximity
    )
  )


# vroom::vroom_write(locations_data, 'locations_data_with_proximity.csv', delim = ',')


combined_data <- combined_data |>
  left_join(
    locations_data,
    by = c("id" = "id", "latitude" = "latitude", "longitude"="longitude")
  ) |>
  mutate(
    # proximity_to_major_city = proximity_to_major_city/1000000,
    proximity_to_capital_city = proximity_to_capital_city/1000000
  )


the_levels_of_endangerment = c("vulnerable", 
                               "definitely endangered",
                               "severely endangered",
                               "critically endangered",
                               "extinct")

combined_data <- combined_data |>
  mutate(
    # Creating a numeric version of degree of endangerment
    degree_of_endangerment_numeric = case_when(
      degree_of_endangerment == "vulnerable" ~ 1,
      degree_of_endangerment == "definitely endangered" ~ 2,
      degree_of_endangerment == "severely endangered" ~ 3,
      degree_of_endangerment == "critically endangered" ~ 4,
      degree_of_endangerment == "extinct" ~ 5
    ),
    #creating an ordred factor version of degree of endangerment 
    degree_of_endangerment_factor = factor(degree_of_endangerment,
                                           levels = the_levels_of_endangerment,
                                           ordered = TRUE)
    
  )


covariates <- c(
  'literacy',
  'infant_mortality',
  'agriculture',
  'proximity_to_capital_city',
  'minority_ed_policy',
  'urban_pop_pc_change_1960_2017'
)


rm(list = ls()[!ls() %in% c("combined_data", "country_doe",
                            "country_doe_orig", "covariates", "getMapGepProp")])

saveRDS(combined_data, "combined_data.Rds")
saveRDS(country_doe_orig, "country_doe_orig.Rds")
saveRDS(country_doe, "country_doe.Rds")
saveRDS(covariates, "covariates.Rds")
saveRDS(getMapGepProp, "getMapGepProp.Rds")


