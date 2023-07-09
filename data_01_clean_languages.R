library(tidyverse) # for data manipulation


# https://www.kaggle.com/code/xhoong/languages-extinction-level-to-representing-country/report

path_raw_data <- 'https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/data.csv'

raw_data_languages <- read_csv(path_raw_data) |>
  # clean the column names: remove spacing and special characters
  janitor::clean_names()


raw_data_languages <- raw_data_languages |>
  separate_rows(countries, country_codes_alpha_3, sep = ',') |>
  separate_rows(iso639_3_codes, sep = ',') |>
  distinct(id, countries, country_codes_alpha_3,iso639_3_codes, .keep_all = TRUE) |>
  mutate(countries = str_replace_all(countries, "\\s*\\(.*", "")) |>
  mutate_if(is.character, str_squish) 



# Using coordinates to identify single country as based reference ---------


na.coord <- raw_data_languages |> filter(is.na(longitude)) |> 
  mutate(country_codes_alpha_3_coord=country_codes_alpha_3)


getMapGepProp <- function(){
  list(
    showframe = T,
    showocean = T,
    #oceancolor = 'rgb(28,107,160)',
    oceancolor = 'rgb(222,243,246)',
    projection = list(
      type = 'orthographic',
      rotation = list(
        lon = 60,
        lat = 10)
    ),
    lonaxis =  list(
      showgrid = F,
      gridcolor = 'rgb(102, 102, 102)'
    ),
    lataxis = list(
      showgrid = F,
      gridcolor = 'rgb(102, 102, 102)'
    )
  )
}

coords2location = function(points){  
  require(sp)
  require(rworldmap)
  require(rworldxtra)
  countriesSP <- getMap(resolution='high')
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
  
  indices$ISO3
}


raw_data_languages <- raw_data_languages |> filter(!is.na(longitude)) |>
  mutate(mult= country_codes_alpha_3 =="" | grepl(",", country_codes_alpha_3),
         country_codes_alpha_3_coord=
           ifelse(mult,as.character(
             coords2location(data.frame(lng=longitude, lat=latitude))), 
             country_codes_alpha_3 ) ) |> dplyr::select(-mult)


# Correct country code SDS to SDN
sds_rec <- raw_data_languages |> 
  filter(country_codes_alpha_3_coord=="SDS") |> 
  mutate(country_codes_alpha_3_coord="SDN")


raw_data_languages <- rbind(raw_data_languages |> filter(!(id %in% sds_rec$id)), 
                            sds_rec,
                            na.coord)



# Manual lookup using Google Map
raw_data_languages[raw_data_languages$id == 262, "country_codes_alpha_3_coord"]<-"CAN"
raw_data_languages[raw_data_languages$id == 1010, "country_codes_alpha_3_coord"]<-"THA"
raw_data_languages[raw_data_languages$id == 1964, "country_codes_alpha_3_coord"]<-"VEN"



# Re-index Degree of Endangerment for languages on country boundary -------

raw_data_languages <- raw_data_languages |> 
  mutate(doe_num=as.numeric(factor(degree_of_endangerment, 
                                   levels=c("Vulnerable", "Definitely endangered", 
                                            "Severely endangered", "Critically endangered",
                                            "Extinct")))) 


country_doe<-aggregate( doe_num ~ country_codes_alpha_3, data=raw_data_languages,
                        function(x) { 
                          c(idx.med=median(x), idx.avg=round(mean(x),1), count=length(x))})


country_doe <- data.frame(country_codes=country_doe$country_codes_alpha_3,
                          degree_of_endangerment_median=country_doe$doe_num[,1],
                          degree_of_endangerment_mean=country_doe$doe_num[,2],
                          language_count=country_doe$doe_num[,3]) 

cleanCountryName <- function(country.df) {
  require(dplyr)
  require(countrycode)
  country.df<-country_doe |> 
    mutate(country.name=countrycode(country_doe$country_codes, "iso3c", "country.name", warn=F))
  country.df[country_doe$country_codes=="ANG", "Country.name"] = "Angola"
  country.df[country_doe$country_codes=="ZAI", "Country.name"] = "Democratic Republic of the Congo"
  country.df
}


country_doe_orig <- cleanCountryName(country_doe) %>% janitor::clean_names() 


country_doe <- country_doe_orig %>%
  mutate(
    country_name = coalesce(country_name, country_name_2)
  ) %>%
  dplyr::select(-country_name_2) |>
  mutate_if(is.character, tolower) |>
  mutate_if(is.character, str_squish)


raw_data_languages <- raw_data_languages |>
  mutate_if(is.factor, as.character) |>
  mutate_if(is.character, tolower) |>
  mutate_if(is.character, str_squish)
  
  



raw_data_languages <- raw_data_languages |>
  left_join(
    country_doe |> dplyr::select(country_codes, country_name),
    by = c("country_codes_alpha_3" = "country_codes")
  ) |>
  dplyr::select(-countries) |>
  rename(countries = country_name)



rm(list = ls()[!ls() %in% c("raw_data_languages", "country_doe", 
                            "country_doe_orig", "getMapGepProp")])

