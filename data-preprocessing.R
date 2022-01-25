library(dplyr)
library(stringr)

#Dataset with the Alpha3Code (ISO3) of all the countries 
path_iso <- 'iso3.csv'
iso_codes <- read.csv(path_iso, sep=',')

#Our dataset
path_data <- "data.csv"
data <- read.csv(path_data, sep=',')

#Converting column type_of_violence in order to have the labels names
cat_numbers <- c(1,2,3)

cat_names <- c('state-based conflict', 'non-state conflict', 'one-sided violence')

for (i in 1:length(cat_numbers)){
  data$type_of_violence[data$type_of_violence == cat_numbers[i]] <- cat_names[i]
}

#Changing some country names for the mapping of ISO3 code
old_names <- c("Bosnia-Herzegovina", "Cambodia (Kampuchea)", "DR Congo (Zaire)", "Iran", 
               "Kingdom of eSwatini (Swaziland)", "Laos", "Macedonia, FYR",
               "Madagascar (Malagasy)", "Moldova", "Myanmar (Burma)", "Russia (Soviet Union)",
               "Serbia (Yugoslavia)", "Syria", "Tanzania", "United States of America",
               "Yemen (North Yemen)", "Zimbabwe (Rhodesia)")

new_names <- c("Bosnia and Herzegovina", "Cambodia", "Congo, the Democratic Republic of the", "Iran, Islamic Republic of",
               "Swaziland", "Lao People's Democratic Republic", "Macedonia, the former Yugoslav Republic of",
               "Madagascar", "Moldova, Republic of", "Myanmar", "Russia",
               "Serbia", "Syrian Arab Republic", "Tanzania, United Republic of", "United States",
               "Yemen", "Zimbabwe")

for (i in 1:length(old_names)){
  data$country[data$country == old_names[i]] <- new_names[i]
}

#Creating a new column ISO3 in our dataset
data["ISO3"] <- iso_codes$Alpha.3.code[match(data$country, iso_codes$Country)]

#Removing some columns that we are not going to use
undesired <- c('relid', 'code_status', 'conflict_dset_id', 'conflict_new_id', 'dyad_dset_id', 'dyad_new_id',
               'dyad_name', 'side_a_dset_id', 'side_a_new_id', 'side_b_dset_id', 'side_b_new_id', 'number_of_sources',
               'source_article', 'source_office', 'source_date', 'source_headline', 'source_original', 'where_prec', 
               'where_coordinates', 'adm_1', 'adm_2', 'geom_wkt', 'priogrid_gid', 'event_clarity', 'date_prec',
               'gwnoa', 'gwnob')

data <- data %>%
  select(-one_of(undesired))

#Renaming column "best" to "death"
data <- data %>% 
  rename(death = best)

#There is a whitespace before the ISO3 code of each row, let's delete it
data$ISO3=str_replace(data$ISO3," ","")

#Exporting the new dataset
write.csv(data, 'data_preprocessed.csv')