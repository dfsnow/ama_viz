#devtools::install_github("Tazinho/snakecase")

library(snakecase)
library(tidyverse)

# Get list of existing environment, this file will delete all the temp
# data frames it uses when it's finished
current_env <- ls()

########################################
#####   File Loading and Cleanup   #####
########################################

# Load the AMA data and convert column names to snake case
ama <- read_csv("data/QUO-10413-R2M6S8.txt")
colnames(ama) <- to_snake_case(colnames(ama))

# Drop columns that contain only NAs (no data)
ama <- ama %>% select_if(function(x) any(!is.na(x)))

########################################
#####    Merge Missing Geodata     #####
########################################

# Read the missing geodata from CSV
missing_geodata <- read_csv("data/access_missing_geocoded_2010_20180822.csv") %>%
  select(research_id, grep("?geoid", colnames(.)))

# Merge to AMA dataset
ama <- ama %>%
  left_join(missing_geodata, by = "research_id")

# Fill in remaining geoids from the AMA data
ama$geoid[is.na(ama$geoid)] <- paste0(
  ama$`fips_state`, ama$`fips_county`,
  ama$`census_tract`, ama$`census_suffix`
  )[is.na(ama$geoid)]

ama$`2003_geoid`[is.na(ama$`2003_geoid`)] <- paste0(
  ama$`2003_fips_state`, ama$`2003_fips_county`,
  ama$`2003_census_tract`, ama$`2003_census_suffix`
)[is.na(ama$`2003_geoid`)]

ama$`2008_geoid`[is.na(ama$`2008_geoid`)] <- paste0(
  ama$`2008_fips_state`, ama$`2008_fips_county`,
  ama$`2008_census_tract`, ama$`2008_census_suffix`
)[is.na(ama$`2008_geoid`)]

ama$`2013_geoid`[is.na(ama$`2013_geoid`)] <- paste0(
  ama$`2013_fips_state`, ama$`2013_fips_county`,
  ama$`2013_census_tract`, ama$`2013_census_suffix`
)[is.na(ama$`2013_geoid`)]

# Clean up spots where pasted NAs
ama <- ama %>%
  mutate(geoid = na_if(geoid, "NANANANA"),
         `2003_geoid` = na_if(`2003_geoid`, "NANANANA"),
         `2008_geoid` = na_if(`2008_geoid`, "NANANANA"),
         `2013_geoid` = na_if(`2013_geoid`, "NANANANA"))

########################################
#####     Merging on table data    #####
########################################

# Load med school data for merging with AMA
ama_med_schools <- read_csv("data/access_med_cleaned_2015_20180905.csv") %>%
  select(med_code, med_address, med_geoid, med_lon, med_lat, med_mcat_score) 

# Load birth city data for merging with AMA
ama_us_cities <- read_csv("data/access_birth_cleaned_2010_20180815.csv") %>%
  select(birth_city, birth_state_abb, birth_country,
         birth_lat, birth_lon, birth_geoid)

# Load residency data for merging with AMA
ama_residency <- read_csv("data/access_res_cleaned_2010_20180815.csv") %>%
  select(res_num, res_lat, res_lon, res_geoid)

# Load census tract centroids
centroids <- read_csv("data/tract_centroids_2010.csv") %>%
  mutate(geoid = paste0(state, county, tract)) %>%
  select(geoid, lat, lon)

# Join all loaded data frames together, see notes for merge statistics
ama_merged <- ama %>%
  left_join(
    ama_us_cities,
    by = c("birth_city", "birth_state" = "birth_state_abb", "birth_country")) %>%
  left_join(ama_residency, by = c("med_training_institution_code" = "res_num")) %>%
  left_join(ama_med_schools, by = c("med_school_id" = "med_code")) %>%
  left_join(centroids, by = c("geoid" = "geoid"))

# Replace all NAs in the geoid column
ama_merged$geoid[grep("NA", ama_merged$geoid)] <- NA

########################################
#####    Merging AHRF/PCSA data    #####
########################################

# Loading AHRF and PCSA flags from Yukako
ahrf_pcsa_dict <- read_csv("data/ahrf_pcsa_specialties_dict.csv")

# Merge on AHRF and PCSA flags and specialty codes
ama_merged <- ama_merged %>%
  left_join(
    filter(ahrf_pcsa_dict, ahrf_pcsa_type == "AHRF") %>%
      select(-ahrf_pcsa_description),
    by = c("primary_specialty" = "ahrf_pcsa_code")
    ) %>%
  mutate(ahrf_flag = ifelse(ahrf_pcsa_type == "AHRF", 1, 0)) %>%
  rename(ahrf_doctor_type = ahrf_pcsa_doctor_type) %>%
  left_join(
    filter(ahrf_pcsa_dict, ahrf_pcsa_type == "PCSA") %>%
      select(-ahrf_pcsa_description),
    by = c("primary_specialty" = "ahrf_pcsa_code")
  ) %>%
  mutate(pcsa_flag = ifelse(ahrf_pcsa_type.y == "PCSA", 1, 0)) %>%
  rename(pcsa_doctor_type = ahrf_pcsa_doctor_type) %>%
  select(-ahrf_pcsa_type.x, -ahrf_pcsa_type.y) %>%
  mutate(
    ahrf_flag = tidyr::replace_na(.$ahrf_flag, 0),
    pcsa_flag = tidyr::replace_na(.$pcsa_flag, 0)
    )

# Add nonclassified and federal flags
fed_codes <- c("080","081","082","083","084","085","086",
               "090","091","092","093","094","095","096")

ama_merged <- ama_merged %>%
  mutate(
    nonclassified_flag = ifelse(tops == "NC" | present_employment == "110", 1, 0)
    ) %>%
  mutate(
    federal_flag = ifelse(present_employment %in% fed_codes, 1, 0)
    )

########################################
### Merging CBSA/Rho classification  ###
########################################

# Loading QCBSA and QRHO files
qcbsa <- read_csv("data/tract_qcbsa_2015.csv") %>% select(geoid, qcbsa)
qrho  <- read_csv("data/county_qrho_2010.csv") %>%
  mutate(county_geoid = paste0(state, county)) %>%
  select(county_geoid, qrho)

# Create county FIPS codes for merging QRHO
ama_merged <- ama_merged %>%
  mutate(`2003_geoid_county_tmp` = paste0(`2003_fips_state`, `2003_fips_county`)) %>%
  mutate(`2008_geoid_county_tmp` = paste0(`2008_fips_state`, `2008_fips_county`)) %>%
  mutate(`2013_geoid_county_tmp` = paste0(`2013_fips_state`, `2013_fips_county`))

for (i in grep("?geoid$", colnames(ama_merged), value = T)){
  .varname <- paste0(i, "_county_tmp")
  ama_merged[[.varname]] <- str_sub(ama_merged[[i]], 0, 5)
}

# Merge on QRHO, then remove temp fips codes
for (i in grep("?_county_tmp$", colnames(ama_merged), value = T)){
  .tmp <- qrho
  colnames(.tmp)[1] <- i
  colnames(.tmp)[2] <- paste0(str_split(i, "_")[[1]][1], "_qrho")
  ama_merged <- ama_merged %>% left_join(.tmp, by = i)
  ama_merged[[i]] <- NULL
}

# Merge QCBSA using existings geoid cols
for (i in grep("?geoid$", colnames(ama_merged), value = T)){
  .tmp <- qcbsa
  colnames(.tmp)[1] <- i
  colnames(.tmp)[2] <- paste0(str_split(i, "_")[[1]][1], "_qcbsa")
  ama_merged <- ama_merged %>% left_join(.tmp, by = i)
}

########################################
###      Merging access scores       ###
########################################

access_score <- read_csv(
  "data/access_tract_costs_2010_20180815.csv",
  col_types = cols(geoid = col_character())
  ) %>%
  mutate(geoid = str_pad(geoid, 11, "left", "0")) %>%
  select(geoid, agg_cost) %>%
  rename(
    access_geoid = geoid,
    access_score = agg_cost)

# Merge access scores
for (i in grep("?geoid$", colnames(ama_merged), value = T)){
  .tmp <- access_score
  colnames(.tmp)[1] <- i
  colnames(.tmp)[2] <- paste0(str_split(i, "_")[[1]][1], "_access_score")
  ama_merged <- ama_merged %>% left_join(.tmp, by = i)
}

########################################
#####           Cleanup            #####
########################################

# Clean up any created temp files
rm(list = ls()[!ls() %in% c(current_env, "ama_merged")])


