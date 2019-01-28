library(tidyverse)

# Super sweet modular filter system AKA comment out the lines you don't want
ama_filtered <- ama_merged %>%
  filter(
    !tops %in% c("IN", "SR"),  # Remove inactive doctors (AHRF definition)
    dead_flag == 0,            # Remove dead doctors
    #do_flag == 0,              # Remove DOs
    yob > 1918,                # Keep only doctors less than 100 years old
    #birth_country == "US1"     # Keep only US born docs
    )
