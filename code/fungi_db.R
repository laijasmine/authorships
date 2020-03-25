library(tidyverse)
library(readxl)

db <- read_xlsx("data/2019Nov29_fungi.xlsx")

# Phylum Class Cabinet Autofill table based on Genus
pcgs <- db %>%
  filter(!is.na(Phylum),!is.na(Cabinet)) %>% 
  select(Phylum, Class, Genus, Cabinet) %>%
  mutate(Genus = str_to_title(Genus)) %>% 
  unique()

# find any specimens that span multiple cabinets
multi <- pcgs %>%  group_by(Genus) %>% 
  summarise(number = n()) %>%
  filter(number > 1)

unique_pcgs_list <- pcgs %>% 
#  filter(Genus %in% multi$Genus) %>% 
  group_by(Phylum, Class, Genus) %>% 
  summarise(Cabinet = paste(Cabinet, collapse = ", ") )

# stick the information back in

# Authorship Autofilled based on Genus Species
gsa <- db %>%
  filter(!is.na(`Species Author`)) %>% 
  select(Genus, Species,`Species Author`, Subspecies, `Subspecies Author`, Variety, `Variety Author`) %>% 
  mutate(`taxon nuame for relation` = paste(Genus, Species)) %>% 
  unique()

# Unique collectors
collect <- db %>%  
  group_by(`Primary Collector`) %>% 
  summarise(n()) %>% 
  unique()

#save the files
write_csv(unique_pcgs_list, "results/fungi_pcgs.csv")
write_csv(gsa,"results/fungi_gsa.csv")
