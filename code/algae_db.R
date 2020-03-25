library(tidyverse)
library(readxl)
library(taxize)

db <- read_csv("data/2019Nov29_algae.csv")

pcgs <- db %>%
  filter(!is.na(Phylum)) %>% 
  select(Phylum, Class, Genus) %>%
  mutate(Genus = str_to_title(Genus)) %>% 
  unique() %>% 
  arrange(Phylum, Class)

gsa <- db %>%
  filter(!is.na(`Species Author`)) %>% 
  select(Genus, Species,`Species Author`, Subspecies, `Subspecies Author`, Variety, `Variety Author`) %>% 
  mutate(`taxon name for relation` = paste(Genus, Species),
         `Species Author` = trimws(`Species Author`)) %>% 
  unique() %>% 
  arrange(Genus)

pcgsa <- db %>%
  filter(!is.na(`Species Author`)) %>% 
  select(Phylum, Class, Genus, Species,`Species Author`, Subspecies, `Subspecies Author`, Variety, `Variety Author`) %>% 
  mutate(`taxon name for relation` = paste(Genus, Species),
         `Species Author` = trimws(`Species Author`)) %>% 
  unique() %>% 
  arrange(Genus)

itis_nm <- itis_terms(gsa$`taxon name for relation`[1:50])

data <- map(itis_nm,"author")

e_taxon <- tibble(`GenusSpecies` = " ",`Author` = " ")
# put everything back together
for(i in 1:length(itis_nm)) {
  if(nrow(itis_nm[[i]]) != 0) {
    species <- itis_nm[[i]]
    
    e_taxon <- e_taxon %>%  add_row(
      GenusSpecies = itis_nm[[i]]$scientificName,
      Author = itis_nm[[i]]$author)
  }
  else{e_taxon <- e_taxon %>%  add_row(Author = NA)}
}

test <- cbind(gsa[1:50,], e_taxon[2:51,])

f <- list(gsa,pcgs,pcgsa)
file_nm <- c("algae_genus.csv","algae_pcgs.csv","algae_pcgsa.csv")
map2(f, file_nm, ~write_csv(.x,paste0("results/",.y), na = ""))

#For example - Acrosiphonia	spinecens and Acrosiphonia spinescens

#Acrosiphonia	coalita	(Rupr.) Scagel, Garbary, Golden & M.W. Hawkes	NA
#Acrosiphonia	coalita	(Ruprecht) Scagel, Garba
#Helping Sandra weed out most of the duplicates in the list and focus on the truly issue species

