# Examining data from Paul Hacker
# Robin Bradley
# robin.bradley@ubc.ca
# Created: 23 February 2026


# Set up ----
setwd("C:/Users/Robin/Documents/School/Williams Lab/cowichan-diversity")

# packages


# read in data
paul.dat <- read.csv("00_rawdata/cabo-leaf-CN-species.csv")

# Data Cleaning ----
clean.dat <- paul.dat %>%
  select(-c(2, 3, 4, 5, 13, 14, 15, 16))%>%
  filter(!(species %in% c("Camassia leichtlinii", "Festuca idahoensis Elmer",
                        "Polystichum munitum (Kaulfuss) C. Presl", "Holodiscus discolor (Pursh) Maximowicz",
                        "Oemleria cerasiformis (Torrey & A. Gray ex Hooker & Arnott) J.W. Landon", 
                        "Rosa nutkana C. Presl", "Crataegus monogyna Jacquin", 
                        "Sericocarpus rigidus Lindley", "Cytisus scoparius (Linnaeus) Link",
                        "Quercus garryana Douglas ex Hooker")))%>%
  mutate(species_code = case_when(
    species == "Camassia quamash" ~ "CAQU",
    species == "Plectritis congesta" ~ "PLCO",
    species == "Lomatium utriculatum" ~"LOUT",
    species == "Dactylis glomerata" ~ "DAGL",
    species == "Lomatium utriculatum (Nuttall ex Torrey & A. Gray) J.M. Coulter & Rose" ~ "LOUT",
    species == "Dactylis glomerata Linnaeus" ~ "DAGL",
    species == "Plectritis congesta (Lindley) de Candolle" ~ "PLCO",
    species == "Bromus sterilis Linnaeus" ~ "BRST",
    species == "Bromus sitchensis var. carinatus (Hooker & Arnott) R.E. Brainerd & Otting" ~ "BRCA",
    species == "Claytonia perfoliata Donn ex Willdenow" ~ "CLPE",
    species == "Symphoricarpos albus (Linnaeus) S.F. Blake" ~ "SYAL",
    species == "Berberis aquifolium Pursh" ~ "BEAQ",
    species == "Sanicula crassicaulis Poeppig ex de Candolle" ~ "SACR",
    species == "Sanicula crassicaulis Poeppig ex de Candolle var. crassicaulis" ~ "SACR",
    species == "Vicia sativa Linnaeus" ~ "VISA",
    species == "Lathyrus sphaericus Retzius" ~ "LASP",
    species == "Poa pratensis Linnaeus" ~ "POPR",
    TRUE ~ "this is wrong"
  ))


# Exploratory plots
ggplot(clean.dat,aes(x=species_code, y=N_perc))+
  geom_boxplot()+
  geom_jitter()+
  ylab("%N")+
  xlab("Species")+
  theme_classic()
  