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
clean.paul.dat <- paul.dat %>%
  select(-c(2, 3, 4, 5, 13, 14, 15, 16))%>%
  filter(!(species %in% c("Camassia leichtlinii", "Festuca idahoensis Elmer",
                        "Polystichum munitum (Kaulfuss) C. Presl", "Holodiscus discolor (Pursh) Maximowicz",
                        "Oemleria cerasiformis (Torrey & A. Gray ex Hooker & Arnott) J.W. Landon", 
                        "Rosa nutkana C. Presl", "Crataegus monogyna Jacquin", 
                        "Sericocarpus rigidus Lindley", "Cytisus scoparius (Linnaeus) Link",
                        "Quercus garryana Douglas ex Hooker")))%>%
  mutate(species = case_when(
    species == "Camassia quamash" ~ "Camassia_quamash",
    species == "Plectritis congesta" ~ "Plectritis_congesta",
    species == "Lomatium utriculatum" ~"Lomatium_utriculatum",
    species == "Dactylis glomerata" ~ "Dactylis_glomerata",
    species == "Lomatium utriculatum (Nuttall ex Torrey & A. Gray) J.M. Coulter & Rose" ~ "Lomatium_utriculatum",
    species == "Dactylis glomerata Linnaeus" ~ "Dactylis_glomerata",
    species == "Plectritis congesta (Lindley) de Candolle" ~ "Plectritis_congesta",
    species == "Bromus sterilis Linnaeus" ~ "Bromus_sterilis",
    species == "Bromus sitchensis var. carinatus (Hooker & Arnott) R.E. Brainerd & Otting" ~ "Bromus_carinatus",
    species == "Claytonia perfoliata Donn ex Willdenow" ~ "Claytonia_perfoliata",
    species == "Symphoricarpos albus (Linnaeus) S.F. Blake" ~ "Symporicarpos_albus",
    species == "Berberis aquifolium Pursh" ~ "Berberis_aquifolium",
    species == "Sanicula crassicaulis Poeppig ex de Candolle" ~ "Sanicula_crassicaulis",
    species == "Sanicula crassicaulis Poeppig ex de Candolle var. crassicaulis" ~ "Sanicula_crassicaulis",
    species == "Vicia sativa Linnaeus" ~ "Vicia_sativa",
    species == "Lathyrus sphaericus Retzius" ~ "Lathyrus_sphaericus",
    species == "Poa pratensis Linnaeus" ~ "Poa_pratensis",
    TRUE ~ "this is wrong"
  ))


# Exploratory plots
ggplot(clean.paul.dat,aes(x=species_code, y=N_perc))+
  geom_boxplot()+
  geom_jitter()+
  ylab("%N")+
  xlab("Species")+
  theme_classic()
  