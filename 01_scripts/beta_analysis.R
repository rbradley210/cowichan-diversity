# Script for modeling community change and beta diversity
# Robin Bradley
# robin.bradley@ubc.ca
# created: 27 Jan 2025
# last updated: 16 July 2025

# Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## packages ----
library(picante)
library(vegan)
#library(tidyverse)
library(ggplot2)
library(ggordiplots)
#library(ggvegan)
library(ggthemes)
library(ggnewscale)
library(paletteer)
library(dplyr)
library(eHOF)
library(tibble)
library(vegan3d)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/01_cowichan_cleanup.R")

# Explore data ----
## examine species matrix
dim(spe.matrix)
head(rownames(spe.matrix))
head(colnames(spe.matrix))


## Does precipitation change cause significant changes in plant community composition over time? ----
### PERMANOVA

trt <- plot_year$trt # convert treatment column to its own object
year <- as.integer(plot_year$year) # convert year column to its own object
plot <- plot_year$plot # convert plot column to it's own object
plot_year$year <- as.integer(plot_year$year)
plot_year$year_trt <- as.factor(paste(plot_year$year, plot_year$trt, sep = "_"))
plot_year$year_trt <- factor(plot_year$year_trt, levels = c("1_control", "1_irrigated", "1_drought",
                                                            "2_control", "2_irrigated", "2_drought",
                                                            "3_control", "3_irrigated", "3_drought",
                                                            "4_control", "4_irrigated", "4_drought",
                                                            "5_control", "5_irrigated", "5_drought",
                                                            "6_control", "6_irrigated", "6_drought",
                                                            "7_control", "7_irrigated", "7_drought", 
                                                            "8_control", "8_irrigated", "8_drought", 
                                                            "9_control", "9_irrigated", "9_drought",
                                                            "10_control", "10_irrigated", "10_drought",
                                                            "11_control", "11_irrigated", "11_drought"
                                                            ))


#adonis2(spe.matrix ~ year*trt, # effect of trt, year, and interaction term
#        method = "bray",
#        by = "terms") # NOT controlling permutations

### Limiting permutations

# permute time WITHIN plots
CTRL <- how(within = Within(type = "series", # time series, maintain time order of samples
                            constant = TRUE, # all plots permuted in the same order because samples taken at the same time (plots experienced the same time)
                            mirror = FALSE), # samples do not influence PREVIOUS samples in time
            plots = Plots(strata = plot,
                          type = "free"), # plots permuted freely  
            nperm = 9999) 

# permute plots WITHIN time points
CTRL.b <- how(within = Within(type = "free"),
              plots = Plots(strata = plot, 
                         type = "series"),
            nperm = 9999) 
          
# check(spe.matrix, control = CTRL) # check how many permutations are possible

set.seed(49) # set seed - RUN EVERY TIME
adonis2(spe.matrix~year*trt, 
        method = "bray", 
        permutations = CTRL, by = "terms") 

set.seed(49) # set seed - RUN EVERY TIME
adonis2(spe.matrix~year*trt, 
        method = "bray", 
        permutations = CTRL.b, by = "terms") 

### pairwise comparisons
# pariwise_adonis package



## NMDS ----
set.seed(49) # set seed - RUN EVERY TIME
nmds.bc <- metaMDS(comm = spe.matrix,
                   autotransform = FALSE, # turn off autotransform
                   distance = "bray", 
                   engine = "monoMDS", # default
                   k =  3, # increased # of dimensions from default of 2 to improve stress score
                   #y =  should I structure this based on time scale?,
                   model = "global", #standard
                   stress = 1, # default and standard
                   maxit = 300,
                   try = 40,
                   trymax = 100
                     )

nmds.bc # stress abt 0.16 - not terrible, not amazing

# explore effect of individual effects on stress score
gof <- goodness(object = nmds.bc)
plot(nmds.bc, display = "sites", type = "none")

points(nmds.bc, display = "sites", cex = 2*gof/mean(gof))

# assess goodness of ordination fit (stress plot)
stressplot(nmds.bc)

# View scores from nmds output, shows position on NMDS axes
nmds_scores<-scores(nmds.bc)

#### Plotting ----
## enfit
en = envfit(nmds.bc, plot_year[2:4], permutations = 0, na.rm = TRUE) # fitting environmental variables

plot(nmds.bc)
plot(en)

##### NO ROTATION -----
# create a plot with sites colored by treatment
mds.fig.trt <- ordiplot(nmds.bc, type = "none")
points(mds.fig.trt, "sites", pch = 19, col = "black", select = plot_year$trt == 
         "control")
points(mds.fig.trt, "sites", pch = 19, col = "coral", select = plot_year$trt == 
         "drought")
points(mds.fig.trt, "sites", pch = 19, col = "deepskyblue3", select = plot_year$trt == 
         "irrigated")
ordiellipse(nmds.bc, plot_year$trt, conf = 0.95, label = TRUE) # add confidence ellipses around treatments


# create a plot with sites hulled by year
mds.fig.yr <-  ordiplot(nmds.bc, type = "none")
points(mds.fig.yr, "sites", pch = 19)
ordiellipse(nmds.bc, year, conf = 0.95, label = TRUE)

# create a plot with sites hulled by plot
mds.fig.yr <-  ordiplot(nmds.bc, type = "none")
points(mds.fig.yr, "sites", pch = 19)
ordiellipse(nmds.bc, plot, conf = 0.95, label = TRUE)

#### WITH ROTATION ----
nmds.bc.rotate <- MDSrotate(nmds.bc, year) # rotate so NMDS1 and year are perfectly correlated (perpendicular)

# Plot with year changing by color
mds.fig.yr <-  ordiplot(nmds.bc.rotate, type = "none")
points(mds.fig.yr, "sites", pch = 19, col = "#1E8E99", select = plot_year$year == "1")
points(mds.fig.yr, "sites", pch = 19, col = "#51C3CC", select = plot_year$year == "2")
points(mds.fig.yr, "sites", pch = 19, col = "#99F9FF", select = plot_year$year == "3")
points(mds.fig.yr, "sites", pch = 19, col = "#CCFEFF", select = plot_year$year == "4")
points(mds.fig.yr, "sites", pch = 19, col = "#FFE5CC", select = plot_year$year == "5")
points(mds.fig.yr, "sites", pch = 19, col = "#FFCA99", select = plot_year$year == "6")
points(mds.fig.yr, "sites", pch = 19, col = "#FFAD65", select = plot_year$year == "7")
points(mds.fig.yr, "sites", pch = 19, col = "#FF8E32", select = plot_year$year == "8")
points(mds.fig.yr, "sites", pch = 19, col = "#CC5800", select = plot_year$year == "9")
points(mds.fig.yr, "sites", pch = 19, col = "#993F00", select = plot_year$year == "10")
points(mds.fig.yr, "sites", pch = 19, col = "black", select = plot_year$year == "11")
plot(envfit(nmds.bc.rotate~year,data = plot_year[4], permutations = 0)) # add year arrow
ordiellipse(nmds.bc.rotate, plot_year$year, conf = 0.95, label = TRUE)

# create a plot with sites colored by treatment
mds.fig.trt <- ordiplot(nmds.bc.rotate, type = "none")
points(mds.fig.trt, "sites", pch = 19, col = "black", select = plot_year$trt == 
         "control")
points(mds.fig.trt, "sites", pch = 19, col = "coral", select = plot_year$trt == 
         "drought")

points(mds.fig.trt, "sites", pch = 19, col = "deepskyblue3", select = plot_year$trt == 
         "irrigated")
ordiellipse(nmds.bc.rotate, plot_year$trt, conf = 0.95, label = TRUE) # add confidence ellipses around treatments

# create a plot with plots connected by year
## Control group
mds.fig.yr <-  ordiplot(nmds.bc.rotate, type = "none")
points(mds.fig.yr, "sites", pch = 19, col = "gray", select = plot_year$trt == "control")
ordiarrows(mds.fig.yr,groups = plot,display = "sites", col = "black", show.group = c("2", "3", "8", "11", "13"))
## Irrigated group
mds.fig.yr <-  ordiplot(nmds.bc.rotate, type = "none")
points(mds.fig.yr, "sites", pch = 19, col = "deepskyblue3", select = plot_year$trt == "irrigated")
ordiarrows(mds.fig.yr, groups = plot, display = "sites", col = "black", show.group = c("1", "4", "6", "9", "12"))
## Drought group
mds.fig.yr <-  ordiplot(nmds.bc.rotate, type = "none")
points(mds.fig.yr, "sites", pch = 19, col = "coral", select = plot_year$trt == "drought")
ordiarrows(mds.fig.yr, groups = plot, display = "sites", col = "black", startmark = 1, show.group = c("5", "7", "10", "14", "15"))

### USING GGPLOT ----
trtcolor <- c("black", "coral", "deepskyblue3")

#### using gg_ordiplots ----
## color by yr
yr <- as.data.frame(plot_year$year)
colnames(yr) <- "Year"

gg.nmds.yr <- gg_ordiplot(nmds.bc.rotate, 
                          #kind = c("se"), conf = 0.95,
                         groups = year)

gg.en <- gg_envfit(nmds.bc.rotate, 
                   env = yr, groups = as.factor(year), arrow.col = "black", plot = FALSE)

gg.en$plot +
  geom_path(data = gg.nmds.yr$df_ellipse, aes(x=x, y=y, color=Group)) +
  labs(color = "Year")+
  scale_colour_paletteer_d("ggsci::default_gsea")+
  theme_classic()

## color by trt
gg.nmds.trt <- gg_ordiplot(nmds.bc.rotate, 
                           kind = c("se"), conf = 0.95,
                           groups = trt)
gg.nmds.trt <- gg.nmds.trt$plot
gg.nmds.trt+
  labs(color = "Treatment")+
  scale_colour_manual(values = trtcolor)+
  theme_classic()

## color by plot
gg.nmds.plot <- gg_ordiplot(nmds.bc.rotate, 
                            label = TRUE,
                          kind = c("se"), conf = 0.95,
                          groups = plot)
gg.nmds.plot <- gg.nmds.plot$plot
gg.nmds.plot +
  labs(color = "Plot")+
  #scale_colour_paletteer_d("ggsci::default_gsea")+
  theme_classic()


## color by yr-trt
gg.nmds.yr.trt <- gg_ordiplot(nmds.bc.rotate, 
                           kind = c("se"), conf = 0.95,
                           groups = year_trt)

gg.nmds.yr.trt <- gg.nmds.yr.trt$plot
gg.nmds.yr.trt+
  #labs(color = "Treatment")+
  #scale_colour_manual(values = trtcolor)+
  theme_classic()

#### using ggvegan----
gg.nmds <- fortify(nmds.bc.rotate) %>% filter(score == "sites")
gg.nmds <- separate(gg.nmds, col = label,
           into = c("plot", "year"),
           sep = "_", remove = FALSE, convert=TRUE) 
gg.nmds$year <- as.factor(gg.nmds$year)

# color by year
ggplot(gg.nmds, aes(x = NMDS1, y = NMDS2, color = year))+
  geom_point(cex = 2)+
  scale_colour_paletteer_d("ggsci::default_gsea")+
  theme_classic()

# color by trt
ggplot(gg.nmds, aes(x = NMDS1, y = NMDS2, color = trt))+
  geom_point(cex = 2)+
  scale_colour_manual(values = trtcolor)+
  theme_classic()

### 3D to show all three dimensions - cool but not that helpful ----
nmds.3d <- ordiplot3d(nmds.bc, type = "none")
points(nmds.3d, "sites", pch = 19, col = "black", select = plot_year$trt == 
         "control")
points(nmds.3d, "sites", pch = 19, col = "coral", select = plot_year$trt == 
         "drought")
points(nmds.3d, "sites", pch = 19, col = "deepskyblue3", select = plot_year$trt == 
         "irrigated")

nmds.3d.rot <- ordirgl(nmds.bc, display = "sites") # rotatable! cant figure out how to recolor points atm


## leftovers from Jill's code ----
## Plotting NMDS in GGPlot
nmds_scores<-scores(nmds.bc.rotate)

#Plot these scores into ggplot
nmds_scores$sites%>%
  as_tibble(rownames="samples")%>%
  ggplot(aes(x=NMDS1,y=NMDS2))+
  geom_point()

# Plot these scores into ggplot w/ year color
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  ggplot(aes(x=NMDS1,y=NMDS2,color=year))+
  geom_point()+
  theme_classic()

# Plot these scores into ggplot w/ trt color
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  ggplot(aes(x=NMDS1,y=NMDS2,color=trt))+
  geom_point()+
  theme_classic()

# Plot these scores into ggplot w/ trt shape and year color
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  ggplot(aes(x=NMDS1,y=NMDS2,color=year, pch = trt))+
  geom_point()+
  theme_classic()

# Plot scores with trt-year color
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  ggplot(aes(x=NMDS1,y=NMDS2,color=trt, pch = trt))+
  geom_point()+
  theme_classic()

# Overall Plot ----
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  ggplot(aes(x=NMDS1,y=NMDS2))+
  ylim(-1.5,1.5)+
  xlim(-1.5, 1.5)+
  geom_point(aes(color = as.factor(year)), size = 3)+
  stat_ellipse(type = "t", level = 0.90, aes(color = as.factor(year)))+
  scale_colour_paletteer_d("ggsci::default_gsea", 
                           name = "Year",
                           labels = c("1", "2", "3", "4", 
                                      "5", "6", "7", "8",
                                      "9", "10", "11"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
  

# Just plot one trt w/ ellipses ----
# control
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  filter(trt == "control")%>%
  ggplot(aes(x=NMDS1,y=NMDS2))+
  labs(title = "Control")+
  ylim(-2,2.25)+
  xlim(-2.5, 2)+
  geom_point(aes(color = year_trt), size = 3)+
  stat_ellipse(type = "t", level = 0.90, aes(color = year_trt))+
  scale_colour_paletteer_d("ggsci::default_gsea", 
                           name = "Year",
                           labels = c("1", "2", "3", "4", 
                                      "5", "6", "7", "8",
                                      "9", "10", "11"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# irrigated
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  filter(trt == "irrigated")%>%
  ggplot(aes(x=NMDS1,y=NMDS2))+
  labs(title = "Irrigated")+
  ylim(-2,2.25)+
  xlim(-2.5, 2)+
  geom_point(aes(color = year_trt), size = 3)+
  stat_ellipse(type = "t", level = 0.90, aes(color = year_trt))+
  scale_colour_paletteer_d("ggsci::default_gsea", 
                           name = "Year",
                           labels = c("1", "2", "3", "4", 
                                      "5", "6", "7", "8",
                                      "9", "10", "11"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# drought
nmds_scores$sites%>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  filter(trt == "drought")%>%
  ggplot(aes(x=NMDS1,y=NMDS2))+
  ylim(-2,2.25)+
  xlim(-2.5, 2)+
  geom_point(aes(color = year_trt), size = 3)+
  labs(title = "Drought")+
  stat_ellipse(type = "t", level = 0.90, aes(color = year_trt))+
  scale_colour_paletteer_d("ggsci::default_gsea", 
                           name = "Year",
                           labels = c("1", "2", "3", "4", 
                                      "5", "6", "7", "8",
                                      "9", "10", "11"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# create plot colored by year
mds.fig <- ordiplot(comm.bc.mds, type = "none")
ordiplot(mds.fig, type = "n", main = "ellipses")
orditorp(mds.fig, display = "sites", labels = F, 
         cex = 1)
ordihull(mds.fig, groups = year)

# create ordispider plots by trt
mds.fig <- ordiplot(comm.bc.mds, type = "none")
ordiplot(mds.fig, type = "n", main = "ellipses")
orditorp(mds.fig, display = "sites", labels = F, 
         cex = 1)
ordispider(mds.fig, groups = plot_year$trt)
# ok that is nothing

# ORDINATION

# PCoA
# input is a distance matrix; use the one we just made with Bray-Curtis
comm.pcoa<-cmdscale(comm.bc.dist)#default is 2 axes
colnames(comm.pcoa)<-c("pcoa1","pcoa2")

comm.pcoa %>%
  as_tibble(rownames="plot_year")%>%
  inner_join(.,plot_year, by="plot_year")%>%
  ggplot(aes(x=pcoa1,y=pcoa2))+
  geom_point()