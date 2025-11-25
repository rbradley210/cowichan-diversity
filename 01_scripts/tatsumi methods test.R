# Script for: Does precipitation change cause plant communities to homogenize or differentiate?
# Robin Bradley
# robin.bradley@ubc.ca
# created: 14 Oct 2025
# last updated: 14 Oct 2025

# 1. Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## Packages
library(tidyverse)
library(betapart)
library(ecopart)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/01_cowichan_cleanup.R")

# 2. Make site-species matrix for each year and treatment ----
## bringing in year and plot rows
spe.matrix <- rownames_to_column(spe.matrix)%>% 
  left_join(plot_year, by = c("rowname" = "plot_year"))

## split into year and treatment 
### Drought plots
spma.2015.d <- spe.matrix %>%
  filter(year == 2015, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2015.d) <- spma.2015.d$plot
spma.2015.d <- spma.2015.d[, -62] 

spma.2016.d <- spe.matrix %>%
  filter(year == 2016, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2016.d) <- spma.2016.d$plot
spma.2016.d <- spma.2016.d[, -62] 

spma.2017.d <- spe.matrix %>%
  filter(year == 2017, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2017.d) <- spma.2017.d$plot
spma.2017.d <- spma.2017.d[, -62] 

spma.2018.d <- spe.matrix %>%
  filter(year == 2018, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2018.d) <- spma.2018.d$plot
spma.2018.d <- spma.2018.d[, -62] 

spma.2019.d <- spe.matrix %>%
  filter(year == 2019, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2019.d) <- spma.2019.d$plot
spma.2019.d <- spma.2019.d[, -62] 

spma.2020.d <- spe.matrix %>%
  filter(year == 2020, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2020.d) <- spma.2020.d$plot
spma.2020.d <- spma.2020.d[, -62] 

spma.2021.d <- spe.matrix %>%
  filter(year == 2021, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2021.d) <- spma.2021.d$plot
spma.2021.d <- spma.2021.d[, -62] 

spma.2022.d <- spe.matrix %>%
  filter(year == 2022, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2022.d) <- spma.2022.d$plot
spma.2022.d <- spma.2022.d[, -62] 

spma.2023.d <- spe.matrix %>%
  filter(year == 2023, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2023.d) <- spma.2023.d$plot
spma.2023.d <- spma.2023.d[, -62] 

spma.2024.d <- spe.matrix %>%
  filter(year == 2024, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2024.d) <- spma.2024.d$plot
spma.2024.d <- spma.2024.d[, -62] 

spma.2025.d <- spe.matrix %>%
  filter(year == 2025, trt == "drought") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2025.d) <- spma.2025.d$plot
spma.2025.d <- spma.2025.d[, -62] 


### irrigated plots
spma.2015.i <- spe.matrix %>%
  filter(year == 2015, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2015.i) <- spma.2015.i$plot
spma.2015.i <- spma.2015.i[, -62] 

spma.2016.i <- spe.matrix %>%
  filter(year == 2016, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2016.i) <- spma.2016.i$plot
spma.2016.i <- spma.2016.i[, -62] 

spma.2017.i <- spe.matrix %>%
  filter(year == 2017, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2017.i) <- spma.2017.i$plot
spma.2017.i <- spma.2017.i[, -62] 

spma.2018.i <- spe.matrix %>%
  filter(year == 2018, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2018.i) <- spma.2018.i$plot
spma.2018.i <- spma.2018.i[, -62] 

spma.2019.i <- spe.matrix %>%
  filter(year == 2019, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2019.i) <- spma.2019.i$plot
spma.2019.i <- spma.2019.i[, -62] 

spma.2020.i <- spe.matrix %>%
  filter(year == 2020, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2020.i) <- spma.2020.i$plot
spma.2020.i <- spma.2020.i[, -62] 

spma.2021.i <- spe.matrix %>%
  filter(year == 2021, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2021.i) <- spma.2021.i$plot
spma.2021.i <- spma.2021.i[, -62] 

spma.2022.i <- spe.matrix %>%
  filter(year == 2022, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2022.i) <- spma.2022.i$plot
spma.2022.i <- spma.2022.i[, -62] 

spma.2023.i <- spe.matrix %>%
  filter(year == 2023, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2023.i) <- spma.2023.i$plot
spma.2023.i <- spma.2023.i[, -62]  

spma.2024.i <- spe.matrix %>%
  filter(year == 2024, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2024.i) <- spma.2024.i$plot
spma.2024.i <- spma.2024.i[, -62] 

spma.2025.i <- spe.matrix %>%
  filter(year == 2025, trt == "irrigated") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2025.i) <- spma.2025.i$plot
spma.2025.i <- spma.2025.i[, -62] 

### control plots
spma.2015.c <- spe.matrix %>%
  filter(year == 2015, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2015.c) <- spma.2015.c$plot
spma.2015.c <- spma.2015.c[, -62] 

spma.2016.c <- spe.matrix %>%
  filter(year == 2016, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2016.c) <- spma.2016.c$plot
spma.2016.c <- spma.2016.c[, -62] 

spma.2017.c <- spe.matrix %>%
  filter(year == 2017, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2017.c) <- spma.2017.c$plot
spma.2017.c <- spma.2017.c[, -62] 

spma.2018.c <- spe.matrix %>%
  filter(year == 2018, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2018.c) <- spma.2018.c$plot
spma.2018.c <- spma.2018.c[, -62] 

spma.2019.c <- spe.matrix %>%
  filter(year == 2019, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2019.c) <- spma.2019.c$plot
spma.2019.c <- spma.2019.c[, -62] 

spma.2020.c <- spe.matrix %>%
  filter(year == 2020, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2020.c) <- spma.2020.c$plot
spma.2020.c <- spma.2020.c[, -62] 

spma.2021.c <- spe.matrix %>%
  filter(year == 2021, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2021.c) <- spma.2021.c$plot
spma.2021.c <- spma.2021.c[, -62] 

spma.2022.c <- spe.matrix %>%
  filter(year == 2022, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2022.c) <- spma.2022.c$plot
spma.2022.c <- spma.2022.c[, -62] 

spma.2023.c <- spe.matrix %>%
  filter(year == 2023, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2023.c) <- spma.2023.c$plot
spma.2023.c <- spma.2023.c[, -62]  

spma.2024.c <- spe.matrix %>%
  filter(year == 2024, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2024.c) <- spma.2024.c$plot
spma.2024.c <- spma.2024.c[, -62] 

spma.2025.c <- spe.matrix %>%
  filter(year == 2025, trt == "control") %>%
  subset(select = -c(year, trt, rowname))
rownames(spma.2025.c) <- spma.2025.c$plot
spma.2025.c <- spma.2025.c[, -62]

# 3. Calculate B-C dissimilarity between treatments in each year-pair
multi.bra

# 4. Calculate homogenization and differentiation components between each year ----
## drought
ep.2015.2016.d <- as.data.frame(ecopart.multi(spma.2015.d, spma.2016.d,
                              index = "baselga",
                              components = "four"))

ep.2016.2017.d <- as.data.frame(ecopart.multi(spma.2016.d, spma.2017.d, 
                              index = "baselga",
                              components = "four"))

ep.2017.2018.d <- as.data.frame(ecopart.multi(spma.2017.d, spma.2018.d, 
                              index = "baselga",
                              components = "four"))

ep.2018.2019.d <- as.data.frame(ecopart.multi(spma.2018.d, spma.2019.d, 
                              index = "baselga",
                              components = "four"))

ep.2019.2020.d <- as.data.frame(ecopart.multi(spma.2019.d, spma.2020.d, 
                              index = "baselga",
                              components = "four"))

ep.2020.2021.d <- as.data.frame(ecopart.multi(spma.2020.d, spma.2021.d, 
                              index = "baselga",
                              components = "four"))

ep.2021.2022.d <- as.data.frame(ecopart.multi(spma.2021.d, spma.2022.d, 
                              index = "baselga",
                              components = "four"))

ep.2022.2023.d <- as.data.frame(ecopart.multi(spma.2022.d, spma.2023.d, 
                              index = "baselga",
                              components = "four"))

ep.2023.2024.d <- as.data.frame(ecopart.multi(spma.2023.d, spma.2024.d, 
                              index = "baselga",
                              components = "four"))

ep.2024.2025.d <- as.data.frame(ecopart.multi(spma.2024.d, spma.2025.d, 
                              index = "baselga",
                              components = "four"))

ep.d <- cbind(ep.2015.2016.d, ep.2016.2017.d, ep.2017.2018.d,
            ep.2018.2019.d, ep.2019.2020.d, ep.2020.2021.d,
            ep.2021.2022.d, ep.2022.2023.d, ep.2023.2024.d,
            ep.2024.2025.d)

ep.d <- as.data.frame(t(ep.d))
rownames(ep.d) <- c("1", "2", "3","4", "5", 
                  "6","7", "8", "9", "10")
ep.d$Total <- ep.d$`Subtractive homogenization`+ep.d$`Subtractive differentiation`+
  ep.d$`Additive homogenization`+ep.d$`Additive differentiation`

ep.d <- rownames_to_column(ep.d) %>%
  pivot_longer(
    cols = c("Subtractive homogenization", "Subtractive differentiation",
             "Additive homogenization", "Additive differentiation", "Total"),
    names_to = "beta_type",
    values_to = "beta_value"
  )

names(ep.d)[1] <- "year"
ep.d$year <- as.integer(ep.d$year)
ep.d$trt <- "drought"


rm(spma.2015.d, spma.2016.d, spma.2017.d, spma.2018.d,
   spma.2019.d, spma.2020.d, spma.2021.d, spma.2022.d,
   spma.2023.d, spma.2024.d, spma.2025.d)

rm(ep.2015.2016.d, ep.2016.2017.d, ep.2017.2018.d,
   ep.2018.2019.d, ep.2019.2020.d, ep.2020.2021.d,
   ep.2021.2022.d, ep.2022.2023.d, ep.2023.2024.d,
   ep.2024.2025.d)

## irrigated
ep.2015.2016.i <- as.data.frame(ecopart.multi(spma.2015.i, spma.2016.i,
                                              index = "baselga",
                                              components = "four"))

ep.2016.2017.i <- as.data.frame(ecopart.multi(spma.2016.i, spma.2017.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2017.2018.i <- as.data.frame(ecopart.multi(spma.2017.i, spma.2018.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2018.2019.i <- as.data.frame(ecopart.multi(spma.2018.i, spma.2019.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2019.2020.i <- as.data.frame(ecopart.multi(spma.2019.i, spma.2020.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2020.2021.i <- as.data.frame(ecopart.multi(spma.2020.i, spma.2021.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2021.2022.i <- as.data.frame(ecopart.multi(spma.2021.i, spma.2022.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2022.2023.i <- as.data.frame(ecopart.multi(spma.2022.i, spma.2023.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2023.2024.i <- as.data.frame(ecopart.multi(spma.2023.i, spma.2024.i, 
                                              index = "baselga",
                                              components = "four"))

ep.2024.2025.i <- as.data.frame(ecopart.multi(spma.2024.i, spma.2025.i, 
                                              index = "baselga",
                                              components = "four"))

ep.i <- cbind(ep.2015.2016.i, ep.2016.2017.i, ep.2017.2018.i,
              ep.2018.2019.i, ep.2019.2020.i, ep.2020.2021.i,
              ep.2021.2022.i, ep.2022.2023.i, ep.2023.2024.i,
              ep.2024.2025.i)

ep.i <- as.data.frame(t(ep.i))
rownames(ep.i) <- c("1", "2", "3","4", "5", 
                    "6","7", "8", "9", "10")
ep.i$Total <- ep.i$`Subtractive homogenization`+ep.i$`Subtractive differentiation`+
  ep.i$`Additive homogenization`+ep.i$`Additive differentiation`

ep.i <- rownames_to_column(ep.i) %>%
  pivot_longer(
    cols = c("Subtractive homogenization", "Subtractive differentiation",
             "Additive homogenization", "Additive differentiation", "Total"),
    names_to = "beta_type",
    values_to = "beta_value"
  )

names(ep.i)[1] <- "year"
ep.i$year <- as.integer(ep.i$year)
ep.i$trt <- "irrigated"


rm(spma.2015.i, spma.2016.i, spma.2017.i, spma.2018.i,
   spma.2019.i, spma.2020.i, spma.2021.i, spma.2022.i,
   spma.2023.i, spma.2024.i, spma.2025.i)

rm(ep.2015.2016.i, ep.2016.2017.i, ep.2017.2018.i,
   ep.2018.2019.i, ep.2019.2020.i, ep.2020.2021.i,
   ep.2021.2022.i, ep.2022.2023.i, ep.2023.2024.i,
   ep.2024.2025.i)

## control
ep.2015.2016.c <- as.data.frame(ecopart.multi(spma.2015.c, spma.2016.c,
                                              index = "baselga",
                                              components = "four"))

ep.2016.2017.c <- as.data.frame(ecopart.multi(spma.2016.c, spma.2017.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2017.2018.c <- as.data.frame(ecopart.multi(spma.2017.c, spma.2018.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2018.2019.c <- as.data.frame(ecopart.multi(spma.2018.c, spma.2019.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2019.2020.c <- as.data.frame(ecopart.multi(spma.2019.c, spma.2020.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2020.2021.c <- as.data.frame(ecopart.multi(spma.2020.c, spma.2021.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2021.2022.c <- as.data.frame(ecopart.multi(spma.2021.c, spma.2022.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2022.2023.c <- as.data.frame(ecopart.multi(spma.2022.c, spma.2023.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2023.2024.c <- as.data.frame(ecopart.multi(spma.2023.c, spma.2024.c, 
                                              index = "baselga",
                                              components = "four"))

ep.2024.2025.c <- as.data.frame(ecopart.multi(spma.2024.c, spma.2025.c, 
                                              index = "baselga",
                                              components = "four"))

ep.c <- cbind(ep.2015.2016.c, ep.2016.2017.c, ep.2017.2018.c,
              ep.2018.2019.c, ep.2019.2020.c, ep.2020.2021.c,
              ep.2021.2022.c, ep.2022.2023.c, ep.2023.2024.c,
              ep.2024.2025.c)

ep.c <- as.data.frame(t(ep.c))
rownames(ep.c) <- c("1", "2", "3","4", "5", 
                    "6","7", "8", "9", "10")
ep.c$Total <- ep.c$`Subtractive homogenization`+ep.c$`Subtractive differentiation`+
  ep.c$`Additive homogenization`+ep.c$`Additive differentiation`

ep.c <- rownames_to_column(ep.c) %>%
  pivot_longer(
    cols = c("Subtractive homogenization", "Subtractive differentiation",
             "Additive homogenization", "Additive differentiation", "Total"),
    names_to = "beta_type",
    values_to = "beta_value"
  )

names(ep.c)[1] <- "year"
ep.c$year <- as.integer(ep.c$year)
ep.c$trt <- "control"


rm(spma.2015.c, spma.2016.c, spma.2017.c, spma.2018.c,
   spma.2019.c, spma.2020.c, spma.2021.c, spma.2022.c,
   spma.2023.c, spma.2024.c, spma.2025.c)

rm(ep.2015.2016.c, ep.2016.2017.c, ep.2017.2018.c,
   ep.2018.2019.c, ep.2019.2020.c, ep.2020.2021.c,
   ep.2021.2022.c, ep.2022.2023.c, ep.2023.2024.c,
   ep.2024.2025.c)

## combine into one data frame
ep <- rbind(ep.d, ep.i, ep.c)

## pull out total change in beta
ep.total <- ep %>%
  filter(beta_type == "Total")


# 4. Plot values over time ----
ggplot(ep.d, aes(x = year, y = beta_value, color = beta_type))+
  geom_point()+
  theme_classic()

ggplot(ep.i, aes(x = year, y = beta_value, color = beta_type))+
  geom_point()+
  theme_classic()

ggplot(ep.c, aes(x = year, y = beta_value, color = beta_type))+
  geom_point()+
  theme_classic()


ggplot(ep.total, aes(x = year, y = beta_value, color = trt))+
  geom_line()+
  theme_classic()



# 5. Bootstrapping? ----