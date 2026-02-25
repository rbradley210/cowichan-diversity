# traits analysis
# Robin Bradley
# robin.bradley@ubc.ca
# Last updated: 31 January 2025

# 1. Set-up ----
# set WD
setwd("C:/Users/Robin/Documents/School/Williams Lab")

# packages
# library(rtry) # for reading in and cleaning data from TRY database
library(readxl)
library(tidyverse)
library(ggtext)
library(visreg)
library(emmeans)
library(FSA)
library(car)
library(lme4)
library(ape)
library(V.PhyloMaker2)
library(phylolm)

# read in data
height <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Height")
weight <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Dry Weight")
area <- read.csv("~/School/Williams Lab/cowichan-diversity/00_rawdata/leafarea.csv")
attr <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Species")
plots <- read.csv("/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info/IDE_plotinfo.csv", header = TRUE)
phylo <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "phylo")
leafN <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Leaf N")

## selecting plants for N analysis ----
# weight.red <- weight %>% 
#   select(c("Plant Species", "plot", "Plant ID", "Leaf ID", "Part ID", "Weight (mg)"))
# 
# weight.leaves <- weight.red %>%
#   group_by(`Plant Species`, plot, `Plant ID`, `Leaf ID`) %>%
#   summarise(tot.weight = sum(`Weight (mg)`)) # combine part IDs to leaf level
# 
# plants <- weight.leaves %>% 
#   group_by(`Plant Species`, plot, `Plant ID`) %>%
#   summarise(`leaf_ids` = list(`Leaf ID`),
#             tot.weight = sum(tot.weight)) # combine leaf IDs to plant level
# 
# plants$leaf_ids <- gsub("c", "", plants$leaf_ids) # clean leaf IDs column for readability
# plants$leaf_ids <- gsub("\\(", "", plants$leaf_ids)
# plants$leaf_ids <- gsub("\\)", "", plants$leaf_ids)
# 
# plants <- left_join(plants, plots, by = "plot")
# 
# plants$sampleID <- paste(plants$`Plant Species`, 
#                          plants$plot, 
#                          plants$`Plant ID`, 
#                          sep = "_") # create sample ID column
# 
# # check for duplicates
# n_occur <- data.frame(table(plants$sampleID))
# plants[plants$sampleID %in% n_occur$Var1[n_occur$Freq > 1],]
# unique(n_occur$Freq)
# rm(n_occur)
# 
# # remove plants with less than 3mg
# plants <- plants[plants$tot.weight > 3, ]
# 
# # export CSV
# write.csv(plants, 
#           "~/School/Williams Lab/cowichan-diversity/04_other_outputs/samplesforNanalysis_inclLOUT.csv", 
#           row.names = FALSE)
# # edited down in excel
# 
# # bring in new sheet
# Nplants <- read.csv("~/School/Williams Lab/cowichan-diversity/04_other_outputs/samplesforNanalysis.csv",
#                   header = TRUE)
# 
# # make table with number of plants per species per trt
# Nplants.tbl <- Nplants %>%
#   group_by(Plant.Species, trt)%>%
#   summarise(count = n())
# 
# # make table with number of trts per species
# Nplants.tbl.2 <- Nplants.tbl %>%
#   group_by(Plant.Species)%>%
#   summarise(count = n())


# 2. Data Cleaning ----
## Making phylogenetic tree ----
### run the function
result <- phylo.maker(phylo)
tree3 <- result$scenario.3
tree3$node.label <- NULL

plot.phylo(result$scenario.3, cex = 1.5, main = "scenario.3")

## Height ----
height <- height %>%
  left_join(plots) %>%
  left_join(attr)

### Log transform height
height$logheight <- log(height$height)
height$species <- as.factor(height$species)
height$plot <- as.factor(height$plot)
height$trt <- factor(height$trt, levels=c("control","drought","irrigated"))
height$growthform <- as.factor(height$growthform)
height$duration <- as.factor(height$duration)
height$nativity <- as.factor(height$nativity)


## Weight ----
weight <- weight %>%
  dplyr::group_by(species, plot, `Plant ID`, `Leaf ID`) %>%
  dplyr::summarize(tot.weight = sum(weight))


weight$sample <- paste(weight$species, 
                         weight$plot,
                         weight$`Plant ID`, 
                         sep = "_") # create sample ID column to match leaf area
weight$sample <- paste(weight$sample,
                         weight$`Leaf ID`, 
                         sep = "") # add leaf ID

## Specific Leaf Area ----
sla <- left_join(area, weight) %>% # combine datasets
  na.omit()%>% # remove NAs
  left_join(plots)%>%
  left_join(attr)
sla$total.leaf.area.mm <- 100*(sla$total.leaf.area) # convert leaf area from cm2 to mm2
sla$sla <- sla$total.leaf.area.mm/sla$tot.weight # calculate SLA
sla$logsla <- log(sla$sla)
sla$log10sla <- log10(sla$sla)
sla$sqrtsla <- sqrt(sla$sla)

hist(sla$sla, right = FALSE) # non-normal
hist(sla$logsla, right = FALSE)

## Leaf Nitrogen ----
leafN <- leafN %>%
  separate(col = sample_id, 
           into = c("species", "plot", "plant_id"),
           sep = "_",
           remove =FALSE)
leafN$plot <- as.numeric(leafN$plot)
leafN <- leafN%>%
  left_join(plots) %>%
  left_join(attr)


# 3. Data Exploration ----
# Height
ggplot(height, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  facet_wrap(~species)+
  theme_test()

# SLA
ggplot(sla, aes(x = trt, y = sla, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("SLA")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  facet_wrap(~species)+
  theme_test()

# Leaf N
ggplot(leafN, aes(x = trt, y = per_N, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Leaf N")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  facet_wrap(~species)+
  theme_test()




# 4. Data Analysis ----
## Height ---- 
### Test for treatment effects (linear models) ----
lm_overall <- lmer(logheight ~ trt + (1|species) + (1|plot), data = height)
summary(lm_overall)
Anova(lm_overall)
plot(lm_overall)

#### Linear model with func. group and trt as fixed, plot as random
lm_func <- lmer(logheight ~ trt*growthform + (1|species) + (1|plot), data = height)
summary(lm_func)
Anova(lm_func)
plot(lm_func)

h_func_grpmeans <- emmeans(lm_func, c("trt", "growthform"))
pairs(h_func_grpmeans)
plot(h_func_grpmeans)
pwpm(h_func_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with nativity and trt as fixed, plot as random
lm_nat <- lmer(logheight ~ trt*nativity + (1|species) + (1|plot), data = height)
summary(lm_nat)
Anova(lm_nat)

#### Linear model with duration and trt as fixed, plot as random
lm_dur <- lmer(logheight ~ trt*duration + (1|species) + (1|plot), data = height)
summary(lm_dur)
Anova(lm_dur)


## SLA ----
### Test for treatment effects (linear models) ----
lm_sla_overall <- lmer(logsla ~ trt + (1|species) + (1|plot), data = sla)
summary(lm_sla_overall)
Anova(lm_sla_overall)
plot(lm_sla_overall)

sla_overall_grpmeans <- emmeans(lm_sla_overall, c("trt"))
pairs(sla_overall_grpmeans)
plot(sla_overall_grpmeans)
pwpm(sla_overall_grpmeans, means = FALSE, diffs = FALSE)

pgls_sla_overall <- phylolm(logsla~trt, 
                           data = sla,
                           phy = tree3,
                           boot = 100)

#### Linear model with func. group and trt as fixed, plot as random
lm_sla_func <- lmer(sla ~ trt*growthform + (1|species) + (1|plot), data = sla)
summary(lm_sla_func)
Anova(lm_sla_func)

sla_func_grpmeans <- emmeans(lm_sla_func, c("trt", "growthform"))
pairs(sla_func_grpmeans)
plot(sla_func_grpmeans)
pwpm(sla_func_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with nativity and trt as fixed, plot as random
lm_sla_nat <- lmer(logsla ~ trt*nativity + (1|species) + (1|plot), data = sla)
summary(lm_sla_nat)
Anova(lm_sla_nat)

sla_nat_grpmeans <- emmeans(lm_sla_nat, c("trt", "nativity"))
pairs(sla_nat_grpmeans)
#plot(sla_nat_grpmeans)
pwpm(sla_nat_grpmeans, means = FALSE, diffs = FALSE)


#### Linear model with duration and trt as fixed, plot as random
lm_sla_dur <- lmer(logsla ~ trt*duration + (1|species) + (1|plot), data = sla)
summary(lm_sla_dur)
Anova(lm_sla_dur)

sla_dur_grpmeans <- emmeans(lm_sla_dur, c("trt", "duration"))
pairs(sla_dur_grpmeans)
#plot(sla_dur_grpmeans)
pwpm(sla_dur_grpmeans, means = FALSE, diffs = FALSE)


# 5. Data Visualization ----
colors <- c("control" = "black", "drought" = "darkorange", "irrigated" = "deepskyblue2")
## Height ----
### overall ----

#### trts only
ggplot(height, aes(trt, height, fill = trt, color = trt)) +
  scale_color_manual(values = colors) +
  geom_jitter(size = 1.5, width = 0.2, alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               size = 2) +
  guides(fill = "none")+
  labs(x = "Treatment", y = "Height (cm)", color = "Treatment") +
  coord_cartesian(ylim = c(0, 115))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())


#### trt by growth form
plot(visreg(lm_func, xvar = "growthform", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("control" = "black", "drought" = "darkorange", "irrigated" = "deepskyblue2"), cex = 1),
     #line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Height",
     xlab = "Trt",
     ylab = "Log Height",
     overlay = TRUE)

plot(visreg(lm_func, xvar = "growthform", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "Height",
     xlab = "Trt",
     ylab = "Log Height",
     overlay = TRUE)



#### trt by nativity
plot(visreg(lm_nat, xvar = "nativity", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Height",
     xlab = "Origin",
     ylab = "Log Height",
     overlay = TRUE)

#### trt by duration
plot(visreg(lm_dur, xvar = "duration", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Height",
     xlab = "Duration",
     ylab = "Log Height",
     overlay = TRUE)


### individual ----
### ALAM
ggplot(h_ALAM, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### BRHO
ggplot(h_BRHO, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### CLPE
ggplot(h_CLPE, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### ELGL
ggplot(h_ELGL, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### RAOC
ggplot(h_RAOC, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### VALO
ggplot(h_VALO, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### VUSP
ggplot(h_VUSP, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()


## SLA ----
#### trts only
plot(visreg(lm_sla_overall, xvar = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "Log (SLA)",
     xlab = "Trt",
     ylab = "Log(SLA)",
     overlay = TRUE)

ggplot(sla, aes(trt, logsla, fill = trt, color = trt)) +
  scale_color_manual(values = colors) +
  geom_jitter(size = 1.5, width = 0.2, alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               size = 2) +
  guides(fill = "none")+
  labs(x = "Treatment", y = "Specific Leaf Area (cm<sup>2</sup>/g)", color = "Treatment") +
  coord_cartesian(ylim = c(0, 5))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())

#### trt by growth form
plot(visreg(lm_sla_func, xvar = "growthform", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.5),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "SLA",
     xlab = "Growth Form",
     ylab = "SLA",
     overlay = TRUE)

plot(visreg(lm_sla_func, xvar = "growthform", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "SLA",
     xlab = "Growth Form",
     ylab = "SLA",
     overlay = TRUE)

ggplot(sla, aes(growthform, sla)) +
  geom_jitter(size = 1.5, width = 0.2, alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               size = 2) +
  guides(fill = "none")+
  labs(x = "Growth Form", y = "Specific Leaf Area (cm<sup>2</sup>/g)") +
  coord_cartesian(ylim = c(0, 110))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())

#### trt by nativity
plot(visreg(lm_sla_nat, xvar = "nativity", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.5),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "SLA",
     xlab = "Origin",
     ylab = "SLA",
     overlay = TRUE)

#### trt by duration
plot(visreg(lm_sla_dur, xvar = "duration", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     #points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.5),
     #line = list(col = c("black", "darkorange", "deepskyblue2")),
     xlab = "Duration",
     ylab = "Specific leaf area (mm^2/mg)",
     overlay = TRUE)

ggplot(sla, aes(duration, sla, fill = trt, color = trt)) +
  scale_color_manual(values = colors) +
  geom_jitter(size = 1.5, position = position_jitterdodge(jitter.width = 0.2), alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7, position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "point", 
               size = 2, position = position_dodge(width = 0.75)) +
  guides(fill = "none")+
  labs(x = "Duration", y = "Specific Leaf Area (cm<sup>2</sup>/g)", color = "Treatment") +
  coord_cartesian(ylim = c(0, 110))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())

