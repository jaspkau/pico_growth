#setwd("G://My Drive//my_folder/db_pico/pico_growth/pico_growth_stats/")
#source("/Users/administrator/Documents/jaspreet/pico/pico_comb_run/packages.r")

## DWS: Never set working direcotry in code. This will fail on anyone elses
## machine and int his case is also a security risk as you just revealed that
## on the machine you are on you code and work under an administrator account.

## instead, document what you expect the working directory to be and let the
## user handle that (eg in an rstudio project file ignored by git). 



#library(dunn.test)
#library(adespatial)
#library(phyloseq)
#library(metagenomeSeq)
#library(mixOmics)
#library(sjPlot)
#library(ggpubr)
#library(gdata)
library(ggplot2)
library(readxl)
library(dplyr)
#library(statmod)
library(glmmTMB)

## Don't dump unnneded packages into the global namespace

# Pico growth data --------------------------------------------------------

growth_raw = read_excel("data/pico_growth_met.xlsx", sheet = 1, na = "?")

###filter the dataset  comprising of 100 plants----------------------
growth_raw = subset(growth_raw, plant_set == "b")

###summarize data for each plant by year and calculate leaf area 

growth = growth_raw %>%
  group_by(year, plant_no) %>%
  summarise(nol = sum(leaf_no, na.rm = TRUE),
            all = mean(leaf_len, na.rm = TRUE),
            alw = mean(leaf_wid, na.rm = TRUE),
            la = sum(3.14*(leaf_len/2)*(leaf_wid/2), na.rm = TRUE), ##leaf area for ellipitical leafs 
            inf = sum(inf_init, na.rm = TRUE),
            infl = sum(inf_len, na.rm = TRUE),
            infw = sum(inf_wid, na.rm = TRUE),
            herb = sum(herb, na.rm = TRUE))
growth

growth$year = as.factor(growth$year)
growth$inf = as.factor(growth$inf)

###plotting data for each plant

p <- ggplot(growth, aes(x=plant_no, y=la, color=year, shape=inf)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_jitter(width = 0.00) +
  scale_color_manual(values=c("red", "blue", "green", "black", "brown", "pink", "grey", "yellow", "khaki"))
p

#Question 1: Is vegetative fitness of a plant in a given year correlated to its reproductive effort in that year?

#crossed mixed model showing the effect of vegetative fitness (leaf area) on reproductive fitness (inflorescence length) in a given year 

q1glm1 = glmmTMB(infl ~ la + (1 | year) + (1 | plant_no),
              family = tweedie(link = "log"),
              ziformula = ~ la + year + plant_no,
              data = growth)
summary(q1glm1)

#Question 2: Does vegetative fitness of an individual in a given year explain its vegetative and / or reproductive success in the following year?

#####load another sheet that provides the data from year0 and year1 side by side
growth2 = read_excel("data/pico_growth_met.xlsx", sheet = 2, na = "NA")

###crossed mixed model showing the effect of vegetative fitness on next year's vegetative fitness
q2glm1 = glmmTMB(la2 ~ la + (1 | year) + (1 | plant_no),
              family = tweedie(link = "log"),
              ziformula = ~ la + year + plant_no,
              data = growth2)
summary(q2glm1)

###mixed model showing the effect of vegetative fitness on next year's reproductive fitness (inflorescence length)

q2glm2 <- glmmTMB(infl2 ~ la + (1 | year) + (1 | plant_no),
                         family = tweedie(link = "log"),
                         ziformula = ~ la + year + plant_no,
                         data = growth2)
summary(q2glm2)

#Question3: Does the reproductive effort made by a plant in a given year explain its vegetative or reproductive success in the following year?

##Question 4: Does herbivory experienced by an individual plant explain patterns of vegetative and reproductive dormancies in the subsequent year? 

