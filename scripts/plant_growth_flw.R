#setwd("G://My Drive//my_folder/db_pico/pico_growth/")
#source("/Users/administrator/Documents/jaspreet/pico/pico_comb_run/packages.r")

library(readxl)
library(dplyr)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(mixedup)

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

## plotting data for each plant

p <- ggplot(growth, aes(x=plant_no, y=infl, color=year, shape=inf)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_jitter(width = 0.00) +
  scale_color_manual(values=c("red", "blue", "green", "black", "brown", "pink", "grey", "yellow", "khaki"))
p

## Question 1: Is vegetative fitness of a plant in a given year correlated to
## its reproductive effort in that year?

## crossed mixed model showing the effect of vegetative fitness (leaf area) on
## reproductive fitness (inflorescence length) in a given year

q1glm1 = glmmTMB(infl ~ la + (1 | year) + (1 | plant_no),
              family = tweedie(link = "log"), ##other option is to use ziGamma family here
              ziformula = ~ la + year,
              data = growth)
summary(q1glm1)

## simulationOutput <- simulateResiduals(fittedModel = q1glm1, plot = F)
## plot(simulationOutput)

## For the following questions, adjust the response variable by taking out the
## random effects. Extract the random effect of year from infl variable by
## using the q1glm1 model

## run model on la as a dependent variable to extract the random effects

q0glm = glmmTMB(la ~ (1 | year) + (1 | plant_no),
                family = tweedie(link = "log"),
                ziformula = ~ year,
                data = growth)
summary(q0glm)

extract_random_effects(q0glm)

## Make a new growth data frame by adding or subtracting the effect of random
## variable

growth2 = growth %>% group_by(year) %>% mutate(new = ifelse(year == 2017, la - 0.698, la)) %>%
  mutate(new = ifelse(year == 2018, la + 1.32, new))%>%
  mutate(new = ifelse(year == 2019, la - 0.251, new))%>%
  mutate(new = ifelse(year == 2020, la - 0.667, new))%>%
  mutate(new = ifelse(year == 2021, la + 0.171, new))%>%
  mutate(new = ifelse(year == 2022, la + 0.01, new))

## DWS: you could do the above with a merge/join rather than hard code it. That
## is fragile. It would also probably work to use year as fixed effect to grab
## coefficients, right? Is result much different?


growth2$la = growth2$new
growth2$la = ifelse(growth2$la < 0, as.numeric(paste(0)), growth2$la)
                                                 
## Question 2: Does vegetative fitness of an individual in a given year explain
## its vegetative fitness in the following year?

## Load another sheet that provides the data from year0 and year1 side by side
growth2 = read_excel("data/pico_growth_met.xlsx", sheet = 2, na = "NA")

## Crossed mixed model showing the effect of vegetative fitness on next year's
## vegetative fitness
q2glm1 = glmmTMB(la2 ~ la + (1 | year) + (1 | plant_no),
              family = tweedie(link = "log"),
              ziformula = ~ la,
              data = growth2)
summary(q2glm1)

## Question3: Does the reproductive effort made by a plant in a given year
## explain its vegetative fitness in the following year?

## mixed model showing the effect of reproductive fitness in y0 on next year's
## reproductive fitness (inflorescence length)

q3glm1 <- glmmTMB(la2 ~ infl + (1 | year) + (1 | plant_no),
                  family = tweedie(link = "log"),
                  ziformula = ~ infl,
                  data = growth2)
summary(q3glm1)

## Question 4: Does herbivory experienced by an individual plant explain
## patterns of vegetative fitness in the subsequent year?

q4glm1 <- glmmTMB(la2 ~ herb + (1 | year) + (1 | plant_no),
                  family = tweedie(link = "log"),
                  ziformula = ~ herb,
                  data = growth2)
summary(q4glm1)
