## plant_growth_flw.R
## Jaspreet Kaur 2024

## DESCRIPTION?


## DWS: There is a lot of repeated code. I suggest using ggplot themes to
## reduce that.

#setwd("C:/Users/Administrator/OneDrive - University of Wisconsin-La Crosse/my_folder/db_pico/pico_growth")


library(readxl)
library(dplyr)
# binary TMB from apt might be ahead of glmmtm, in that case do
# install.packages("glmmTMB", type="source")
library(glmmTMB) 
library(DHARMa)
library(ggplot2)
library(tidyr)
library(ggeffects)
library(ggpubr)

# Pico growth data --------------------------------------------------------

growth_raw <- read_excel("data/pico_growth_met.xlsx", sheet = 1, na = "?")

## Filter the dataset  comprising of 100 plants
growth_raw <- subset(growth_raw, plant_set == "b")

## Summarize data for each plant by year and calculate leaf area 

growth <- growth_raw %>%
  group_by(year, plant_no) %>%
  summarise(nol = sum(leaf_no, na.rm = TRUE),
            all = mean(leaf_len, na.rm = TRUE),
            alw = mean(leaf_wid, na.rm = TRUE),
            # leaf area for ellipitical leaves:
            la = sum(3.14*(leaf_len/2)*(leaf_wid/2), na.rm = TRUE), 
            inf = sum(inf_init, na.rm = TRUE),
            infl = sum(inf_len, na.rm = TRUE),
            infw = sum(inf_wid, na.rm = TRUE),
            herb = sum(herb, na.rm = TRUE))
growth

## DWS: The below drop seems dangerous? 700 to 637 observations? deserves
## explanation. NaNs occur in 'all' and 'alw'. Why? Does that really mean you
## must drop those observations completely?
growth <- growth %>% drop_na() 

growth$year <- as.factor(growth$year)
growth$inf <- as.factor(growth$inf)


## Question 1: Is vegetative fitness of a plant in a given year correlated to
## its reproductive effort in that year?

## crossed mixed model showing the effect of vegetative fitness (leaf area) on
## reproductive fitness (inflorescence length) in a given year-

q1glm1 <- glmmTMB(infl ~ la + (1 | year) + (1 | plant_no),
              family = tweedie(link = "log"), # other option is to use ziGamma
                                              # family here
              ziformula = ~ la + (1 | year),
              data = growth)
summary(q1glm1)

## plot the predicted data for q1glm1 (Fig. 1)-

## plot predictions for the conditional model
q1glm1.pre.con <- ggpredict(q1glm1, terms = "la", type = "fixed")

q1a <- ggplot(q1glm1.pre.con, aes(x=x, y=predicted)) +
  geom_line(size=1, shape=20, col="black") +
  labs(y="Inflorescence length (cm)", x = expression(paste("Leaf area (cm"^"2",")"))) +
#  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme_classic() +
  geom_point(data=growth, inherit.aes = TRUE, aes(x=la, y=infl), shape=21, col="grey48")

## plot predictions for the zero-inflated model
q1glm1.pre.zi <- ggpredict(q1glm1, terms = "la", type = "zi_prob")

q1b <- ggplot(q1glm1.pre.zi, aes(x=x, y=predicted)) +
  geom_line(size=1, shape=20, col="black") +
  labs(y="Probability of reproductive dormancy", x = expression(paste("Leaf area (cm"^"2",")"))) +
  #geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  ylim(-0.1, 1) +
  theme_classic()

pdf(file = "results/q1glm1.pdf", width = 7, height = 3)
ggarrange(q1a, q1b, ncol = 2, nrow = 1)
dev.off()



## Question 2: Does vegetative fitness of an individual in a given year explain-------------------
## its vegetative or reproductive fitness in the following year?-------------------------

## Load another sheet that provides the data from year0 and year1 side by side
growth2 <- read_excel("data/pico_growth_met.xlsx", sheet = 2, na = "NA")
growth2$year.res <- as.factor(growth2$year.res)
growth2$year.pre <- as.factor(growth2$year.pre)

## Crossed mixed model showing the effect of vegetative fitness on next year's-
## vegetative fitness
q2glm1 <- glmmTMB(la.res ~ la.pre + (1 | year.res) + (1 | plant_no),
              family = tweedie(link = "log"),
              ziformula = ~ la.pre + (1 | year.res) + (1 | plant_no),
              data = growth2)
summary(q2glm1)

##plot the predicted data for q2glm1
###plot predictions for the conditional model
q2glm1.pre.con <- ggpredict(q2glm1, terms = "la.pre", type = "fixed")

q2a <- ggplot(q2glm1.pre.con, aes(x=x, y=predicted)) +
  geom_line(size=1, shape=20, col="black") +
  labs(x = expression(paste("Leaf area in year y"^"t-1", " (cm"^"2",")")), y=expression(paste("Leaf area in year y"^"t", " (cm"^"2",")"))) +
#  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme_classic() +
  geom_point(data=growth2, inherit.aes = TRUE, aes(x=la.pre, y=la.res), shape=21, col="grey48")

###plot predictions for the zero-inflated model
q2glm1.pre.zi <- ggpredict(q2glm1, terms = "la.pre", type = "zi_prob")

q2b <- ggplot(q2glm1.pre.zi, aes(x=x, y=predicted)) +
  geom_line(size=1, shape=20, col="black") +
  labs(x = expression(paste("Leaf area in year y"^"t-1", " (cm"^"2",")")), y=expression(paste("Probability of vegetative dormancy in y"^"t"))) +
#  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  ylim(0, 0.55) +
  theme_classic()

###export the plot
pdf(file = "results/q2glm1.pdf", width = 7.5, height = 3.5)
ggarrange(q2a, q2b, ncol = 2, nrow = 1)
dev.off()

### Crossed mixed model showing the effect of vegetative fitness on next year's--
## reproductive fitness

q2glm2 <- glmmTMB(infl.res ~ la.pre + (1 | year.res) + (1 | plant_no),
                 family = tweedie(link = "log"),
                 ziformula = ~ la.pre + (1 | year.res) + (1 | plant_no),
                 data = growth2)
summary(q2glm2)

## plot the predicted data for q2glm2-
## plot predictions for the conditional model
q2glm2.pre.con <- ggpredict(q2glm2, terms = "la.pre", type = "fixed")

q2.2a <- ggplot(q2glm2.pre.con, aes(x=x, y=predicted)) +
  geom_point(size=2, shape=20, col="black") +
  labs(x = expression(paste("Leaf area in year y"^"t-1", " (cm"^"2",")")), y=expression(paste("Inflorescence length in y"^"t", "(cm)"))) +
  
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme_classic() +
  geom_point(data=growth2, inherit.aes = TRUE, aes(x=la.pre, y=la.res), shape=21, col="grey48")

## plot predictions for the zero-inflated model
q2glm2.pre.zi <- ggpredict(q2glm2, terms = "la.pre", type = "zi_prob")

q2.2b <- ggplot(q2glm2.pre.zi, aes(x=x, y=predicted)) +
  #  geom_point(size=2, shape=20, col="black") +
    geom_line(size=2, shape=20, col="black") +
  labs(x = expression(paste("Leaf area in year y"^"t-1", " (cm"^"2",")")), y=expression(paste("Probability of reproductive dormancy in y"^"t"))) +
 # geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  ## DWS: BUT lm does not make sense here?
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  ylim(0, 1) +
  theme_classic()

## export the plot
pdf(file = "results/q2glm2.pdf", width = 7.5, height = 3.5)
ggarrange(q2.2a, q2.2b, ncol = 2, nrow = 1)
dev.off()



## Question3: Does the reproductive effort made by a plant in a given year-------
## explain its vegetative or reproductive fitness in the following year?--------

## mixed model showing the effect of reproductive fitness in yt-1 on next year's
## vegetative fitness

q3glm1 <- glmmTMB(la.res ~ infl.pre + (1 | year.res) + (1 | plant_no),
                  family = tweedie(link = "log"),
                  ziformula = ~ infl.pre + (1 | year.res),
                  data = growth2)
summary(q3glm1)

##plot the data for q3glm1-
###plot predictions for the conditional model
q3glm1.pre.con <- ggpredict(q3glm1, terms = "infl.pre", type = "fixed")

q3a <- ggplot(q3glm1.pre.con, aes(x=x, y=predicted)) +
  geom_line(size=1, shape=20, col="black") +
  labs(x = expression(paste("Inflorescence length in y"^"t-1", "(cm)")), y=expression(paste("Leaf area in year y"^"t", " (cm"^"2",")"))) +
#  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme_classic() +
  geom_point(data=growth2, inherit.aes = TRUE, aes(x=infl.pre, y=la.res), shape=21, col="grey48")

###plot predictions for the zero-inflated model
q3glm2.pre.zi <- ggpredict(q3glm1, terms = "infl.pre", type = "zi_prob")

q3b <- ggplot(q3glm2.pre.zi, aes(x=x, y=predicted)) +
  geom_line(size=1, shape=20, col="black") +  labs(x = expression(paste("Inflorescence length in y"^"t-1", "(cm)")), y=expression(paste("Probability of vegetative dormancy in y"^"t"))) +
#  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  ylim(0, 0.25) +
  theme_classic()

###export the plot
pdf(file = "results/q3glm1.pdf", width = 7.5, height = 3.5)
ggarrange(q3a, q3b, ncol = 2, nrow = 1)
dev.off()

## mixed model showing the effect of reproductive fitness in yt-1 on next year's
## reproductive fitness

q3glm2 <- glmmTMB(infl.res ~ infl.pre + (1 | year.res) + (1 | plant_no),
                  family = tweedie(link = "log"),
                  ziformula = ~ infl.pre + (1 | year.res),
                  data = growth2)
summary(q3glm2)

##plot the data for q3glm2-
###plot predictions for the conditional model
q3glm2.pre.con <- ggpredict(q3glm2, terms = "infl.pre", type = "fixed")

q3.2a <- ggplot(q3glm2.pre.con, aes(x=x, y=predicted)) +
  geom_line(size=1, col="black") +
  labs(x = expression(paste("Inflorescence length in y"^"t-1", "(cm)")), y=expression(paste("Inflorescence length in y"^"t", "(cm)"))) +
#  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme_classic() +
  geom_point(data=growth2, inherit.aes = TRUE, aes(x=infl.pre, y=infl.res), shape=21, col="grey48")

###plot predictions for the zero-inflated model
q3glm2.pre.zi <- ggpredict(q3glm2, terms = "infl.pre", type = "zi_prob")

q3.2b <- ggplot(q3glm2.pre.zi, aes(x=x, y=predicted)) +
  geom_line(size=1, col="black") +
  labs(x = expression(paste("Inflorescence length in y"^"t-1", "(cm)")), y=expression(paste("Probability of reproductive dormancy in y"^"t"))) +
 # geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  ylim(0, 1) +
  theme_classic()

###export the plot
pdf(file = "results/q3glm2.pdf", width = 7.5, height = 3.5)
ggarrange(q3.2a, q3.2b, ncol = 2, nrow = 1)
dev.off()



## Question 4: Does herbivory experienced by an individual plant explain-------
## patterns of vegetative or reproductive fitness in the subsequent year?-------

q4glm1 <- glmmTMB(la.res ~ herb.pre + (1 | year.res) + (1 | plant_no),
                  family = tweedie(link = "log"),
                  ziformula = ~ herb.pre + (1 | year.res),
                  data = growth2)
summary(q4glm1)

##plot the data for q4glm1-
###plot predictions for the conditional model
q4glm1.pre.con <- ggpredict(q4glm1, terms = "herb.pre", type = "fixed")

q4a <- ggplot(q4glm1.pre.con, aes(x=as.character(x), y=predicted)) +
  geom_line(size=1, color="black") +
  labs(x = expression(paste("Herbivory in y"^"t-1", "(0/1)")), y=expression(paste("Leaf area in year y"^"t", " (cm"^"2",")"))) +
#  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.1)) +
  theme_classic() +
  geom_point(data=growth2, inherit.aes = TRUE, aes(x=as.character(herb.pre), y=la.res), shape=21, col="grey48")

###plot predictions for the zero-inflated model
q4glm1.pre.zi <- ggpredict(q4glm1, terms = "herb.pre", type = "zi_prob")

q4b <- ggplot(q4glm1.pre.zi, aes(x=as.character(x), y=predicted)) +
  geom_line(size=1, color="black") +
  labs(x = expression(paste("Herbivory in y"^"t-1", "(0/1)")), y=expression(paste("Probability of vegetative dormancy in y"^"t"))) +
 # geom_smooth(method=lm, se=FALSE, fullrange=FALSE, level=0.95) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.1)) +
  ylim(0, 1) +
  theme_classic()

###export the plot
pdf(file = "results/q4glm1.pdf", width = 7.5, height = 3.5)
ggarrange(q4a, q4b, ncol = 2, nrow = 1)
dev.off()


########read the file that shows the climatic variability and flowering census and fitness data from about 7-9 year-----------------------

demo = read_excel("data/pico_growth_met.xlsx", sheet = 3, na = "?")

#############Add a linear regression model to show this effect
lm1 = lm(popsize ~ moisture, data = demo)
summary(lm1)
anova(lm1)

lm2 = lm(mean_la ~ moisture, data = demo)
summary(lm2)
anova(lm2)

lm3 = lm(mean_infl ~ moisture, data = demo)
summary(lm3)
anova(lm3)
