## INTRODUCTION TO LINEAR MIXED MODELS: OCC tutorial ----
## edited TZ 20200801

### PACKAGES 

library(ggplot2)
library(lme4)

### 1. MIXED EFFECT MODELLING ----

## why: handle messy data (many covariates, structured data, bad sample sizes) and saves DF

### 2. EXPLORE DATA  ----
## load and take a look

load(file = "dragons.RData")

head(dragons)

## distribution of testScore (response var)

hist(dragons$testScore) # normal? or close

## standardise explanatory vars (centering 0, scaling 1) to be able to compare effect sizes

dragons$bodyLength2 <- scale(dragons$bodyLength, center = T, scale = T)

### 3. FIT ALL DATA ----

## linear model 

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)

summary(basic.lm)

## plot lm 

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

### Assumptions:

## residuals (should be flat)

plot(basic.lm, which = 1) # not flat, but not too bad

## big sample site, less trend

## qq plot (should be on the line)

plot(basic.lm, which = 2) # extremes off, but not too bad

## observation independence?

boxplot(testScore ~ mountainRange, data = dragons)

## look at points: not independent, MtRange varies in body size and score

ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange))+
  geom_point(size = 2)+
  theme_classic()+
  theme(legend.position = "none")

### 4. MULTIPLE ANALYSES ----

## regression for each range?

(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) +
   geom_point() +
   facet_wrap( ~ mountainRange) +
   xlab("length") +
   ylab("test score"))

## for each site ?? too many

## type I error doing multiple comparisons

### 5. MODIFY MODEL ----

## add as fixed effect

mountain.lm <- lm(data = dragons, testScore ~ bodyLength2 + mountainRange)

summary(mountain.lm) # body length non significant

## here it is testing for differences among mt ranges, not what we want
## we want to test effect of body length and control for the variation coming from mountain ranges: use random factors

### 6. MIXED EFFECTS MODELS ----
