## INTRODUCTION TO LINEAR MIXED MODELS: OCC tutorial ----
## from: https://ourcodingclub.github.io/tutorials/mixed-models/
## code and clever comments: Gabriela K Hajduk
## last edit TZ 20200803

### PACKAGES 

library(ggplot2)
library(lme4)
library(ggeffects)
library(tidyverse)
library(sjPlot) # plot re
library(stargazer) # formatted tables

### 1. MIXED EFFECT MODELLING ----

## why: handle messy data (many covariates, structured data, bad sample sizes) and
## saves DF

### 2. EXPLORE DATA  ----
## load and take a look

load(file = "dragons.RData")

head(dragons)

## distribution of testScore (response var)

hist(dragons$testScore) # normal? or close

## standardise explanatory vars (centering 0, scaling 1) to be able to compare effect sizes

dragons$bodyLength2 <- scale(dragons$bodyLength, center = T, scale = T)

### 3. FIT ALL DATA ----

## question: is there an association between body length and test score?

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
## we want to test effect of body length and control for the variation coming from
## mountain ranges:

## use random factors

### 6. MIXED EFFECTS MODELS ----

## account for correlations btw data, use all

## estimate fewer parameters, avoid problems from multiple comparisons (separate
## regressions)

## FIXED vs. RANDOM effects ----
## depends on research question
## fixed = explanatory var in linear regression, what am I making predictions about
## random = grouping factors, want to control for, influence obs pattern, just "noise"
## how much variation due to fixed controlling for random (sample of all the 
## possibilities)
## random could be fixed if interested in predictions about sampled groups
## fixed effect should have at least 5 levels (not M/F!), for confidence reasons

## cool links in the tutorial:
# partitioning of variation, partial pooling to estimate random effects:
# https://dynamicecology.wordpress.com/2015/11/04/is-it-a-fixed-or-random-effect/
# https://stats.stackexchange.com/questions/4700/what-is-the-difference-between-fixed-effect-random-effect-and-mixed-effect-mode

# model complexity
# https://dynamicecology.wordpress.com/2015/02/05/how-many-terms-in-your-model-before-statistical-machismo/
# https://dynamicecology.wordpress.com/2014/12/02/why-are-your-statistical-models-more-complex-these-days/

# definitions: https://stats.stackexchange.com/questions/4700/what-is-the-difference-between-fixed-effect-random-effect-and-mixed-effect-mode/4702#4702

### FIRST MM

## residual variation modelled through variance
## Question: is there an association between body length and test score AFTER
# controlling for the variation in mountain ranges?

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer) 
# random effects: how much variance among levels of grouping factor(s)[60% of total
# variance] plus residual variance; fixed: same as linear

## other links
# https://www.r-bloggers.com/making-sense-of-random-effects/
# https://www.theanalysisfactor.com/understanding-random-effects-in-mixed-models/
# bodo winter tutorial lme analyses

# Once we account for the mountain ranges, body length doesn’t actually explain the
# differences in the test scores. Model extimate is smaller than error, not diff from
# zero

## plots 

plot(mixed.lmer) # ok, no pattern

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer)) # on the line

## CROSSED vs. NESTED factors ----

## crossed = some subjects experienced multiple levels (partially, e.g. found same id
# in multiple plots) or all subjects all levels of an effect (fully, e.g. season)

## nested = hierarchical, sampling has stratification
## HLMs or multilevel models are a kind of MM, factors are in hierarchy
## pseudoreplication, or massively increasing your sampling size by using non-
## independent data
## assumption of independance of observations that is central to linear regression.

# leafLength ~ treatment + (1|Bed/Plant/Leaf) + (1|Season)

## implicit and explicit nesting: site abc for each mt, a means sth only w/ certain mt
# sites as additional rd effect

dragons <- within(dragons, sample <- factor(mountainRange:site))
# code them better this way, sample for the model

# "To sum up: for nested random effects, the factor appears ONLY within a particular
# level of another factor (each site belongs to a specific mountain range and only to
# that range); for crossed effects, a given factor appears in more than one level of
# another factor (dragons appearing within more than one mountain range). Or you can
# just remember that if your random effects aren’t nested, then they are crossed!"

### SECOND MM

mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  
# treats the two random effects as if they are crossed: 3 sites instead of 24

summary(mixed.WRONG)

# Question: Is there an association between body length and intelligence in dragons
# after controlling for variation in mountain ranges and sites within mountain ranges?

mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  

# the syntax stays the same, but now the nesting is taken into account

summary(mixed.lmer2)

# other syntax (1|mountainRange/site) or even (1|mountainRange) + (1|mountainRange:site)

## plot 

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

### random slopes

## random intercept would allow different intercepts for the levels 

mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) # adding the fixed variable into the random effect brackets, random slope in the model

## model the intelligence of dragons as a function of body length, knowing that populations have
## different intelligence baselines and that the relationship may vary among populations

summary(mixed.ranslope)

## plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# body length doesn’t influence the test scores

### 7. PRESENT RESULTS ----

## plot model predictions, with estimates

pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))


(ggplot(pred.mm) +
    geom_line(aes(x = x, y = predicted)) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), fill = "lightgrey", alpha = 0.5) +
    geom_point(data = dragons, aes(x = bodyLength2, y = testScore, colour = mountainRange)) +
    labs(x = "Body Length (scaled)", y = "Test Score", title = "Body Length does not affect intelligence") +
    theme_minimal()
)

## specify: visualise how the relationships vary according to different levels of random effects

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal() 

## !look at the raw data, the summary output, and the predictions all together

## showing the variation among levels of random effects, plot the departure from the 
## overall model
## estimate for intercepts - and slopes, if you have a random slope model

(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = T))

summary(mixed.ranslope)

# https://cran.r-project.org/web/packages/dotwhisker/vignettes/dotwhisker-vignette.html

## tables

stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# https://www.jakeruss.com/cheatsheets/stargazer/

# http://varianceexplained.org/r/broom-intro/

### 8. P-VALUES AND MODEL SELECTION ----

### fixed effects structure

## 10 times more data than parameters you are trying to estimate

#From worst to best:
#Wald Z-tests
#Wald t-tests (but LMMs need to be balanced and nested)
#Likelihood ratio tests (via anova() or drop1())
#MCMC or parametric bootstrap confidence intervals
# https://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi

# With large sample sizes, p-values based on the likelihood ratio are generally 
# considered okay. NOTE: With small sample sizes, you might want to look into 
# deriving p-values using the Kenward-Roger or Satterthwaite approximations (for
# REML models). Check out the pbkrtest package.

full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = F)  # our model
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = F)  # no fixed effects

## compare full and reduced

anova(reduced.lmer, full.lmer)  # the two models are not significantly different

# REML = F: residual/restricted ML is the default parameter estimation criterion
# for lmm, REML estimates of variance components are preferred as less biased (in
# the previous models it was true)

# REML assumes that the fixed effects structure is correct. You should use ML when
# comparing models with different fixed effects, as ML doesn't rely on the coefficients
# of the fixed effects - and that's why we are refitting our full and reduced models
# above with the addition of REML = FALSE in the call.

## Even though you use ML to compare models, you should report parameter estimates from your final "best" REML model, as ML may underestimate variance of the random effects.

# NOTE 2: Models can also be compared using the AICc function from the AICcmodavg
# package. The Akaike Information Criterion (AIC) is a measure of model quality. 
# AICc corrects for bias created by small sample size when estimating AIC. Generally,
# if models are within 2 AICc units of each other they are very similar. Within 5
# units they are quite similar, over 10 units difference and you can probably be happy
# with the model with lower AICc. As with p-values though, there is no "hard line"
# that's always correct.

# NOTE 3: There isn't really an agreed upon way of dealing with the variance from the
# random effects in mixed models when it comes to assessing significance. Both p-
# values and effect sizes have issues, although from what I gather, p-values seem to 
# cause more disagreement than effect sizes, at least in the R community.

### random effects structure

# pseudoreplication: ns is not a problem, part of the exp design 
# account for variability (season or year temporal effects like drought)
# model selection 

# Following Zuur's advice (2009), we use REML estimators for comparison of models 
# with different random effects (we keep fixed effects constant). (Zuur: "Two models
# with nested random structures cannot be done with ML because the estimators for the
# variance terms are biased." )

# NOTE: Do NOT vary random and fixed effects at the same time - either deal with your
# random effects structure or with your fixed effects structure at any given point.

# NOTE 2: Do NOT compare lmer models with lm models (or glmer with glm).

### entire model selection 

# There are two ways here:
# (i) "top-down", where you start with a complex model and gradually reduce it,
# (ii) "step up", where you start with a simple model and add new variables to it.
# Unfortunately, you might arrive at different final models by using those strategies
# and so you need to be careful.

# The model selection process recommended by Zuur et al. (2009) is a top-down strategy
# and goes as follows:
  
# 1. fit a full model (he even recommends "beyond optimal" i.e. more complex than
# you'd expect or want it to be)
# 2. sort out the random effects structure (use REML likelihoods or REML AIC or BIC)
# 3. sort out fixed effects structure (either use REML the F-statistic or the t-
# statistic or compare nested ML models - keep your random effects constant)
# 4. once you arrive at the final model present it using REML estimation
# NOTE: At the risk of sounding like a broken record: I think it's best to decide on
# what your model is based on biology/ecology/data structure etc. than through
# following model selection blindly. Additionally, just because something is non-
# significant doesn't necessarily mean you should always get rid of it.


