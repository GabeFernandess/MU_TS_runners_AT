
library(readxl)
library(tidyverse)
library(ggpubr)
library(lme4)
library(easystats)
library(afex)
library(emmeans)
library (Rmisc)
library (sjstats)
library(ggplot2)
library(ggsignif)


#GM Covis of MU discharge rate - 10, 20% MVIC-----

#dataset gm 
GM_1020 <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/GM_10-20.xlsx")

#lmm gm
lmer_GM2_CV1020 = lmer(CV ~ Groups * as.factor(Intensity) + (RThr | Name),
                       data = GM_1020)


anova(lmer_GM2_CV1020) %>%
  knitr::kable()
qqnorm(residuals(lmer_GM2_CV1020)); qqline(residuals(lmer_GM2_CV1020))

posthocgmcv <- emmeans(lmer_GM2_CV1020, "Intensity", data=GM_1020)
pairs(posthocgmcv, adjust="bonferroni")

posthocgm_1020cv = emmeans(lmer_GM2_CV1020, specs = pairwise ~ Groups:as.factor(Intensity),
                           adjust="bonferroni")
posthocgm_1020cv


GM2_1020.emm = emmeans(lmer_GM2_CV1020, specs = pairwise ~ Groups:as.factor(Intensity))
GM2_1020.emm

#effect size

anova_gm_CV1020 <- anova(lmer_GM2_CV1020)
#omega e eta sq-----
omega_gm_CV1020 <- omega_sq(anova_gm_CV1020)
eta_gm_CV1020 <- eta_sq(anova_gm_CV1020)


qqnorm(residuals(lmer_GM2_CV1020)); qqline(residuals(lmer_GM2_CV1020))
anova(lmer_GM2_CV1020) %>%
  knitr::kable()

posthocgm <- emmeans(lmer_GM2_CV1020, "Intensity", data=GM_1020)
pairs(posthocgm, adjust="bonferroni")

posthocgm
anova_gm2_cv_1020 <- anova(lmer_GM2_CV1020)
omega_gm2_cv_1020 <- omega_sq(anova_gm2_cv_1020)
eta_gm2_cv_1020 <- eta_sq(anova_gm2_cv_1020)

GM2_1020.emm = emmeans(lmer_GM2_CV1020, specs = pairwise ~ Groups:as.factor(Intensity))
GM2_1020.emm



#GL Covis of MU discharge rate - 10, 20% MVIC ----

#dataset gl
GL_1020  <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/GL_10-20.xlsx")

#lmm gl
lmer_GL2_1020 = lmer(CV ~ Groups * as.factor(Intensity) + (RThr | Name),
                data = GL_1020)

qqnorm(residuals(lmer_GL2_1020)); qqline(residuals(lmer_GL2_1020))

anova(lmer_GL2_1020)  %>%
  knitr::kable()

posthocGL2_1020 = emmeans(lmer_GL2_1020, specs = pairwise ~ Groups:as.factor(Intensity),
                       adjust="bonferroni")
posthocGL2_1020

#effect size

anova_gl2_1020 <- anova(lmer_GL2_1020)
eta_gl2_1020 <- eta_sq(anova_gl2_1020)
omega_gl2_1020 <- omega_sq(anova_gl2_1020)

#SOL Covis of MU discharge rate - 10, 20% MVIC-----

#dataset sol

SOL_1020  <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/SOL_10-20.xlsx")


lmer_SOL2_CV1020 = lmer(CV ~ Groups * as.factor(Intensity) + (RThr | Name),
                       data = SOL_1020)

anova(lmer_SOL2_CV1020) %>%
  knitr::kable()

posthocsol_1020cv = emmeans(lmer_SOL2_CV1020, specs = pairwise ~ Groups:as.factor(Intensity),
                           adjust="bonferroni")
posthocsol_1020cv

#Effect size SolCV-----

anova_sol2_CV1020 <- anova(lmer_SOL2_CV1020)
omega_sol2_CV1020 <- omega_sq(anova_sol2_CV1020)
eta_sol2_CV1020 <- eta_sq(anova_sol2_CV1020)


