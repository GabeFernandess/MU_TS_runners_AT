
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


#GM Recruitment threshold - 10, 20% MVIC-----

#dataset gm
GM_1020 <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/GM_10-20.xlsx")

lmer_GM_rt1020 = lmer(RThr ~ Groups * as.factor(Intensity) + (1 | Name),
                      data = GM_1020)

anova(lmer_GM_rt1020) %>%
  knitr::kable()

posthocsgm_1020rt = emmeans(lmer_GM_rt1020, specs = pairwise ~ Groups:as.factor(Intensity),
                            adjust="bonferroni")
posthocsgm_1020rt
anova_gm2_rt1020 <- anova(lmer_GM_rt1020)
eta_gm2_rt1020 <- eta_sq(lmer_GM_rt1020)


#GL Recruitment threshold - 10, 20% MVIC-----

#dataset gl 
GL_1020  <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/GL_10-20.xlsx")

lmer_GL_rt1020 = lmer(RThr ~ Groups * as.factor(Intensity) + (1 | Name),
                      data = GL_1020)

anova(lmer_GL_rt1020) %>%
  knitr::kable()


posthocsgl_1020rt = emmeans(lmer_GL_rt1020, specs = pairwise ~ Groups:as.factor(Intensity),
                            adjust="bonferroni")
posthocsgl_1020rt
anova_gl2_rt1020 <- anova(lmer_GL_rt1020)
eta_gl2_rt1020 <- eta_sq(lmer_GL_rt1020)


#SOL Recruitment threshold - 10, 20% MVIC-----

#dataset SOL

SOL_1020  <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/SOL_10-20.xlsx")

#SOL
lmer_SOL_rt1020 = lmer(RThr ~ Groups * as.factor(Intensity) + (1 | Name),
                       data = SOL_1020)

anova(lmer_SOL_rt1020) %>%
  knitr::kable()

posthocssol_1020rt = emmeans(lmer_SOL_rt1020, specs = pairwise ~ Groups:as.factor(Intensity),
                             adjust="bonferroni")

anova_sol2_rt1020 <- anova(lmer_SOL_rt1020)
eta_sol2_rt1020 <- eta_sq(lmer_SOL_rt1020)



