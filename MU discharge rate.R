
library(readxl)
library(tidyverse)
library(ggpubr)
library(lme4)
library(easystats)
library(afex)
library(emmeans)
library (Rmisc)
library(ggbeeswarm)
library (sjstats)
library(ggplot2)
library(viridis)
library(ggsignif)


#GM discharge rate - 10, 20% MVIC

#dataset gm -----
GM_1020 <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/GM_10-20.xlsx")

#lmm gm

lmer_GM2_1020 = lmer(MU ~ Groups * as.factor(Intensity) + (RThr | Name),
                data = GM_1020)

qqnorm(residuals(lmer_GM2_1020)); qqline(residuals(lmer_GM2_1020))
anova(lmer_GM2_1020) %>%
  knitr::kable()

posthocgm <- emmeans(lmer_GM2_1020, "Intensity", data=GM_1020)
pairs(posthocgm, adjust="bonferroni")

posthocgm
anova_gm2_1020 <- anova(lmer_GM2_1020)
#omega e eta sq-----
omega_gm_1020 <- omega_sq(anova_gm2_1020)
eta_gm_1020 <- eta_sq(anova_gm2_1020)

GM2_1020.emm = emmeans(lmer_GM2_1020, specs = pairwise ~ Groups:as.factor(Intensity))
GM2_1020.emm


gmm1_1020 <- summarySE(GM_1020, measurevar="MU", groupvars=c("Groups","Intensity"))
gmm1_1020


#graph GM

gm_mu <- GM_1020 %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Intensity),
             y=MU))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Name)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 3.5,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=gmm1_1020, aes(ymin=MU-ci, ymax = MU+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(legend.position = "none")+
  labs(x = " Torque Intensity (% peak isometric torque)", y= " Motor Unit discharge rate (pps)")

gm_mu


#GL discharge rate - 10, 20% MVIC

#dataset gl -----
GL_1020  <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/GL_10-20.xlsx")

#lmm gl
lmer_GL2_1020 = lmer(MU ~ Groups * as.factor(Intensity) + (RThr | Name),
                data = GL_1020)

qqnorm(residuals(lmer_GL2_1020)); qqline(residuals(lmer_GL2_1020))

anova(lmer_GL_1020_1, lmer_GL2_1020)  %>%
  knitr::kable()

posthocGL2_1020 = emmeans(lmer_GL2_1020, specs = pairwise ~ Groups:as.factor(Intensity),
                       adjust="bonferroni")
posthocGL2_1020

anova_gl2_1020 <- anova(lmer_GL2_1020)
eta_gl2_1020 <- eta_sq(anova_gl2_1020)
omega_gl2_1020 <- omega_sq(anova_gl2_1020)

#graph GL

gll1_1020 <- summarySE(GL_1020, measurevar="MU", groupvars=c("Groups","Intensity"))
gll1_1020


gl_mu <- GL_1020 %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Intensity),
             y=MU))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Name)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 3.5,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=gll1_1020, aes(ymin=MU-ci, ymax = MU+ci),
                position = position_nudge(x = -0.2), width = 0, size=1) +
  theme(legend.position = "none")+
  labs(x = " Torque Intensity (% peak isometric torque)", y= " Motor Unit discharge rate (pps)")

gl_mu

mycomparisongl <- list (c("10", "20")) #pvalue stat for GL graph

# SOL discharge rate - 10, 20% MVIC

#dataset SOL ----

SOL_1020  <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/SOL_10-20.xlsx")


lmer_SOL2_1020 = lmer(MU ~ Groups * as.factor(Intensity) + (RThr | Name),
                 data = SOL_1020)

anova(lmer_SOL2_1020) %>%
  knitr::kable()

posthocsolcv <- emmeans(lmer_SOL2_1020, "Intensity", data=SOL_1020)
pairs(posthocsolcv, adjust="bonferroni")

posthocsol_1020cv = emmeans(lmer_SOL2_1020, specs = pairwise ~ Groups:as.factor(Intensity),
                         adjust="bonferroni")
posthocsol_1020cv

anova_sol2_1020 <- anova(lmer_SOL2_1020)
eta_sol2_1020 <- eta_sq(anova_sol2_1020)
omega_sol2_1020 <- omega_sq(anova_sol2_1020)

# graph SOL

soll1_1020 <- summarySE(SOL_1020, measurevar="MU", groupvars=c("Groups","Intensity"))
soll1_1020

sol_mu <- SOL_1020 %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Intensity),
             y=MU))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Name)) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 3.5,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=soll1_1020, aes(ymin=MU-ci, ymax = MU+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(legend.position = "none")+
  labs(x = " Torque Intensity (% peak isometric torque)", y= " Motor Unit discharge rate (pps)")

sol_mu



#final Graphs -------
sol_mu+ theme(axis.title = element_text(size = 12))+
  labs(caption="Data presented as mean and ± 95% CI")+   
  theme(plot.caption = element_text(size = 10))+
  theme(plot.tag = element_text(size = 11))+ 
  theme(axis.text.x = element_text(vjust=1))


ggsave(file = "sol1.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")

gm_mu+  theme(axis.title = element_text(size = 12))+
  labs(caption="Data presented as mean and ± 95% CI")+   
  theme(plot.caption = element_text(size = 10))+
  theme(plot.tag = element_text(size = 11))+ 
  theme(axis.text.x = element_text(vjust=1))
 
ggsave(file = "gm1.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")


gl1 <- gl_mu + stat_compare_means(label = "p.signif", size=6, hide.ns = TRUE) + 
  labs(caption="Data presented as mean and ± 95% CI.   
       ** Denotes statistical difference between intensities, p<0.001.")+
 theme(axis.title = element_text(size = 12))+
  theme(plot.caption = element_text(size = 10))+ 
  theme(axis.text.x = element_text(vjust=1))
  
gl1

ggsave(file = "gl1.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")
