rm(list=ls())
library(lme4)
library(MASS)
library(pscl)
library(lmtest)
library(fitdistrplus)
library(visreg)
library(dplyr)
library(ggplot2)

df_tenure <- read.csv("Gandak_fishingtenurehydro_data.csv",stringsAsFactors = T)
summary(df_tenure)
str(df_tenure)
table(df_tenure$encrate)
df_tenure <- df_tenure%>% mutate(dist_barrage_s = scale(dist_barrage, center = TRUE, scale = TRUE)[,1])
############----------------------------------------------------------
##GRDs
grd_tenure <- df_tenure%>%filter(species=="GRD")
str(grd_tenure)
table(grd_tenure$count)
grd_tenure$year <- as.factor(grd_tenure$year)
grd_tenure$yearindex <- as.factor(grd_tenure$yearindex)

##Graphical exploration
plot(grd_tenure$encrate~grd_tenure$season)
plot(grd_tenure$encrate~grd_tenure$fishregime)
plot(grd_tenure$encrate~grd_tenure$year)
ggplot(aes(y=encrate, x=dist_barrage, group=year,color=year), data=grd_tenure)+
  geom_line()
ggplot(aes(y=encrate, x=dist_barrage), data=grd_tenure)+
  geom_point()+geom_smooth()
ggplot(aes(y=encrate, x=fishingactivity), data=grd_tenure)+
  geom_point()+geom_smooth()
ggplot(aes(y=encrate, x=fishingboats), data=grd_tenure)+
  geom_point()+geom_smooth()
ggplot(aes(y=encrate, x=stretch_order), data=grd_tenure)+
  geom_point()+geom_smooth()

cor_df <- grd_tenure%>%select(stretch_order,fishingactivity,fishingboats,dist_barrage)
cor(cor_df,  method="pearson") 
##fishing boats and fishing activity: high correlation 
##stretch_order and dist_barrage: high correlation

##Checking distribution of 'count'
fit.poisson <- fitdist(grd_tenure$count,"pois")
fit.negbin <- fitdist(grd_tenure$count,"nbinom")
par(mfrow=c(1,2), mar=c(4,4,2,2))
cdfcomp(list(fit.poisson, fit.negbin),horizontals=F, addlegend=T,legendtext=c("Poisson","NegBinom"))
qqcomp(list(fit.poisson,fit.negbin),addlegend=T,legendtext=c("Poisson","NegBinom"))

##Checking models using poisson and negbinom distr
##using offset to account for survey length variability
grd_m1_p <- glm(count~fishregime,offset=log(length_km), family='poisson', data=grd_tenure)
summary(grd_m1_p) ##res. deviance to df ratio > 3

grd_m1_nb <- glm.nb(count~fishregime+offset(log(length_km)), data=grd_tenure)
summary(grd_m1_nb) ##res. deviance to df ratio ~1.3

lrtest(grd_m1_p, grd_m1_nb)
##going forward with negbinom
grd_m2 <- glm.nb(count~dist_barrage_s+offset(log(length_km)), data=grd_tenure)
summary(grd_m2)

grd_m2a <- glm.nb(count~dist_barrage_s+I(dist_barrage_s^2)+offset(log(length_km)), data=grd_tenure)
grd_m2a_sum <- summary(grd_m2a)
anova(grd_m2,grd_m2a, test='Chisq')

grd_m3 <- glm.nb(count~dist_barrage_s+I(dist_barrage_s^2)+fishregime+offset(log(length_km)), data=grd_tenure)
summary(grd_m3)
anova(grd_m3,grd_m2a, test='Chisq') ##no significant effect of fishingregime

##continuing with model grd_m2a, with addition of new covariates
grd_m4 <- glm.nb(count~dist_barrage_s+I(dist_barrage_s^2)+yearindex+offset(log(length_km)), data=grd_tenure)
summary(grd_m4)
anova(grd_m4,grd_m2a, test='Chisq') ##no significant effect of yearindex

##continuing with model grd_m2a, with addition of new covariates
grd_m5 <- glm.nb(count~dist_barrage_s+I(dist_barrage_s^2)+fishingactivity+offset(log(length_km)), data=grd_tenure)
summary(grd_m5)
anova(grd_m5,grd_m2a, test='Chisq') ##no significant effect of fishingactivity

grd_m6 <- glm.nb(count~dist_barrage_s+I(dist_barrage_s^2)+fishingboats+offset(log(length_km)), data=grd_tenure)
summary(grd_m6)
anova(grd_m6,grd_m2a, test='Chisq') ##no significant effect of fishingboats

##checking random effects of yearindex
grd_m7 <- glmer.nb(count~dist_barrage_s+I(dist_barrage_s^2)+(1|yearindex)+offset(log(length_km)), data=grd_tenure)
summary(grd_m7)

##checking random effects of fishing regime
grd_m8 <- glmer.nb(count~dist_barrage_s+I(dist_barrage_s^2)+(1|fishregime)+offset(log(length_km)), data=grd_tenure)
summary(grd_m8)

##Analysis of residuals for best model - m2a
par(mfrow=c(2,2))
plot(grd_m2a, which=c(1,2,4))
plot(resid(grd_m2a)~grd_tenure$dist_barrage);abline(0,0)
par(mfrow=c(1,1))

##Exporting results for grd count: m2a
grd_count_m2a_estimate <- data.frame(grd_m2a_sum[["coefficients"]])
rownames(grd_count_m2a_estimate) <- c("intercept","dist_barrage(scaled)","dist_barrage(scaled)^2")

write.table(grd_count_m2a_estimate, file = "grd_count_estimate.txt", sep = "\t",
            row.names = T)

##Modelling the encounter rate for GRDs------
grd_encrate_m1 <- glm.nb(encrate~fishregime, weights=length_km, data=grd_tenure)
summary(grd_encrate_m1)

grd_encrate_m2 <- glm.nb(encrate~dist_barrage_s,weights=length_km, data=grd_tenure)
summary(grd_encrate_m2)

grd_encrate_m3 <- glm.nb(encrate~dist_barrage_s+I(dist_barrage_s^2), weights=length_km, data=grd_tenure)
summary(grd_encrate_m3)
anova(grd_encrate_m3,grd_encrate_m2, test='Chisq')

grd_encrate_m4 <- glm.nb(encrate~dist_barrage_s+I(dist_barrage_s^2)+fishregime,weights=length_km,data=grd_tenure)
summary(grd_encrate_m4)

anova(grd_encrate_m4,grd_encrate_m3, test='Chisq') ##no significant effect of fishingregime

grd_encrate_m5 <- glm.nb(encrate~dist_barrage_s+I(dist_barrage_s^2)+yearindex,
                         weights=length_km,data=grd_tenure)
summary(grd_encrate_m5)

anova(grd_encrate_m5,grd_encrate_m3, test='Chisq') 

grd_encrate_m6 <- glm.nb(encrate~dist_barrage_s+I(dist_barrage_s^2)+yearindex+fishingactivity,
                         weights=length_km,data=grd_tenure)
grd_encrate_m6_sum <- summary(grd_encrate_m6)

anova(grd_encrate_m6,grd_encrate_m5, test='Chisq') 

grd_encrate_m7 <- glm.nb(encrate~dist_barrage_s+I(dist_barrage_s^2)+yearindex+fishingboats,
                         weights=length_km,data=grd_tenure)
grd_encrate_m7_sum <- summary(grd_encrate_m7)

anova(grd_encrate_m7,grd_encrate_m5, test='Chisq') 

AIC(grd_encrate_m1,grd_encrate_m2,grd_encrate_m3,grd_encrate_m4,
    grd_encrate_m5, grd_encrate_m6, grd_encrate_m7)

##Exporting results for grd encounter rate: m6 and m7
grd_encrate_m6_estimate <- data.frame(grd_encrate_m6_sum[["coefficients"]])
rownames(grd_encrate_m6_estimate) <- c("intercept","dist_barrage(scaled)","dist_barrage(scaled)^2",
                                        "yearindex2", "yearindex3","fishingactivity")
write.table(grd_encrate_m6_estimate, file = "grd_encrate_m6_estimate.txt", sep = "\t",
            row.names = T)

grd_encrate_m7_estimate <- data.frame(grd_encrate_m7_sum[["coefficients"]])
rownames(grd_encrate_m7_estimate) <- c("intercept","dist_barrage(scaled)","dist_barrage(scaled)^2",
                                       "yearindex2", "yearindex3","fishingboats")
write.table(grd_encrate_m7_estimate, file = "grd_encrate_m7_estimate.txt", sep = "\t",
            row.names = T)
############----------------------------------------------------------
##Gharials
gharial_tenure <- df_tenure%>%filter(species=="gharial")
str(gharial_tenure)
table(gharial_tenure$count) ##~36 % zeros
gharial_tenure$year <- as.factor(gharial_tenure$year)
gharial_tenure$yearindex <- as.factor(gharial_tenure$yearindex)

##Graphical exploration
plot(gharial_tenure$encrate~gharial_tenure$season)
plot(gharial_tenure$encrate~gharial_tenure$fishregime)
plot(gharial_tenure$encrate~gharial_tenure$year)
ggplot(aes(y=encrate, x=dist_barrage, group=year,color=year), data=gharial_tenure)+
  geom_line()
ggplot(aes(y=encrate, x=dist_barrage), data=gharial_tenure)+
  geom_point()+geom_smooth()
ggplot(aes(y=encrate, x=fishingactivity), data=gharial_tenure)+
  geom_point()+geom_smooth()
ggplot(aes(y=encrate, x=fishingboats), data=gharial_tenure)+
  geom_point()+geom_smooth()
ggplot(aes(y=encrate, x=stretch_order), data=gharial_tenure)+
  geom_point()+geom_smooth()

cor_df_gharial <- grd_tenure%>%select(stretch_order,fishingactivity,fishingboats,dist_barrage)
cor(cor_df_gharial) 

##Checking distribution of 'count'
fit.poisson2 <- fitdist(gharial_tenure$count,"pois")
fit.negbin2 <- fitdist(gharial_tenure$count,"nbinom")
par(mfrow=c(1,2), mar=c(4,4,2,2))
cdfcomp(list(fit.poisson2, fit.negbin2),horizontals=F, addlegend=T,legendtext=c("Poisson","NegBinom"))
qqcomp(list(fit.poisson2,fit.negbin2),addlegend=T,legendtext=c("Poisson","NegBinom"))

##Checking models using poisson and negbinom distr
gharial_m1_p <- glm(count~fishregime,offset=log(length_km), family='poisson', data=gharial_tenure)
summary(grd_m1_p) 

gharial_m1_nb <- glm.nb(count~fishregime+offset(log(length_km)), data=gharial_tenure)
summary(grd_m1_nb) 

lrtest(gharial_m1_p, gharial_m1_nb) ##NB has significantly higher log-likelihood

##Using ZINB to account for excess of zeroes
gharial_m_null <- zeroinfl(formula = count ~ 1 + offset(log(length_km))| 1, data = gharial_tenure, dist = "negbin")
summary(gharial_m_null) #null model
  
gharial_m2 <- zeroinfl(formula = count ~ fishregime + offset(log(length_km))| 1, data = gharial_tenure, dist = "negbin")
summary(gharial_m2) 
lrtest(gharial_m2,gharial_m_null) #no significant effect of fishing regime on count part

gharial_m3 <- zeroinfl(formula = count ~ dist_barrage_s + offset(log(length_km))| 1, data = gharial_tenure, dist = "negbin")
summary(gharial_m3)
lrtest(gharial_m3,gharial_m_null) #no significant effect of dist_barrage on count part

gharial_m4 <- zeroinfl(formula = count ~ dist_barrage_s + I(dist_barrage_s^2) + offset(log(length_km))| 1, data = gharial_tenure, dist = "negbin")
summary(gharial_m4) 
lrtest(gharial_m4,gharial_m_null) 

gharial_m5 <- zeroinfl(formula = count ~ yearindex + offset(log(length_km))| 1, data = gharial_tenure, dist = "negbin")
summary(gharial_m5)
lrtest(gharial_m5,gharial_m_null) #no significant effect of year on count part

gharial_m6 <- zeroinfl(formula = count ~ fishingactivity + offset(log(length_km))| 1, data = gharial_tenure, dist = "negbin")
summary(gharial_m6)
lrtest(gharial_m6,gharial_m_null) #no significant effect of fishingactivity on count part

gharial_m7 <- zeroinfl(formula = count ~ fishingboats + offset(log(length_km))| 1, data = gharial_tenure, dist = "negbin")
summary(gharial_m7)
lrtest(gharial_m7,gharial_m_null) #no significant effect of fishingboats on count part

gharial_m8 <- zeroinfl(formula = count ~ 1 + offset(log(length_km))| fishingactivity, data = gharial_tenure, dist = "negbin")
summary(gharial_m8) #SE >>> estimates in ZI component

gharial_m9 <- zeroinfl(formula = count ~ 1 + offset(log(length_km))| fishingboats, data = gharial_tenure, dist = "negbin")
summary(gharial_m9) #SE >>> estimates in ZI component

##ZI models dont seem to converge well, better to use neg-binom models
gharial_m_null_nb <- glm.nb(count~1 + offset(log(length_km)), data=gharial_tenure)
summary(gharial_m_null_nb)
anova(gharial_m_null_nb, gharial_m1_nb, test = 'Chisq')

gharial_m2_nb <- glm.nb(count~dist_barrage_s+offset(log(length_km)), data=gharial_tenure)
summary(gharial_m2_nb)
anova(gharial_m_null_nb, gharial_m2_nb, test = 'Chisq')

gharial_m3_nb <- glm.nb(count~yearindex+offset(log(length_km)), data=gharial_tenure)
summary(gharial_m3_nb)
anova(gharial_m_null_nb, gharial_m3_nb, test = 'Chisq')

gharial_m4_nb <- glm.nb(count~fishingactivity+offset(log(length_km)), data=gharial_tenure)
summary(gharial_m4_nb)
anova(gharial_m_null_nb, gharial_m4_nb, test = 'Chisq')

gharial_m5_nb <- glm.nb(count~fishingboats+offset(log(length_km)), data=gharial_tenure)
summary(gharial_m5_nb)
anova(gharial_m_null_nb, gharial_m5_nb, test = 'Chisq')

##Modelling the encounter rate for gharials------
gharial_encrate_m_null <- glm.nb(encrate~1, weights=length_km, data=gharial_tenure)
summary(gharial_encrate_m_null)

gharial_encrate_m1 <- glm.nb(encrate~fishregime, weights=length_km, data=gharial_tenure)
summary(gharial_encrate_m1)
anova(gharial_encrate_m_null, gharial_encrate_m1, test='Chisq')

gharial_encrate_m2 <- glm.nb(encrate~fishregime+dist_barrage_s,weights=length_km, data=gharial_tenure)
summary(gharial_encrate_m2)
anova(gharial_encrate_m1,gharial_encrate_m2, test='Chisq')

gharial_encrate_m3 <- glm.nb(encrate~fishregime+dist_barrage_s+I(dist_barrage_s^2), weights=length_km, data=gharial_tenure)
summary(gharial_encrate_m3)
anova(gharial_encrate_m2,gharial_encrate_m3, test='Chisq')

gharial_encrate_m4 <- glm.nb(encrate~fishregime+dist_barrage_s+yearindex,
                         weights=length_km,data=gharial_tenure)
summary(gharial_encrate_m4)
anova(gharial_encrate_m2,gharial_encrate_m4, test='Chisq')

gharial_encrate_m5 <- glm.nb(encrate~fishregime+dist_barrage_s+fishingactivity,
                             weights=length_km,data=gharial_tenure)
gharial_encrate_m5_sum <- summary(gharial_encrate_m5)
anova(gharial_encrate_m2,gharial_encrate_m5, test='Chisq')

gharial_encrate_m6 <- glm.nb(encrate~fishregime+dist_barrage_s+fishingboats,
                             weights=length_km,data=gharial_tenure)
summary(gharial_encrate_m6)
anova(gharial_encrate_m2,gharial_encrate_m6, test='Chisq')

##Exporting results for gharial encounter rate: m5
gharial_encrate_m5_estimate <- data.frame(gharial_encrate_m5_sum[["coefficients"]])
rownames(gharial_encrate_m5_estimate) <- c("intercept","fishingregime-cooperative","fishingregime-free",
                                           "dist_barrage (scaled)", "fishingactivity")
write.table(gharial_encrate_m5_estimate, file = "gharial_encrate_m5_estimate.txt", sep = "\t",
            row.names = T)
