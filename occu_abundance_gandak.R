rm(list=ls())
library(unmarked)
library(detect)
library(AICcmodavg)
library(modelsummary)
library(broom)
library(tidyverse)
library(ggplot2)

grd <- read.csv("grd_Occu_Gandak.csv")
summary(grd)
covariate <- read.csv("Covariate_Gandak.csv",stringsAsFactors = T)
covariate <- covariate[-211,]
summary(covariate)
gharial <- read.csv("Gharial_Occu_Gandak.csv")
summary(gharial)
############----------------------------------------------------------
##Occupancy of grds at 5km scale
##Exploratory analysis
grd_cov_merged <- merge(grd,covariate,by=c("reach_ID","replicate_segt"))
##distance along river
dist_occ1 <- ggplot(data=grd_cov_merged,aes(x=dist_km.x,y=grd2010_occ))+
           geom_point()+theme_bw()
dist_occ2 <- ggplot(data=grd_cov_merged,aes(x=dist_km.x,y=grd2013_occ))+
           geom_point()+theme_bw()
dist_occ3 <- ggplot(data=grd_cov_merged,aes(x=dist_km.x,y=grd2017_occ))+
           geom_point()+theme_bw()

##fishing activity
fishingactivity_occ1 <- grd_cov_merged%>%filter(grd2010_occ==1)%>%
  ggplot(aes(x=fishingactivity_2010))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2010)")+theme_bw()

fishingactivity_occ2 <- grd_cov_merged%>%filter(grd2013_occ==1)%>%
  ggplot(aes(x=fishingactivity_2014))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2014)")+theme_bw()

fishingactivity_occ3 <- grd_cov_merged%>%filter(grd2017_occ==1)%>%
  ggplot(aes(x=fishingactivity_2017))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2017)")+theme_bw()

fishingactivity_occ <- ggarrange(fishingactivity_occ1, fishingactivity_occ2, fishingactivity_occ3, ncol = 1, nrow = 3)

ggsave("fishingactivity_occ.png", plot=fishingactivity_occ,width=40,height=40,unit="cm",dpi=500)    

##fishing boats
fishingboats_occ1 <- grd_cov_merged%>%filter(grd2010_occ==1)%>%
  ggplot(aes(x=fishingboats_2010))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ2 <- grd_cov_merged%>%filter(grd2013_occ==1)%>%
  ggplot(aes(x=fishingboats_2014))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ3 <- grd_cov_merged%>%filter(grd2017_occ==1)%>%
  ggplot(aes(x=fishingboats_2017))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2017)")+
  theme_bw()

##formatting variables to unmarked format---------
##creating new dataset for modelling occupancy and abundance at 5km segment scale
covariate_occ5km <- covariate%>%group_by(reach_ID)%>%
  summarise(
    median_dist_km=median(dist_km),
    total_fishingactivity_2010=sum(fishingactivity_2010),
    total_fishingactivity_2014=sum(fishingactivity_2014),
    total_fishingactivity_2017=sum(fishingactivity_2017),
    total_fishingboats_2010=sum(fishingboats_2010),
    total_fishingboats_2014=sum(fishingboats_2014),
    total_fishingboats_2017=sum(fishingboats_2017),
    season_2010=unique(season_2010),
    season_2014=unique(season_2014),
    season_2017=unique(season_2017))
summary(covariate_occ5km)
str(covariate_occ5km)

##formatting occu data 
grd_occ1 <- grd%>%select(reach_ID,replicate_segt,grd2010_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2010_occ)
grd_occ2 <- grd%>%select(reach_ID,replicate_segt,grd2013_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2013_occ)
grd_occ3 <- grd%>%select(reach_ID,replicate_segt,grd2017_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2017_occ)
grd_occ_list <- list(grd_occ1,grd_occ2,grd_occ3)
grd_occ <- grd_occ_list%>%reduce(full_join,by="reach_ID")
grd_occ_mat <- data.matrix(grd_occ[,2:16])

##fishing activity - obscov (in unmarked)
cov_fishingactivity2010 <- covariate%>%select(reach_ID,replicate_segt,fishingactivity_2010)%>%pivot_wider(names_from = replicate_segt,values_from = fishingactivity_2010)
cov_fishingactivity2014 <- covariate%>%select(reach_ID,replicate_segt,fishingactivity_2014)%>%pivot_wider(names_from = replicate_segt,values_from = fishingactivity_2014)
cov_fishingactivity2017 <- covariate%>%select(reach_ID,replicate_segt,fishingactivity_2017)%>%pivot_wider(names_from = replicate_segt,values_from = fishingactivity_2017)
cov_fishingactivity_list <- list(cov_fishingactivity2010,
                                 cov_fishingactivity2014,
                                 cov_fishingactivity2017)
cov_fishingactivity <- cov_fishingactivity_list%>%reduce(full_join,by="reach_ID")
cov_fishingactivity_mat <- data.matrix(cov_fishingactivity[,2:16])

##fishing boats - obscov (in unmarked)
cov_fishingboats2010 <- covariate%>%select(reach_ID,replicate_segt,fishingboats_2010)%>%pivot_wider(names_from = replicate_segt,values_from = fishingboats_2010)
cov_fishingboats2014 <- covariate%>%select(reach_ID,replicate_segt,fishingboats_2014)%>%pivot_wider(names_from = replicate_segt,values_from = fishingboats_2014)
cov_fishingboats2017 <- covariate%>%select(reach_ID,replicate_segt,fishingboats_2017)%>%pivot_wider(names_from = replicate_segt,values_from = fishingboats_2017)
cov_fishingboats_list <- list(cov_fishingboats2010,
                              cov_fishingboats2014,
                              cov_fishingboats2017)
cov_fishingboats <- cov_fishingboats_list%>%reduce(full_join,by="reach_ID")
cov_fishingboats_mat <- data.matrix(cov_fishingboats[,2:16])

cov_fishing <- list(fishingact=cov_fishingactivity_mat,fishingboat=cov_fishingboats_mat)

##Testing correlation between fishing act and fishing boats as obsCovs
stack_fishingact <- stack(covariate,select=c("fishingactivity_2010","fishingactivity_2014","fishingactivity_2017"))
stack_fishingboat <- stack(covariate,select=c("fishingboats_2010","fishingboats_2014","fishingboats_2017"))
cor(stack_fishingact$values,stack_fishingboat$values) ##~0.244

##Bundling into unmarked multi-season frame
grd_occ_df <- unmarkedMultFrame(y=grd_occ_mat,siteCovs = covariate_occ5km[,2],
                         yearlySiteCovs=list(total_fishingactivity=covariate_occ5km[,3:5],
                                             total_fishingboats=covariate_occ5km[,6:8],
                                             season=covariate_occ5km[,9:11]
                                             ), 
                         obsCovs=cov_fishing,
                         numPrimary=3) 
summary(grd_occ_df) 
grd_occ_df@yearlySiteCovs[["season"]] <- relevel(grd_occ_df@yearlySiteCovs[["season"]], ref = "Jan")  

##Models
grd_occ_null <- colext(~1, ~1, ~1, ~1, grd_occ_df)
grd_occ_null_sum <- summary(grd_occ_null)
backTransform(grd_occ_null,type="psi")
backTransform(grd_occ_null,type="col")
backTransform(grd_occ_null,type="ext")
backTransform(grd_occ_null,type="det")

grd_occ_model2 <- colext(~1, ~total_fishingactivity, ~total_fishingactivity, ~1, grd_occ_df)
summary(grd_occ_model2) ##for col: SE>>estimate

grd_occ_model2a <- colext(~1, ~total_fishingactivity, ~1, ~1, grd_occ_df)
summary(grd_occ_model2a)

grd_occ_model2b <- colext(~1, ~1, ~total_fishingactivity, ~1, grd_occ_df)
summary(grd_occ_model2b)

grd_occ_model3 <- colext(~1, ~total_fishingboats, ~total_fishingboats, ~1, grd_occ_df)
summary(grd_occ_model3) ##Nans produced

grd_occ_model3a <- colext(~1, ~total_fishingboats, ~1, ~1, grd_occ_df)
summary(grd_occ_model3a) ##SE>>estimate

grd_occ_model3b <- colext(~1, ~1, ~total_fishingboats, ~1, grd_occ_df)
summary(grd_occ_model3b) ##Nans produced

grd_occ_model4 <- colext(~1, ~season, ~season, ~1, grd_occ_df)
summary(grd_occ_model4)

grd_occ_model4a <- colext(~1, ~season, ~1, ~1, grd_occ_df)
summary(grd_occ_model4a)

grd_occ_model4b <- colext(~1, ~1, ~season, ~1, grd_occ_df)
summary(grd_occ_model4b)

grd_occ_model5 <- colext(~median_dist_km, ~1, ~1, ~1, grd_occ_df)
summary(grd_occ_model5) ##SE>>estimate

grd_occ_model6 <- colext(~median_dist_km, ~total_fishingboats, ~1, ~1, grd_occ_df)
summary(grd_occ_model6) ##SE>>estimate

grd_occ_model7 <- colext(~median_dist_km, ~total_fishingactivity, ~total_fishingactivity, ~1, grd_occ_df)
summary(grd_occ_model7) ##for col and dist_km:SE>>estimate

grd_occ_model7a <- colext(~median_dist_km, ~total_fishingactivity, ~1, ~1, grd_occ_df)
summary(grd_occ_model7a) ##for col and dist_km:SE>>estimate

grd_occ_model7b <- colext(~median_dist_km, ~1, ~total_fishingactivity, ~1, grd_occ_df)
summary(grd_occ_model7b) ##for dist_km: SE>>estimate

grd_occ_model8 <- colext(~1, ~1, ~1, ~fishingact, grd_occ_df) ##estimates close to zero
summary(grd_occ_model8) 

grd_occ_model9 <- colext(~1, ~1, ~1, ~fishingboat, grd_occ_df) ##estimates close to zero
summary(grd_occ_model9)

grd_occ_models <- fitList('psi(.)gam(.)eps(.)p(.)' = grd_occ_null,
                          'psi(.)gam(fishingact)eps(fishingact)p(.)' = grd_occ_model2,
                          'psi(.)gam(fishingact)eps(.)p(.)' = grd_occ_model2a,
                          'psi(.)gam(.)eps(fishingact)p(.)' = grd_occ_model2b,
                          'psi(.)gam(fishingboat)eps(.)p(.)' = grd_occ_model3a,
                          'psi(.)gam(season)eps(season)p(.)' = grd_occ_model4,
                          'psi(.)gam(season)eps(.)p(.)' = grd_occ_model4a,
                          'psi(.)gam(.)eps(season)p(.)' = grd_occ_model4b,
                          'psi(dist_km)gam(.)eps(.)p(.)' = grd_occ_model5,
                          'psi(dist_km)gam(fishingboat)eps()p(.)' = grd_occ_model6,
                          'psi(dist_km)gam(fishingact)eps(fishingact)p(.)' = grd_occ_model7,
                          'psi(dist_km)gam(fishingact)eps(.)p(.)' = grd_occ_model7a,
                          'psi(dist_km)gam(.)eps(fishingact)p(.)' = grd_occ_model7b, 
                          'psi(.)gam(.)eps(.)p(fishingact)' = grd_occ_model8,
                          'psi(.)gam(.)eps(.)p(fishingboat)' = grd_occ_model9     
                          )
                  
grd_occ_models_AIC <- modSel(grd_occ_models)
grd_occ_models_AICtable <- grd_occ_models_AIC@Full[c("model","negLogLike","nPars","n","AIC","delta","AICwt","cumltvWt")]

write.table(grd_occ_models_AICtable, file = "grd_occ_AICtable.txt", sep = "\t",
            row.names = F)

##Custom function to extract estimates using modelsummary
##for class of models: 'unmarkedFitColExt'
tidy.unmarkedFitColExt <- function(m, ...) {
  m_sum <- summary(m)
  result <- dplyr::bind_rows(m_sum) 
  ret <- data.frame(
    term      = row.names(result),
    estimate  = result[,1],
    std.error = result[,2],
    z         = result[,3],
    p         = result[,4])
  ret
}

glance.unmarkedFitColExt <- function(m, ...) {
  ret <- data.frame(
    AIC = m@AIC,
    negLogLike   = m@negLogLike)
  ret
}

##Exporting coefficients of best (<2 delta AIC) and null models
grd_occ5km_list <- list(
  "Best"     = grd_occ_model4,
  "Second best" = grd_occ_model4a,
  "Third best" = grd_occ_model4b,
  "Null" = grd_occ_null
)

modelsummary(grd_occ5km_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "grd_occ5km_table.docx")
modelsummary(grd_occ_model4)
modelsummary(grd_occ_model4a)
modelsummary(grd_occ_model4b)
modelsummary(grd_occ_null)

############----------------------------------------------------------
##Abundance of grds at 5km scale
grd_count5km <- grd%>%group_by(reach_ID)%>%
  summarise(
    total_count_2010=sum(grd2010_count),
    total_count_2013=sum(grd2013_count),
    total_count_2017=sum(grd2017_count))

grd_count_mat <- data.matrix(grd_count5km[,2:4])  

grd_count_df <- unmarkedFramePCO(y=grd_count_mat,siteCovs = covariate_occ5km[,2],
                                 yearlySiteCovs=list(total_fishingactivity=covariate_occ5km[,3:5],
                                                     total_fishingboats=covariate_occ5km[,6:8],
                                                     season=covariate_occ5km[,9:11]), 
                                 obsCovs=list(total_fishingactivity=covariate_occ5km[,3:5],
                                              total_fishingboats=covariate_occ5km[,6:8],
                                              season=covariate_occ5km[,9:11]),
                                 numPrimary=3) 
summary(grd_count_df) 
grd_count_df@yearlySiteCovs[["season"]] <- relevel(grd_count_df@yearlySiteCovs[["season"]], ref = "Jan")  
grd_count_df@obsCovs[["season"]] <- relevel(grd_count_df@obsCovs[["season"]], ref = "Jan")  

##Following guidelines from AHM book Vol.2
##Using dynamics="trend"
m1 <- pcountOpen(lam = ~1, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~1, data = grd_count_df, K = 150,
                 dynamics = "trend",
                 control = list(trace=TRUE))
m1_sum <- summary(m1)
m2_inits <- c(coef(m1)[1],0,coef(m1)[2],coef(m1)[3])

m2 <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~1, data = grd_count_df, K = 150,
                 dynamics = "trend",
                 starts = m2_inits,
                 control = list(trace=TRUE))
summary(m2)
coef(m2)
m3_inits <- c(coef(m2)[1],coef(m2)[2],coef(m2)[3],coef(m2)[4],0)

m3 <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~total_fishingactivity, data = grd_count_df, K = 150,
                 dynamics = "trend",
                 starts = m3_inits,
                 control = list(trace=TRUE))
summary(m3)

m4 <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~total_fishingboats, data = grd_count_df, K = 150,
                 dynamics = "trend",
                 starts = m3_inits,
                 control = list(trace=TRUE))
summary(m4)
m5_inits <- c(coef(m2)[1:3],0,coef(m2)[4])

m5 <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~total_fishingactivity, 
                 omega = ~1,
                 p = ~1, data = grd_count_df, K = 150,
                 dynamics = "trend",
                 starts = m5_inits,
                 control = list(trace=TRUE))
summary(m5)

m6 <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~total_fishingboats, 
                 omega = ~1,
                 p = ~1, data = grd_count_df, K = 150,
                 dynamics = "trend",
                 starts = m5_inits,
                 control = list(trace=TRUE))
summary(m6)

m7 <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~season, 
                 omega = ~1,
                 p = ~1, data = grd_count_df, K = 150,
                 dynamics = "trend",
                 starts = m5_inits,
                 control = list(trace=TRUE))
summary(m7)

##Using dynamics="notrend"
m1_notrend <- pcountOpen(lam = ~1, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~1, data = grd_count_df, K = 150,
                 dynamics = "notrend",
                 starts = c(3,0,-1), ##from trend model
                 control = list(trace=TRUE))
summary(m1_notrend)
m2_inits_notrend <- c(coef(m1_notrend)[1],0.01,coef(m1_notrend)[2],coef(m1_notrend)[3])

m2_notrend <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~1, data = grd_count_df, K = 150,
                 dynamics = "notrend",
                 starts = m2_inits_notrend,
                 control = list(trace=TRUE))
summary(m2_notrend)
m3_inits_notrend <- c(coef(m2_notrend)[1:4],0)

m3_notrend <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~total_fishingactivity, data = grd_count_df, K = 150,
                 dynamics = "notrend",
                 starts = m3_inits_notrend,
                 control = list(trace=TRUE))
summary(m3_notrend)

m4_notrend <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~total_fishingboats, data = grd_count_df, K = 150,
                 dynamics = "notrend",
                 starts = m3_inits_notrend,
                 control = list(trace=TRUE))
summary(m4_notrend)
m5_inits_notrend <- c(coef(m2_notrend)[1:3],0,coef(m2_notrend)[4])

##Didnt complete running models with covs on omega (on 12.09.22)

##Constructing AIC list for both sets of dynamics
grd_count5km_models <- fitList('trend-lam(.)gam(.)p(.)' = m1, 
                            'trend-lam(dist_km)gam(.)p(.)' = m2, 
                            'trend-lam(dist_km)gam(.)p(fishingact)' = m3, 
                            'trend-lam(dist_km)gam(.)p(fishingboat)' = m4, 
                            'trend-lam(dist_km)gam(fishingact)p(.)' = m5, 
                            'trend-lam(dist_km)gam(fishingboat)p(.)' = m6,
                            'trend-lam(dist_km)gam(season)p(.)' = m7, 
                            'notrend-lam(.)omega(.)p(.)' = m1_notrend, 
                            'notrend-lam(dist_km)omega(.)p(.)' = m2_notrend, 
                            'notrend-lam(dist_km)omega(.)p(fishingact)' = m3_notrend) #excluding m4_notrend due to Nans

grd_count5km_AIC <- modSel(grd_count5km_models)
grd_count5km_AICtable <- grd_count5km_AIC@Full[c("model","negLogLike","nPars","n","AIC","delta","AICwt","cumltvWt")]
write.table(grd_count5km_AICtable, file = "grd_count5km_AICtable.txt", sep = "\t",
            row.names = F)

##Custom function to extract estimates using modelsummary
##for class of models: 'unmarkedFitPCO'
tidy.unmarkedFitPCO <- function(m, ...) {
  m_sum <- summary(m)
  result <- dplyr::bind_rows(m_sum) 
  ret <- data.frame(
    term      = row.names(result),
    estimate  = result[,1],
    std.error = result[,2],
    z         = result[,3],
    p         = result[,4])
  ret
}

glance.unmarkedFitPCO <- function(m, ...) {
  ret <- data.frame(
    AIC = m@AIC,
    negLogLike   = m@negLogLike)
  ret
}

##Exporting coefficients of best (<2 delta AIC) and null models
grd_count5km_list <- list(
  "Best"     = m4,
  "Second best" = m2_notrend,
  "Third best" = m2,
  "Null-trend" = m1,
  "Null-notrend" = m1_notrend
)

modelsummary(grd_count5km_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "grd_count5km_coeftable.docx")

modelsummary(m4)
modelsummary(m2_notrend)
modelsummary(m2)
modelsummary(m1)
modelsummary(m1_notrend)

############----------------------------------------------------------
##Occupancy of grds at 1km scale
##Adding covariate at 1km scale to account for spatial autocorrelation
spatial_autocov_grd2010 <- vector("integer",nrow(grd))
for(i in 1:nrow(grd)){
  if(i==1){
    spatial_autocov_grd2010[i] <- ifelse(grd$grd2010_occ[i+1]==0,0,1)
  } else if(i==nrow(grd)){
    spatial_autocov_grd2010[i] <- ifelse(grd$grd2010_occ[i-1]==0,0,1)
  } else
    spatial_autocov_grd2010[i] <- ifelse(grd$grd2010_occ[i-1]==0 & grd$grd2010_occ[i+1]==0,0,1)
  print(spatial_autocov_grd2010)
}
spatial_autocov_grd2010

spatial_autocov_grd2013 <- vector("integer",nrow(grd))
for(i in 1:nrow(grd)){
  if(i==1){
    spatial_autocov_grd2013[i] <- ifelse(grd$grd2013_occ[i+1]==0,0,1)
  } else if(i==nrow(grd)){
    spatial_autocov_grd2013[i] <- ifelse(grd$grd2013_occ[i-1]==0,0,1)
  } else
    spatial_autocov_grd2013[i] <- ifelse(grd$grd2013_occ[i-1]==0 & grd$grd2013_occ[i+1]==0,0,1)
  print(spatial_autocov_grd2013)
}
spatial_autocov_grd2013

spatial_autocov_grd2017 <- vector("integer",nrow(grd))
for(i in 1:nrow(grd)){
  if(i==1){
    spatial_autocov_grd2017[i] <- ifelse(grd$grd2017_occ[i+1]==0,0,1)
  } else if(i==nrow(grd)){
    spatial_autocov_grd2017[i] <- ifelse(grd$grd2017_occ[i-1]==0,0,1)
  } else
    spatial_autocov_grd2017[i] <- ifelse(grd$grd2017_occ[i-1]==0 & grd$grd2017_occ[i+1]==0,0,1)
  print(spatial_autocov_grd2017)
}
spatial_autocov_grd2017

grd <- cbind(grd,spatial_autocov_grd2010,spatial_autocov_grd2013,spatial_autocov_grd2017)
grd$dist_km_log <- log(grd$dist_km)
grd_svocc_merged <- merge(grd,covariate,by=c("reach_ID","replicate_segt","dist_km"))
str(grd_svocc_merged)

##Building single-visit models using the 'detect' package
#For 2010
svocc_grd2010_m1 <- svocc(grd2010_occ ~ dist_km_log | 1, grd_svocc_merged)
summary(svocc_grd2010_m1)
coef(svocc_grd2010_m1, "sta")
confint(svocc_grd2010_m1, model="sta")
coef(svocc_grd2010_m1, "det")
confint(svocc_grd2010_m1, model="det")

svocc_grd2010_m1.1 <- svocc(grd2010_occ ~ dist_km_log | spatial_autocov_grd2010, grd_svocc_merged)
summary(svocc_grd2010_m1.1)
coef(svocc_grd2010_m1.1, "sta")
confint(svocc_grd2010_m1.1, model="sta")
coef(svocc_grd2010_m1.1, "det")
confint(svocc_grd2010_m1.1, model="det")

svocc_grd2010_m2 <- svocc(grd2010_occ ~ dist_km_log | fishingactivity_2010, grd_svocc_merged)
summary(svocc_grd2010_m2)
coef(svocc_grd2010_m2, "sta")
confint(svocc_grd2010_m2,model="sta")
coef(svocc_grd2010_m2, "det")
confint(svocc_grd2010_m2,model="det")

svocc_grd2010_m3 <- svocc(grd2010_occ ~ dist_km_log | fishingboats_2010, grd_svocc_merged)
summary(svocc_grd2010_m3)
coef(svocc_grd2010_m3, "sta")
confint(svocc_grd2010_m3,model="sta")
coef(svocc_grd2010_m3, "det")
confint(svocc_grd2010_m3,model="det")

svocc_grd2010_m4 <- svocc(grd2010_occ ~ dist_km_log + spatial_autocov_grd2010| 1, grd_svocc_merged)
summary(svocc_grd2010_m4)
coef(svocc_grd2010_m4, "sta")
confint(svocc_grd2010_m4,model="sta")
coef(svocc_grd2010_m4, "det")
confint(svocc_grd2010_m4, model="det")

svocc_grd2010_m5 <- svocc(grd2010_occ ~ dist_km_log + fishingactivity_2010| 1, grd_svocc_merged)
summary(svocc_grd2010_m5)
coef(svocc_grd2010_m5, "sta")
confint(svocc_grd2010_m5,model="sta")
coef(svocc_grd2010_m5, "det")
confint(svocc_grd2010_m5,model="det")

svocc_grd2010_m6 <- svocc(grd2010_occ ~ dist_km_log + fishingboats_2010| 1, grd_svocc_merged)
summary(svocc_grd2010_m6)
coef(svocc_grd2010_m6, "sta")
confint(svocc_grd2010_m6,model="sta")
coef(svocc_grd2010_m6, "det")
confint(svocc_grd2010_m6,model="det")

svocc_grd2010_m7 <- svocc(grd2010_occ ~ dist_km_log + fishingactivity_2010 + fishingboats_2010| 1, grd_svocc_merged)
summary(svocc_grd2010_m7)
coef(svocc_grd2010_m7, "sta")
confint(svocc_grd2010_m7,model="sta")
coef(svocc_grd2010_m7, "det")
confint(svocc_grd2010_m7,model="det")

svocc_grd2010_m8 <- svocc(grd2010_occ ~ dist_km_log + fishingactivity_2010 + fishingboats_2010 + spatial_autocov_grd2010| fishingactivity_2010, grd_svocc_merged)
summary(svocc_grd2010_m8)
coef(svocc_grd2010_m8, "sta")
confint(svocc_grd2010_m8,model="sta")
coef(svocc_grd2010_m8, "det")
confint(svocc_grd2010_m8,model="det")

svocc.step(svocc_grd2010_m8, model="sta")
svocc.step(svocc_grd2010_m8, model="det")

svocc_grd2010_aic <- AIC(svocc_grd2010_m1,svocc_grd2010_m1.1,svocc_grd2010_m2,
    svocc_grd2010_m3,svocc_grd2010_m4,svocc_grd2010_m5,
    svocc_grd2010_m6,svocc_grd2010_m7,svocc_grd2010_m8)

##Custom function to extract estimates using modelsummary
##for class of models: 'svocc'
tidy.svocc <- function(m, ...) {
  m_sum <- summary(m)
  sta <- data.frame(m_sum[["sta"]])
  det <- data.frame(m_sum[["det"]])
  result <- dplyr::bind_rows(sta,det)
  ret <- data.frame(
    term      = row.names(result),
    estimate  = result[,1],
    std.error = result[,2],
    z         = result[,3],
    p         = result[,4])
  ret
}

tidy.svocc(svocc_grd2010_m1.1)

glance.svocc <- function(m, ...) {
  ret <- data.frame(
    AIC = AIC(m),
    negLogLike   = m[["loglik"]])
  ret
}

##Exporting coefficients of best (<2 delta AIC) and 2nd best model
grd_svocc2010_list <- list(
  "Best"     = svocc_grd2010_m1.1,
  "Second best" = svocc_grd2010_m5,
  "Null" = svocc_grd2010_m1
)

modelsummary(grd_svocc2010_list, shape = term + statistic ~ model,
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "grd_svocc2010_coeftable.docx")

modelsummary(svocc_grd2010_m1.1)
modelsummary(svocc_grd2010_m5)
modelsummary(svocc_grd2010_m1)

#For 2013
svocc_grd2013_full <- svocc(grd2013_occ ~ dist_km_log + fishingactivity_2014 + fishingboats_2014 + spatial_autocov_grd2013| fishingactivity_2014 + fishingboats_2014, grd_svocc_merged)
summary(svocc_grd2013_full)
coef(svocc_grd2013_full, "sta")
confint(svocc_grd2013_full,model="sta")
coef(svocc_grd2013_full, "det")
confint(svocc_grd2013_full,model="det")

svocc.step(svocc_grd2013_full, model="sta")
svocc.step(svocc_grd2013_full, model="det")

svocc_grd2013_m1 <- svocc(grd2013_occ ~ dist_km_log|1,grd_svocc_merged)
summary(svocc_grd2013_m1) #no significant effect of dist_km
coef(svocc_grd2013_m1, "sta")
confint(svocc_grd2013_m1, model="sta")
coef(svocc_grd2013_m1, "det")
confint(svocc_grd2013_m1, model="det")

svocc_grd2013_m2 <- svocc(grd2013_occ ~ dist_km_log | fishingactivity_2014, grd_svocc_merged)
summary(svocc_grd2013_m2) ##NA test stats
coef(svocc_grd2013_m2, "sta")
confint(svocc_grd2013_m2,model="sta")
coef(svocc_grd2013_m2, "det")
confint(svocc_grd2013_m2,model="det")

svocc_grd2013_m3 <- svocc(grd2013_occ ~ dist_km_log | fishingboats_2014, grd_svocc_merged)
summary(svocc_grd2013_m3) ##NA test stats
coef(svocc_grd2013_m3, "sta")
confint(svocc_grd2013_m3,model="sta")
coef(svocc_grd2013_m3, "det")
confint(svocc_grd2013_m3,model="det")

svocc_grd2013_m4 <- svocc(grd2013_occ ~ dist_km_log + spatial_autocov_grd2013| 1, grd_svocc_merged)
summary(svocc_grd2013_m4) ##SE > esimates
coef(svocc_grd2013_m4, "sta")
confint(svocc_grd2013_m4,model="sta")
coef(svocc_grd2013_m4, "det")
confint(svocc_grd2013_m4,model="det")

svocc_grd2013_m4a <- svocc(grd2013_occ ~ dist_km_log| spatial_autocov_grd2013, grd_svocc_merged)
summary(svocc_grd2013_m4a) ##SE > esimates
coef(svocc_grd2013_m4a, "sta")
confint(svocc_grd2013_m4a,model="sta")
coef(svocc_grd2013_m4a, "det")
confint(svocc_grd2013_m4a,model="det")

svocc_grd2013_m5 <- svocc(grd2013_occ ~ dist_km_log + fishingactivity_2014| 1, grd_svocc_merged)
summary(svocc_grd2013_m5) ##NA test stats
coef(svocc_grd2013_m5, "sta")
confint(svocc_grd2013_m5,model="sta")
coef(svocc_grd2013_m5, "det")
confint(svocc_grd2013_m5,model="det")

svocc_grd2013_m6 <- svocc(grd2013_occ ~ dist_km_log + fishingboats_2014| 1, grd_svocc_merged)
summary(svocc_grd2013_m6)
coef(svocc_grd2013_m6, "sta")
confint(svocc_grd2013_m6,model="sta")
coef(svocc_grd2013_m6, "det")
confint(svocc_grd2013_m6,model="det")

svocc_grd2013_aic <- AIC(svocc_grd2013_m1,svocc_grd2013_m2,svocc_grd2013_m3,
                         svocc_grd2013_m4,svocc_grd2013_m4a,svocc_grd2013_m5,
                         svocc_grd2013_m6)

##Exporting coefficients of non-NA and null models
grd_svocc2013_list <- list(
  "Model 1" = svocc_grd2013_m6,
  "Model 2" = svocc_grd2013_m4,
  "Model 3" = svocc_grd2013_m4a,
  "Null" = svocc_grd2013_m1)

modelsummary(grd_svocc2013_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "grd_svocc2013_coeftable.docx")

modelsummary(svocc_grd2013_m6)
modelsummary(svocc_grd2013_m4)
modelsummary(svocc_grd2013_m4a)
modelsummary(svocc_grd2013_m1)

##For 2017
svocc_grd2017_m1 <- svocc(grd2017_occ ~ dist_km_log|1,grd_svocc_merged)
summary(svocc_grd2017_m1)
coef(svocc_grd2017_m1, "sta")
confint(svocc_grd2017_m1, model="sta")
coef(svocc_grd2017_m1, "det")
confint(svocc_grd2017_m1, model="det")

svocc_grd2017_m1.1 <- svocc(grd2017_occ ~ dist_km_log|spatial_autocov_grd2017,grd_svocc_merged)
summary(svocc_grd2017_m1.1)
coef(svocc_grd2017_m1.1, "sta")
confint(svocc_grd2017_m1.1, model="sta")
coef(svocc_grd2017_m1.1, "det")
confint(svocc_grd2017_m1.1, model="det")

svocc_grd2017_m2 <- svocc(grd2017_occ ~ dist_km_log|fishingactivity_2017, grd_svocc_merged)
summary(svocc_grd2017_m2)
coef(svocc_grd2017_m2, "sta")
confint(svocc_grd2017_m2, model="sta")
coef(svocc_grd2017_m2, "det")
confint(svocc_grd2017_m2, model="det")

svocc_grd2017_m3 <- svocc(grd2017_occ ~ dist_km_log|fishingboats_2017, grd_svocc_merged)
summary(svocc_grd2017_m3)
coef(svocc_grd2017_m3, "sta")
confint(svocc_grd2017_m3, model="sta")
coef(svocc_grd2017_m3, "det")
confint(svocc_grd2017_m3, model="det")

svocc_grd2017_m4 <- svocc(grd2017_occ ~ dist_km_log+spatial_autocov_grd2017|1, grd_svocc_merged)
summary(svocc_grd2017_m4)
coef(svocc_grd2017_m4, "sta")
confint(svocc_grd2017_m4, model="sta")
coef(svocc_grd2017_m4, "det")
confint(svocc_grd2017_m4, model="det")

svocc_grd2017_m5 <- svocc(grd2017_occ ~ dist_km_log+fishingactivity_2017|1, grd_svocc_merged)
summary(svocc_grd2017_m5)
coef(svocc_grd2017_m5, "sta")
confint(svocc_grd2017_m5, model="sta")
coef(svocc_grd2017_m5, "det")
confint(svocc_grd2017_m5, model="det")

svocc_grd2017_m6 <- svocc(grd2017_occ ~ dist_km_log+fishingboats_2017|1, grd_svocc_merged)
summary(svocc_grd2017_m6)
coef(svocc_grd2017_m6, "sta")
confint(svocc_grd2017_m6, model="sta")
coef(svocc_grd2017_m6, "det")
confint(svocc_grd2017_m6, model="det")

svocc_grd2017_m8 <- svocc(grd2017_occ ~ dist_km_log+spatial_autocov_grd2017|spatial_autocov_grd2017, grd_svocc_merged)
summary(svocc_grd2017_m8) ##lowest AIC but strange coefficients
coef(svocc_grd2017_m8, "sta")
confint(svocc_grd2017_m8, model="sta")
coef(svocc_grd2017_m8, "det")
confint(svocc_grd2017_m8, model="det")

svocc_grd2017_aic <- AIC(svocc_grd2017_m1,svocc_grd2017_m1.1,svocc_grd2017_m2,svocc_grd2017_m3,
                         svocc_grd2017_m4,svocc_grd2017_m5,svocc_grd2017_m6,svocc_grd2017_m8)
                         
##Exporting results - 2017 - m1.1 and m4
##Exporting coefficients of best models (<2 dekta AIC) and null model
grd_svocc2017_list <- list(
  "Best" = svocc_grd2017_m8,
  "Second best" = svocc_grd2017_m1.1,
  "Third best" = svocc_grd2017_m4,
  "Null" = svocc_grd2017_m1)

modelsummary(grd_svocc2017_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "grd_svocc2017_coeftable.docx")

modelsummary(svocc_grd2017_m8)
modelsummary(svocc_grd2017_m1.1)
modelsummary(svocc_grd2017_m4)
modelsummary(svocc_grd2017_m1)

############----------------------------------------------------------
##Abundance of grds at 1km scale
table(grd$grd2010_count) ##117 sites with zero count
table(grd$grd2013_count) ##120 sites with zero count
table(grd$grd2017_count) ##138 sites with zero count

#For 2010
plot(grd_svocc_merged$grd2010_count~grd_svocc_merged$dist_km_log)
plot(grd_svocc_merged$grd2010_count~grd_svocc_merged$fishingactivity_2010)
plot(grd_svocc_merged$grd2010_count~grd_svocc_merged$fishingboats_2010)

svabu_grd2010_m1 <- svabu(grd2010_count ~ dist_km_log | 1, grd_svocc_merged)
summary(svabu_grd2010_m1) ##NA test stats

svabu_grd2010_m2 <- svabu(grd2010_count ~ dist_km_log | fishingactivity_2010, grd_svocc_merged)
svabu_grd2010_m2_sum <- summary(svabu_grd2010_m2)

svabu_grd2010_m3 <- svabu(grd2010_count ~ dist_km_log | fishingboats_2010, grd_svocc_merged)
summary(svabu_grd2010_m3) ##NA test stats

svabu_grd2010_m4 <- svabu(grd2010_count ~ dist_km_log | spatial_autocov_grd2010, grd_svocc_merged)
summary(svabu_grd2010_m4) ##NA test stats

svabu_grd2010_m5 <- svabu(grd2010_count ~ dist_km_log + fishingactivity_2010 | 1, grd_svocc_merged)
summary(svabu_grd2010_m5) ##NA test stats

svabu_grd2010_m6 <- svabu(grd2010_count ~ dist_km_log + fishingboats_2010 | 1, grd_svocc_merged)
summary(svabu_grd2010_m6) ##NA test stats

svabu_grd2010_m7 <- svabu(grd2010_count ~ dist_km_log + zif(dist_km_log) | 1, grd_svocc_merged)
summary(svabu_grd2010_m7) ##NA test stats

svabu_grd2010_m8 <- svabu(grd2010_count ~ dist_km_log + zif(spatial_autocov_grd2010)| 1, grd_svocc_merged)
summary(svabu_grd2010_m8) ##NA test stats

svabu_grd2010_m9 <- svabu(grd2010_count ~ dist_km_log + zif(fishingactivity_2010)| 1, grd_svocc_merged)
summary(svabu_grd2010_m9) ##NA test stats

svabu_grd2010_m10 <- svabu(grd2010_count ~ dist_km_log + zif(fishingboats_2010)| 1, grd_svocc_merged)
summary(svabu_grd2010_m10) ##NA test stats

##Custom function to extract estimates using modelsummary
##for class of models: 'svabu'
tidy.svabu <- function(m, ...) {
  m_sum <- summary(m)
  sta <- data.frame(m_sum[["sta"]])
  det <- data.frame(m_sum[["det"]])
  zif <- data.frame(m_sum[["zif"]])
  result <- dplyr::bind_rows(sta,det,zif)
  ret <- data.frame(
    term      = row.names(result),
    estimate  = result[,1],
    std.error = result[,2],
    z         = result[,3],
    p         = result[,4])
  ret
}

glance.svabu <- function(m, ...) {
  ret <- data.frame(
    AIC = AIC(m),
    negLogLike   = m[["loglik"]])
  ret
}

#For 2013
plot(grd_svocc_merged$grd2013_count~grd_svocc_merged$dist_km_log)
plot(grd_svocc_merged$grd2013_count~grd_svocc_merged$fishingactivity_2014)
plot(grd_svocc_merged$grd2013_count~grd_svocc_merged$fishingboats_2014)

svabu_grd2013_m1 <- svabu(grd2013_count ~ dist_km_log | 1, grd_svocc_merged)
summary(svabu_grd2013_m1) ##NA test stats

svabu_grd2013_m2 <- svabu(grd2013_count ~ dist_km_log | fishingactivity_2014, grd_svocc_merged)
summary(svabu_grd2013_m2)

svabu_grd2013_m3 <- svabu(grd2013_count ~ dist_km_log | fishingboats_2014, grd_svocc_merged)
summary(svabu_grd2013_m3) ##NA test stats

svabu_grd2013_m4 <- svabu(grd2013_count ~ dist_km_log | spatial_autocov_grd2013, grd_svocc_merged)
summary(svabu_grd2013_m4) ##NA test stats

svabu_grd2013_m5 <- svabu(grd2013_count ~ dist_km_log + fishingactivity_2014 | 1, grd_svocc_merged)
summary(svabu_grd2013_m5) ##NA test stats

svabu_grd2013_m6 <- svabu(grd2013_count ~ dist_km_log + fishingboats_2014 | 1, grd_svocc_merged)
summary(svabu_grd2013_m6)

svabu_grd2013_m7 <- svabu(grd2013_count ~ dist_km_log + zif(dist_km_log) | 1, grd_svocc_merged)
summary(svabu_grd2013_m7) ##NA test stats

svabu_grd2013_m8 <- svabu(grd2013_count ~ dist_km_log + zif(spatial_autocov_grd2013)| 1, grd_svocc_merged)
summary(svabu_grd2013_m8) ##NA test stats

svabu_grd2013_m9 <- svabu(grd2013_count ~ dist_km_log + zif(fishingactivity_2014)| 1, grd_svocc_merged)
summary(svabu_grd2013_m9) ##NA test stats

svabu_grd2013_m10 <- svabu(grd2013_count ~ dist_km_log + zif(fishingboats_2014)| 1, grd_svocc_merged)
summary(svabu_grd2013_m10) ##NA test stats

#For 2017
plot(grd_svocc_merged$grd2017_count~grd_svocc_merged$dist_km_log)
plot(grd_svocc_merged$grd2017_count~grd_svocc_merged$fishingactivity_2017)
plot(grd_svocc_merged$grd2017_count~grd_svocc_merged$fishingboats_2017)

svabu_grd2017_m1 <- svabu(grd2017_count ~ dist_km_log | 1, grd_svocc_merged)
summary(svabu_grd2017_m1) ##NA test stats

svabu_grd2017_m2 <- svabu(grd2017_count ~ dist_km_log | fishingactivity_2017, grd_svocc_merged)
summary(svabu_grd2017_m2) ##NA test stats

svabu_grd2017_m3 <- svabu(grd2017_count ~ dist_km_log | fishingboats_2017, grd_svocc_merged)
summary(svabu_grd2017_m3)

svabu_grd2017_m4 <- svabu(grd2017_count ~ dist_km_log | spatial_autocov_grd2017, grd_svocc_merged)
summary(svabu_grd2017_m4) ##NA test stats

svabu_grd2017_m5 <- svabu(grd2017_count ~ dist_km_log + fishingactivity_2017 | 1, grd_svocc_merged)
summary(svabu_grd2017_m5) ##NA test stats

svabu_grd2017_m6 <- svabu(grd2017_count ~ dist_km_log + fishingboats_2017 | 1, grd_svocc_merged)
summary(svabu_grd2017_m6) 

svabu_grd2017_m7 <- svabu(grd2017_count ~ dist_km_log + zif(dist_km_log) | 1, grd_svocc_merged)
summary(svabu_grd2017_m7) ##NA test stats

svabu_grd2017_m8 <- svabu(grd2017_count ~ dist_km_log + zif(spatial_autocov_grd2017)| 1, grd_svocc_merged)
summary(svabu_grd2017_m8) ##NA test stats

svabu_grd2017_m9 <- svabu(grd2017_count ~ dist_km_log + zif(fishingactivity_2017)| 1, grd_svocc_merged)
summary(svabu_grd2017_m9) ##NA test stats

svabu_grd2017_m10 <- svabu(grd2017_count ~ dist_km_log + zif(fishingboats_2017)| 1, grd_svocc_merged)
summary(svabu_grd2017_m10) ##NA test stats

##Exporting coefficients of all models with non-NA test stats for 3 years
grd_svabu_list <- list(
  "Model (2010)"     = svabu_grd2010_m2,
  "Model 1 (2013)" = svabu_grd2013_m2,
  "Model 2 (2013)" = svabu_grd2013_m6,
  "Model 1 (2017)" = svabu_grd2017_m3,
  "Model 2 (2017)" = svabu_grd2017_m6
)

modelsummary(grd_svabu_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "grd_svabu_coeftable.docx")

modelsummary(svabu_grd2010_m2)
modelsummary(svabu_grd2013_m2)
modelsummary(svabu_grd2013_m6)
modelsummary(svabu_grd2017_m3)
modelsummary(svabu_grd2017_m6)

############----------------------------------------------------------
##Occupancy of gharials at 5km scale
##Exploratory analysis
grl_cov_merged <- merge(gharial,covariate,by=c("reach_ID","replicate_segt"))
##distance along river
dist_occ_grl1 <- ggplot(data=grl_cov_merged,aes(x=dist_km.x,y=gharial_2014occ))+
  geom_point()+theme_bw()
dist_occ_grl2 <- ggplot(data=grl_cov_merged,aes(x=dist_km.x,y=gharial_2017occ))+
  geom_point()+theme_bw()

##fishing activity
fishingactivity_occ_grl1 <- grl_cov_merged%>%filter(gharial_2014occ==1)%>%
  ggplot(aes(x=fishingactivity_2014))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2014)")+theme_bw()

fishingactivity_occ_grl2 <- grl_cov_merged%>%filter(gharial_2017occ==1)%>%
  ggplot(aes(x=fishingactivity_2017))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2017)")+theme_bw()

##fishing boats
fishingboats_occ_grl1 <- grl_cov_merged%>%filter(gharial_2014occ==1)%>%
  ggplot(aes(x=fishingboats_2014))+
  geom_histogram(binwidth = 1,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ_grl2 <- grl_cov_merged%>%filter(gharial_2017occ==1)%>%
  ggplot(aes(x=fishingboats_2017))+
  geom_histogram(binwidth = 1,color="red")+
  labs(y="No. of detections",x="Fishing boats (2017)")+
  theme_bw()

##formatting variables to unmarked format---------
##formatting occu data
grl_occ1 <- gharial%>%select(reach_ID,replicate_segt,gharial_2014occ)%>%
            pivot_wider(names_from = replicate_segt,values_from = gharial_2014occ)
grl_occ2 <- gharial%>%select(reach_ID,replicate_segt,gharial_2017occ)%>%
            pivot_wider(names_from = replicate_segt,values_from = gharial_2017occ)
grl_occ_list <- list(grl_occ1,grl_occ2)
grl_occ <- grl_occ_list%>%reduce(full_join,by="reach_ID")
grl_occ_mat <- data.matrix(grl_occ[,2:11])

cov_fishingactivity_mat_grl <- cov_fishingactivity_mat[,6:15]
cov_fishingboats_mat_grl <- cov_fishingboats_mat[,6:15]

cov_fishing_grl <- list(fishingact_grl=cov_fishingactivity_mat_grl,fishingboat_grl=cov_fishingboats_mat_grl)
season_grl <- rep(c("Apr","Nov"),times=42)

##Bundling into unmarked multi-season frame
grl_occ_df <- unmarkedMultFrame(y=grl_occ_mat,siteCovs = covariate_occ5km[,2],
                                yearlySiteCovs=list(total_fishingactivity=covariate_occ5km[,4:5],
                                                    total_fishingboats=covariate_occ5km[,7:8],
                                                    season=matrix(season_grl,nrow=42,byrow = T)),
                                obsCovs=cov_fishing_grl,
                                numPrimary=2) 
summary(grl_occ_df) 

##Models
grl_occ_model1 <- colext(~1, ~1, ~1, ~1, grl_occ_df)
summary(grl_occ_model1)
backTransform(grl_occ_model1,type="psi")
backTransform(grl_occ_model1,type="col")
backTransform(grl_occ_model1,type="ext")
backTransform(grl_occ_model1,type="det")

grl_occ_model2 <- colext(~median_dist_km, ~1, ~1, ~1, grl_occ_df)
summary(grl_occ_model2) ##SE>>estimate

grl_occ_model3 <- colext(~1, ~total_fishingactivity, ~total_fishingactivity, ~1, grl_occ_df)
summary(grl_occ_model3)

grl_occ_model3a <- colext(~1, ~total_fishingactivity, ~1, ~1, grl_occ_df)
summary(grl_occ_model3a)

grl_occ_model3b <- colext(~1, ~1, ~total_fishingactivity, ~1, grl_occ_df)
summary(grl_occ_model3b) #NA test stats

grl_occ_model4 <- colext(~1, ~total_fishingboats, ~total_fishingboats, ~1, grl_occ_df)
summary(grl_occ_model4) 

grl_occ_model4a <- colext(~1, ~total_fishingboats, ~1, ~1, grl_occ_df)
summary(grl_occ_model4a) 

grl_occ_model4b <- colext(~1, ~1, ~total_fishingboats, ~1, grl_occ_df)
summary(grl_occ_model4b) 

grl_occ_model5 <- colext(~1, ~1, ~1, ~fishingact_grl, grl_occ_df)
summary(grl_occ_model5)

grl_occ_model6 <- colext(~1, ~1, ~1, ~fishingboat_grl, grl_occ_df)
summary(grl_occ_model6) 

grl_occ5km_models <- fitList('psi(.)gam(.)eps(.)p(.)' = grl_occ_model1,
                          'psi(dist_km)gam(.)eps(.)p(.)' = grl_occ_model2,
                          'psi(.)gam(fishingact)eps(fishingact)p(.)' = grl_occ_model3,
                          'psi(.)gam(fishingact)eps(.)p(.)' = grl_occ_model3a,
                          'psi(.)gam(fishingboat)eps(fishingboat)p(.)' = grl_occ_model4,
                          'psi(.)gam(fishingboat)eps(.)p(.)' = grl_occ_model4a,
                          'psi(.)gam(.)eps(fishingboat)p(.)' = grl_occ_model4b,
                          'psi(.)gam(.)eps(.)p(fishingact)' = grl_occ_model5,
                          'psi(.)gam(.)eps(.)p(fishingboat)' = grl_occ_model6)


grl_occ5km_models_AIC <- modSel(grl_occ5km_models)
grl_occ5km_models_AICtable <- grl_occ5km_models_AIC@Full[c("model","negLogLike","nPars","n","AIC","delta","AICwt","cumltvWt")]
write.table(grl_occ_models_AICtable, file = "gharial_occ5km_AICtable.txt", sep = "\t",
            row.names = F)

##Exporting coefficients of best (<2 delta AIC) and null models
grl_occ5km_list <- list(
  "Best"     = grl_occ_model3a,
  "Second best" = grl_occ_model4a,
  "Third best" = grl_occ_model4,
  "Fourth best" = grl_occ_model3,
  "Null" = grl_occ_model1
)

modelsummary(grl_occ5km_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "gharial_occ5km_coeftable.docx")

modelsummary(grl_occ_model3a)
modelsummary(grl_occ_model4a)
modelsummary(grl_occ_model4)
modelsummary(grl_occ_model3)
modelsummary(grl_occ_model1)

############----------------------------------------------------------
##Abundance of gharials at 5km scale
gharial_count5km <- gharial%>%group_by(reach_ID)%>%
  summarise(
    total_count_2014=sum(gharial_count_2014),
    total_count_2017=sum(gharial_count_2017))

gharial_count_mat <- data.matrix(gharial_count5km[,2:3])  

gharial_count_df <- unmarkedFramePCO(y=gharial_count_mat,siteCovs = covariate_occ5km[,2],
                                 yearlySiteCovs=list(total_fishingactivity=covariate_occ5km[,4:5],
                                                     total_fishingboats=covariate_occ5km[,7:8]),
                                 obsCovs=list(total_fishingactivity=covariate_occ5km[,4:5],
                                              total_fishingboats=covariate_occ5km[,7:8]),
                                 numPrimary=2) 
summary(gharial_count_df) 

##Using dynamics="trend"
m1_grl <- pcountOpen(lam = ~1, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~1, data = gharial_count_df, K = 110, #K set to 100+max(y)
                 dynamics = "trend",
                 control = list(trace=TRUE))
summary(m1_grl)
m2_inits_grl <- c(coef(m1_grl)[1],0,coef(m1_grl)[2],coef(m1_grl)[3])

m2_grl <- pcountOpen(lam = ~median_dist_km, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~1, data = gharial_count_df, K = 110,
                 dynamics = "trend",
                 starts = m2_inits_grl,
                 control = list(trace=TRUE))
summary(m2_grl)
coef(m2_grl)
m3_inits_grl <- c(coef(m2_grl)[1],coef(m2_grl)[3],coef(m2_grl)[4],0)

m3_grl <- pcountOpen(lam = ~1, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~total_fishingactivity, data = gharial_count_df, K = 110,
                 dynamics = "trend",
                 starts = m3_inits_grl,
                 control = list(trace=TRUE))
summary(m3_grl)

m4_grl <- pcountOpen(lam = ~1, 
                 gam = ~1, 
                 omega = ~1,
                 p = ~total_fishingboats, data = gharial_count_df, K = 110,
                 dynamics = "trend",
                 starts = m3_inits_grl,
                 control = list(trace=TRUE))
summary(m4_grl)
m5_inits_grl <- c(coef(m3_grl)[1:2],0,coef(m3_grl)[3:4])

m5_grl <- pcountOpen(lam = ~1, 
                     gam = ~total_fishingactivity, 
                     omega = ~1,
                     p = ~total_fishingactivity, data = gharial_count_df, K = 110,
                     dynamics = "trend",
                     starts = m5_inits_grl,
                     control = list(trace=TRUE))
summary(m5_grl)

m6_grl <- pcountOpen(lam = ~1, 
                     gam = ~total_fishingboats, 
                     omega = ~1,
                     p = ~total_fishingactivity, data = gharial_count_df, K = 110,
                     dynamics = "trend",
                     starts = m5_inits_grl,
                     control = list(trace=TRUE))
summary(m6_grl)

##Using dynamics="notrend"
m1_notrend_grl <-pcountOpen(lam = ~1, 
                            gam = ~1, 
                            omega = ~1,
                            p = ~1, data = gharial_count_df, K = 110,
                            dynamics = "notrend",
                            starts = c(0.5,0.2,0.1), ##from trend model
                            control = list(trace=TRUE))
summary(m1_notrend_grl)
m2_notrend_inits_grl <- c(coef(m1_notrend_grl)[1],0,
                          coef(m1_notrend_grl)[2],
                          coef(m1_notrend_grl)[3])

m2_notrend_grl <-pcountOpen(lam = ~median_dist_km, 
                            gam = ~1, 
                            omega = ~1,
                            p = ~1, data = gharial_count_df, K = 110,
                            dynamics = "notrend",
                            starts = m2_notrend_inits_grl,
                            control = list(trace=TRUE))
summary(m2_notrend_grl)
m3_notrend_inits_grl <- c(coef(m2_notrend_grl)[1:4],0)

m3_notrend_grl <-pcountOpen(lam = ~median_dist_km, 
                            gam = ~1, 
                            omega = ~1,
                            p = ~total_fishingactivity, data = gharial_count_df, K = 110,
                            dynamics = "notrend",
                            starts = m3_notrend_inits_grl,
                            control = list(trace=TRUE))
summary(m3_notrend_grl)

m4_notrend_grl <-pcountOpen(lam = ~median_dist_km, 
                            gam = ~1, 
                            omega = ~1,
                            p = ~total_fishingboats, data = gharial_count_df, K = 110,
                            dynamics = "notrend",
                            starts = m3_notrend_inits_grl,
                            control = list(trace=TRUE))
summary(m4_notrend_grl)

##Constructing AIC list for both sets of dynamics
gharial_count5km_models <- fitList('trend-lam(.)gam(.)p(.)' = m1_grl, 
                            'trend-lam(dist_km)gam(.)p(.)' = m2_grl, 
                            'trend-lam(.)gam(.)p(fishingact)' = m3_grl, 
                            'trend-lam(.)gam(.)p(fishingboat)' = m4_grl, 
                            'trend-lam(.)gam(fishingact)p(fishingact)' = m5_grl, 
                            'trend-lam(.)gam(fishingboat)p(fishingact)' = m6_grl,
                            'notrend-lam(.)omega(.)p(.)' = m1_notrend_grl, 
                            'notrend-lam(dist_km)omega(.)p(.)' = m2_notrend_grl, 
                            'notrend-lam(dist_km)omega(.)p(fishingact)' = m3_notrend_grl,
                            'notrend-lam(dist_km)omega(.)p(fishingboat)' = m4_notrend_grl) 

gharial_count5km_models_AIC <- modSel(gharial_count5km_models)
gharial_count5km_models_AICtable <- gharial_count5km_models_AIC@Full[c("model","negLogLike","nPars","n","AIC","delta","AICwt","cumltvWt")]
write.table(gharial_count5km_models_AICtable, file = "gharial_count5km_AICtable.txt", sep = "\t",
            row.names = F)

##Exporting coefficients of best (~2 delta AIC)i.e
gharial_count5km_list <- list(
  "Best"     = m3_grl,
  "Second best/Null" = m1_grl,
  "Third best" = m2_grl,
  "Fourth best" = m5_grl,
  "Fifth best" = m6_grl,
  "Sixth best" = m4_grl)

modelsummary(gharial_count5km_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "gharial_count5km_coeftable.docx")

modelsummary(m3_grl)
modelsummary(m1_grl)
modelsummary(m2_grl)
modelsummary(m5_grl)
modelsummary(m6_grl)
modelsummary(m4_grl)

############----------------------------------------------------------
##Occupancy of gharials at 1km scale
spatial_autocov_gharial2014 <- vector("integer",nrow(gharial))
for(i in 1:nrow(gharial)){
  if(i==1){
    spatial_autocov_gharial2014[i] <- ifelse(gharial$gharial_2014occ[i+1]==0,0,1)
  } else if(i==nrow(gharial)){
    spatial_autocov_gharial2014[i] <- ifelse(gharial$gharial_2014occ[i-1]==0,0,1)
  } else
    spatial_autocov_gharial2014[i] <- ifelse(gharial$gharial_2014occ[i-1]==0 & 
                                             gharial$gharial_2014occ[i+1]==0,0,1)
}
spatial_autocov_gharial2014
spatial_autocov_gharial2017 <- vector("integer",nrow(gharial))
for(i in 1:nrow(gharial)){
  if(i==1){
    spatial_autocov_gharial2017[i] <- ifelse(gharial$gharial_2017occ[i+1]==0,0,1)
  } else if(i==nrow(gharial)){
    spatial_autocov_gharial2017[i] <- ifelse(gharial$gharial_2017occ[i-1]==0,0,1)
  } else
    spatial_autocov_gharial2017[i] <- ifelse(gharial$gharial_2017occ[i-1]==0 & 
                                           gharial$gharial_2017occ[i+1]==0,0,1)
}
spatial_autocov_gharial2017
gharial<-cbind(gharial,spatial_autocov_gharial2014,spatial_autocov_gharial2017)

##using log transformation
##instead of 'scale()' transformation or random number addition
gharial$dist_km_log <- log(gharial$dist_km)
gharial_svocc_merged <- merge(gharial,covariate,by=c("reach_ID","replicate_segt","dist_km"))%>%
                        arrange(reach_ID,replicate_segt)
str(gharial_svocc_merged)

#For 2014
svocc_gharial2014_m1 <- svocc(gharial_2014occ ~ dist_km_log | 1, gharial_svocc_merged)
summary(svocc_gharial2014_m1)
coef(svocc_gharial2014_m1, "sta")
confint(svocc_gharial2014_m1, model="sta")
coef(svocc_gharial2014_m1, "det")
confint(svocc_gharial2014_m1, model="det")

svocc_gharial2014_m2 <- svocc(gharial_2014occ ~ dist_km_log | spatial_autocov_gharial2014, gharial_svocc_merged)
summary(svocc_gharial2014_m2)
coef(svocc_gharial2014_m2, "sta")
confint(svocc_gharial2014_m2, model="sta")
coef(svocc_gharial2014_m2, "det")
confint(svocc_gharial2014_m2, model="det")

svocc_gharial2014_m3 <- svocc(gharial_2014occ ~ dist_km_log| fishingactivity_2014, gharial_svocc_merged)
summary(svocc_gharial2014_m3) #NA test stats
coef(svocc_gharial2014_m3, "sta")
confint(svocc_gharial2014_m3, model="sta")
coef(svocc_gharial2014_m3, "det")
confint(svocc_gharial2014_m3, model="det")

svocc_gharial2014_m4 <- svocc(gharial_2014occ ~ dist_km_log| fishingboats_2014, gharial_svocc_merged)
summary(svocc_gharial2014_m4) #NA test stats
coef(svocc_gharial2014_m4, "sta")
confint(svocc_gharial2014_m4, model="sta")
coef(svocc_gharial2014_m4, "det")
confint(svocc_gharial2014_m4, model="det")

svocc_gharial2014_m5 <- svocc(gharial_2014occ ~ dist_km_log+spatial_autocov_gharial2014| spatial_autocov_gharial2014, gharial_svocc_merged)
summary(svocc_gharial2014_m5) #NA test stats
coef(svocc_gharial2014_m5, "sta")
confint(svocc_gharial2014_m5, model="sta")
coef(svocc_gharial2014_m5, "det")
confint(svocc_gharial2014_m5, model="det")

svocc_gharial2014_m6 <- svocc(gharial_2014occ ~ dist_km_log+fishingactivity_2014| 1, gharial_svocc_merged)
summary(svocc_gharial2014_m6) #NA test stats
coef(svocc_gharial2014_m6, "sta")
confint(svocc_gharial2014_m6, model="sta")
coef(svocc_gharial2014_m6, "det")
confint(svocc_gharial2014_m6, model="det")

svocc_gharial2014_m7 <- svocc(gharial_2014occ ~ dist_km_log+fishingboats_2014| 1, gharial_svocc_merged)
summary(svocc_gharial2014_m7) #NA test stats
coef(svocc_gharial2014_m7, "sta")
confint(svocc_gharial2014_m7, model="sta")
coef(svocc_gharial2014_m7, "det")
confint(svocc_gharial2014_m7, model="det")

#For 2017
svocc_gharial2017_m1 <- svocc(gharial_2017occ ~ dist_km_log | 1, gharial_svocc_merged)
summary(svocc_gharial2017_m1)
coef(svocc_gharial2014_m1, "sta")
confint(svocc_gharial2014_m1, model="sta")
coef(svocc_gharial2014_m1, "det")
confint(svocc_gharial2014_m1, model="det")

svocc_gharial2017_m2 <- svocc(gharial_2017occ ~ dist_km_log | spatial_autocov_gharial2017, gharial_svocc_merged)
summary(svocc_gharial2017_m2)
coef(svocc_gharial2017_m2, "sta")
confint(svocc_gharial2017_m2, model="sta")
coef(svocc_gharial2017_m2, "det")
confint(svocc_gharial2017_m2, model="det")

svocc_gharial2017_m3 <- svocc(gharial_2017occ ~ dist_km_log | fishingactivity_2017, gharial_svocc_merged)
summary(svocc_gharial2017_m3)
coef(svocc_gharial2017_m3, "sta")
confint(svocc_gharial2017_m3, model="sta")
coef(svocc_gharial2017_m3, "det")
confint(svocc_gharial2017_m3, model="det")

svocc_gharial2017_m4 <- svocc(gharial_2017occ ~ dist_km_log | fishingboats_2017, gharial_svocc_merged)
summary(svocc_gharial2017_m4)
coef(svocc_gharial2017_m4, "sta")
confint(svocc_gharial2017_m4, model="sta")
coef(svocc_gharial2017_m4, "det")
confint(svocc_gharial2017_m4, model="det")

svocc_gharial2017_m5 <- svocc(gharial_2017occ ~ dist_km_log + spatial_autocov_gharial2017| 1, gharial_svocc_merged)
summary(svocc_gharial2017_m5) ##NA test stats
coef(svocc_gharial2017_m5, "sta")
confint(svocc_gharial2017_m5, model="sta")
coef(svocc_gharial2017_m5, "det")
confint(svocc_gharial2017_m5, model="det")

svocc_gharial2017_m6 <- svocc(gharial_2017occ ~ dist_km_log + fishingactivity_2017| 1, gharial_svocc_merged)
summary(svocc_gharial2017_m6) ##NA test stats
coef(svocc_gharial2017_m6, "sta")
confint(svocc_gharial2017_m6, model="sta")
coef(svocc_gharial2017_m6, "det")
confint(svocc_gharial2017_m6, model="det")

svocc_gharial2017_m7 <- svocc(gharial_2017occ ~ dist_km_log + fishingboats_2017| 1, gharial_svocc_merged)
summary(svocc_gharial2017_m7) ##NA test stats
coef(svocc_gharial2017_m7, "sta")
confint(svocc_gharial2017_m7, model="sta")
coef(svocc_gharial2017_m7, "det")
confint(svocc_gharial2017_m7, model="det")

svocc_gharial2017_m8 <- svocc(gharial_2017occ ~ dist_km_log + spatial_autocov_gharial2017| spatial_autocov_gharial2017, gharial_svocc_merged)
summary(svocc_gharial2017_m8) ##NA test stats
coef(svocc_gharial2017_m8, "sta")
confint(svocc_gharial2017_m8, model="sta")
coef(svocc_gharial2017_m8, "det")
confint(svocc_gharial2017_m8, model="det")

##Exporting coefficients of models with non-NA test stats
gharial_svocc_list <- list(
  "Model 1 (2014)" = svocc_gharial2014_m1,
  "Model 2 (2014)" = svocc_gharial2014_m2,
  "Model 1 (2017)" = svocc_gharial2017_m1,
  "Model 2 (2017)" = svocc_gharial2017_m2,
  "Model 3 (2017)" = svocc_gharial2017_m3,
  "Model 4 (2017)" = svocc_gharial2017_m4)

modelsummary(gharial_svocc_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "gharial_svocc_coeftable.docx")

modelsummary(svocc_gharial2014_m1)
modelsummary(svocc_gharial2014_m2)
modelsummary(svocc_gharial2017_m1)
modelsummary(svocc_gharial2017_m2)
modelsummary(svocc_gharial2017_m3)
modelsummary(svocc_gharial2017_m4)

############----------------------------------------------------------
##Abundance of gharials at 1km scale
table(gharial$gharial_count_2014) ##184 sites with zero count
table(gharial$gharial_count_2017) ##179 sites with zero count

#For 2014
svabu_gharial2014_m1 <- svabu(gharial_count_2014 ~ dist_km_log | 1, gharial_svocc_merged)
summary(svabu_gharial2014_m1)

svabu_gharial2014_m2 <- svabu(gharial_count_2014 ~ dist_km_log | spatial_autocov_gharial2014, gharial_svocc_merged)
summary(svabu_gharial2014_m2) ##NA test stats

svabu_gharial2014_m3 <- svabu(gharial_count_2014 ~ dist_km_log | fishingactivity_2014, gharial_svocc_merged)
summary(svabu_gharial2014_m3)

svabu_gharial2014_m4 <- svabu(gharial_count_2014 ~ dist_km_log | fishingboats_2014, gharial_svocc_merged)
summary(svabu_gharial2014_m4)

svabu_gharial2014_m5 <- svabu(gharial_count_2014 ~ dist_km_log + fishingactivity_2014| 1, gharial_svocc_merged)
summary(svabu_gharial2014_m5) ##NA test stats

svabu_gharial2014_m6 <- svabu(gharial_count_2014 ~ dist_km_log + fishingboats_2014| 1, gharial_svocc_merged)
summary(svabu_gharial2014_m6)

svabu_gharial2014_m7 <- svabu(gharial_count_2014 ~ dist_km_log + zif(spatial_autocov_gharial2014)| 1, gharial_svocc_merged)
summary(svabu_gharial2014_m7)

svabu_gharial2014_m8 <- svabu(gharial_count_2014 ~ dist_km_log + zif(fishingactivity_2014)| 1, gharial_svocc_merged)
summary(svabu_gharial2014_m8)

svabu_gharial2014_m9 <- svabu(gharial_count_2014 ~ dist_km_log + zif(fishingboats_2014)| 1, gharial_svocc_merged)
summary(svabu_gharial2014_m9)

svabu_gharial2014_m10 <- svabu(gharial_count_2014 ~ dist_km_log + zif(dist_km_log)| 1, gharial_svocc_merged)
summary(svabu_gharial2014_m10)

svabu_gharial2014_aic <- AIC(svabu_gharial2014_m1,svabu_gharial2014_m3,svabu_gharial2014_m4,
                             svabu_gharial2014_m6,svabu_gharial2014_m7,svabu_gharial2014_m8,
                             svabu_gharial2014_m9,svabu_gharial2014_m10)

##Exporting coefficients of best models (top two) and null
gharial_svabu2014_list <- list(
  "Best" = svabu_gharial2014_m3,
  "Second best" = svabu_gharial2014_m4,
  "Null" = svabu_gharial2014_m1)

modelsummary(gharial_svabu2014_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "gharial_svabu2014_coeftable.docx")

modelsummary(svabu_gharial2014_m3)
modelsummary(svabu_gharial2014_m4)
modelsummary(svabu_gharial2014_m1)

#For 2017
svabu_gharial2017_m1 <- svabu(gharial_count_2017 ~ dist_km_log | 1, gharial_svocc_merged)
summary(svabu_gharial2017_m1) #NA test stats

svabu_gharial2017_m2 <- svabu(gharial_count_2017 ~ dist_km_log | spatial_autocov_gharial2017, gharial_svocc_merged)
summary(svabu_gharial2017_m2) #NA test stats

svabu_gharial2017_m3 <- svabu(gharial_count_2017 ~ dist_km_log | fishingactivity_2017, gharial_svocc_merged)
summary(svabu_gharial2017_m3)

svabu_gharial2017_m4 <- svabu(gharial_count_2017 ~ dist_km_log | fishingboats_2017, gharial_svocc_merged)
summary(svabu_gharial2017_m4)

svabu_gharial2017_m5 <- svabu(gharial_count_2017 ~ dist_km_log + fishingactivity_2017| 1, gharial_svocc_merged)
summary(svabu_gharial2017_m5)

svabu_gharial2017_m6 <- svabu(gharial_count_2017 ~ dist_km_log + fishingboats_2017| 1, gharial_svocc_merged)
summary(svabu_gharial2017_m6)

svabu_gharial2017_m7 <- svabu(gharial_count_2017 ~ dist_km_log + zif(spatial_autocov_gharial2017)| 1, gharial_svocc_merged)
summary(svabu_gharial2017_m7)  #NA test stats

svabu_gharial2017_m8 <- svabu(gharial_count_2017 ~ dist_km_log + zif(fishingactivity_2017)| 1, gharial_svocc_merged)
summary(svabu_gharial2017_m8)  #NA test stats

svabu_gharial2017_m9 <- svabu(gharial_count_2017 ~ dist_km_log + zif(fishingboats_2017)| 1, gharial_svocc_merged)
summary(svabu_gharial2017_m9)  #NA test stats

svabu_gharial2017_m10 <- svabu(gharial_count_2017 ~ dist_km_log + zif(dist_km_log)| 1, gharial_svocc_merged)
summary(svabu_gharial2017_m10)  #NA test stats

##Exporting coefficients of models with non NA test stats
gharial_svabu2017_list <- list(
  "Model 1" = svabu_gharial2017_m4,
  "Model 2" = svabu_gharial2017_m3,
  "Model 3" = svabu_gharial2017_m5,
  "Model 4" = svabu_gharial2017_m6)

modelsummary(gharial_svabu2017_list, 
             statistic = c("s.e. = {std.error}", "z = {z}","p = {p}"),
             output = "gharial_svabu2017_coeftable.docx")

modelsummary(svabu_gharial2017_m4)
modelsummary(svabu_gharial2017_m3)
modelsummary(svabu_gharial2017_m5)
modelsummary(svabu_gharial2017_m6)