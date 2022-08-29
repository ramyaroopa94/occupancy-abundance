rm(list=ls())
library(unmarked)
library(AICcmodavg)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(devtools)
library(detect)
grd<-read.csv("grd_Occu_Gandak.csv")
summary(grd)
covariate<-read.csv("Covariate_Gandak.csv",stringsAsFactors = T)
covariate<-covariate[-211,]
summary(covariate)
gharial<-read.csv("Gharial_Occu_Gandak.csv")
summary(gharial)
############----------------------------------------------------------
##Occupancy models for grd at 5km scale
##Exploratory analysis
grd_cov_merged<-merge(grd,covariate,by=c("reach_ID","replicate_segt"))
##distance along river
dist_occ1<-ggplot(data=grd_cov_merged,aes(x=dist_km.x,y=grd2010_occ))+
           geom_point()+theme_bw()
dist_occ2<-ggplot(data=grd_cov_merged,aes(x=dist_km.x,y=grd2013_occ))+
           geom_point()+theme_bw()
dist_occ3<-ggplot(data=grd_cov_merged,aes(x=dist_km.x,y=grd2017_occ))+
           geom_point()+theme_bw()

##fishing activity
fishingactivity_occ1<-grd_cov_merged%>%filter(grd2010_occ==1)%>%
  ggplot(aes(x=fishingactivity_2010))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2010)")+theme_bw()

fishingactivity_occ2<-grd_cov_merged%>%filter(grd2013_occ==1)%>%
  ggplot(aes(x=fishingactivity_2014))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2014)")+theme_bw()

fishingactivity_occ3<-grd_cov_merged%>%filter(grd2017_occ==1)%>%
  ggplot(aes(x=fishingactivity_2017))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2017)")+theme_bw()

fishingactivity_occ<-ggarrange(fishingactivity_occ1, fishingactivity_occ2, fishingactivity_occ3, ncol = 1, nrow = 3)

ggsave("fishingactivity_occ.png", plot=fishingactivity_occ,width=40,height=40,unit="cm",dpi=500)    

##fishing boats
fishingboats_occ1<-grd_cov_merged%>%filter(grd2010_occ==1)%>%
  ggplot(aes(x=fishingboats_2010))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ2<-grd_cov_merged%>%filter(grd2013_occ==1)%>%
  ggplot(aes(x=fishingboats_2014))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ3<-grd_cov_merged%>%filter(grd2017_occ==1)%>%
  ggplot(aes(x=fishingboats_2017))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2017)")+
  theme_bw()

##formatting variables to unmarked format---------
##creating new dataset for modelling occupancy at 5km segment scale
covariate_occ5km<-covariate%>%group_by(reach_ID)%>%
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
grd_occ1<-grd%>%select(reach_ID,replicate_segt,grd2010_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2010_occ)
grd_occ2<-grd%>%select(reach_ID,replicate_segt,grd2013_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2013_occ)
grd_occ3<-grd%>%select(reach_ID,replicate_segt,grd2017_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2017_occ)
grd_occ_list<-list(grd_occ1,grd_occ2,grd_occ3)
grd_occ<-grd_occ_list%>%reduce(full_join,by="reach_ID")
grd_occ_mat<-data.matrix(grd_occ[,2:16])

##fishing activity - obscov (in unmarked)
cov_fishingactivity2010<-covariate%>%select(reach_ID,replicate_segt,fishingactivity_2010)%>%pivot_wider(names_from = replicate_segt,values_from = fishingactivity_2010)
cov_fishingactivity2014<-covariate%>%select(reach_ID,replicate_segt,fishingactivity_2014)%>%pivot_wider(names_from = replicate_segt,values_from = fishingactivity_2014)
cov_fishingactivity2017<-covariate%>%select(reach_ID,replicate_segt,fishingactivity_2017)%>%pivot_wider(names_from = replicate_segt,values_from = fishingactivity_2017)
cov_fishingactivity_list<-list(cov_fishingactivity2010,
                               cov_fishingactivity2014,
                               cov_fishingactivity2017)
cov_fishingactivity<-cov_fishingactivity_list%>%reduce(full_join,by="reach_ID")
cov_fishingactivity_mat<-data.matrix(cov_fishingactivity[,2:16])

##fishing boats - obscov (in unmarked)
cov_fishingboats2010<-covariate%>%select(reach_ID,replicate_segt,fishingboats_2010)%>%pivot_wider(names_from = replicate_segt,values_from = fishingboats_2010)
cov_fishingboats2014<-covariate%>%select(reach_ID,replicate_segt,fishingboats_2014)%>%pivot_wider(names_from = replicate_segt,values_from = fishingboats_2014)
cov_fishingboats2017<-covariate%>%select(reach_ID,replicate_segt,fishingboats_2017)%>%pivot_wider(names_from = replicate_segt,values_from = fishingboats_2017)
cov_fishingboats_list<-list(cov_fishingboats2010,
                            cov_fishingboats2014,
                            cov_fishingboats2017)
cov_fishingboats<-cov_fishingboats_list%>%reduce(full_join,by="reach_ID")
cov_fishingboats_mat<-data.matrix(cov_fishingboats[,2:16])

cov_fishing<-list(fishingact=cov_fishingactivity_mat,fishingboat=cov_fishingboats_mat)

##Testing correlation between fishing act and fishing boats as obsCovs
stack_fishingact<-stack(covariate,select=c("fishingactivity_2010","fishingactivity_2014","fishingactivity_2017"))
stack_fishingboat<-stack(covariate,select=c("fishingboats_2010","fishingboats_2014","fishingboats_2017"))
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
grd_occ_df@yearlySiteCovs[["season"]]<-relevel(grd_occ_df@yearlySiteCovs[["season"]], ref = "Jan")  

##Models
grd_occ_model1 <- colext(~1, ~1, ~1, ~1, grd_occ_df)
summary(grd_occ_model1)
backTransform(grd_occ_model1,type="psi")
backTransform(grd_occ_model1,type="col")
backTransform(grd_occ_model1,type="ext")
backTransform(grd_occ_model1,type="det")

grd_occ_model2 <- colext(~1, ~total_fishingactivity, ~total_fishingactivity, ~1, grd_occ_df)
summary(grd_occ_model2)

grd_occ_model3 <- colext(~1, ~total_fishingboats, ~total_fishingboats, ~1, grd_occ_df)
summary(grd_occ_model3) ##Nans produced

grd_occ_model4 <- colext(~1, ~season-1, ~season-1, ~1, grd_occ_df)
summary(grd_occ_model4)

grd_occ_model5 <- colext(~median_dist_km, ~1, ~1, ~1, grd_occ_df)
summary(grd_occ_model5)

grd_occ_model6 <- colext(~median_dist_km, ~total_fishingboats, ~total_fishingboats, ~1, grd_occ_df)
summary(grd_occ_model6) ##Nans produced

grd_occ_model7 <- colext(~median_dist_km, ~total_fishingactivity, ~total_fishingactivity, ~1, grd_occ_df)
summary(grd_occ_model7)

grd_occ_model8 <- colext(~median_dist_km, ~total_fishingactivity+total_fishingboats, ~total_fishingactivity+total_fishingboats, ~1, grd_occ_df)
summary(grd_occ_model8)

grd_occ_model9 <- colext(~median_dist_km, ~season-1, ~season-1, ~1, grd_occ_df)
summary(grd_occ_model9)

grd_occ_model10 <- colext(~1, ~1, ~1, ~fishingact, grd_occ_df) ##estimates close to zero
summary(grd_occ_model10)

grd_occ_model11 <- colext(~1, ~1, ~1, ~fishingboat, grd_occ_df) ##estimates close to zero
summary(grd_occ_model11)

grd_occ_model12 <- colext(~1, ~season-1+total_fishingactivity, ~season-1+total_fishingactivity, ~1, grd_occ_df)
summary(grd_occ_model12)

grd_occ_models <- fitList('psi(.)gam(.)eps(.)p(.)' = grd_occ_model1,
                          'psi(.)gam(fishingact)eps(fishingact)p(.)' = grd_occ_model2,
                          'psi(.)gam(season)eps(season)p(.)' = grd_occ_model4, 
                          'psi(dist_km)gam(.)eps(.)p(.)' = grd_occ_model5,
                          'psi(dist_km)gam(fishingact)eps(fishingact)p(.)' = grd_occ_model7,
                          'psi(dist_km)gam(fishingact+fishingboat)eps(fishingact+fishingboat)p(.)' = grd_occ_model8, 
                          'psi(dist_km)gam(season)eps(season)p(.)' = grd_occ_model9, 
                          'psi(.)gam(.)eps(.)p(fishingact)' = grd_occ_model10,
                          'psi(.)gam(.)eps(.)p(fishingboat)' = grd_occ_model11,     
                          'psi(.)gam(fishingact+season)eps(fishingact+season)p(.)' = grd_occ_model12)
                  
grd_occ_models_AIC<-modSel(grd_occ_models)
grd_occ_models_AICtable<-grd_occ_models_AIC@Full[c("model","negLogLike","nPars","n","AIC","delta","AICwt","cumltvWt")]
write.table(grd_occ_models_AICtable, file = "grd_occ_AIC_23.08.22.txt", sep = "\t",
            row.names = F)

grd_occ_top1<-summary(grd_occ_model4)
grd_occ_top1_estimate<-rbind(grd_occ_top1[["psi"]],grd_occ_top1[["col"]],
                             grd_occ_top1[["ext"]],grd_occ_top1[["det"]])
rownames(grd_occ_top1_estimate)<-c("psi-intercept","col-seasonJan","col-seasonApr",
                                   "ext-seasonJan","ext-seasonApr","det-intercept")
write.table(grd_occ_top1_estimate, file = "grd_occ_estimate_top1.txt", sep = "\t",
            row.names = T)

grd_occ_top2<-summary(grd_occ_model1)
grd_occ_top2_estimate<-rbind(grd_occ_top2[["psi"]],grd_occ_top2[["col"]],
                             grd_occ_top2[["ext"]],grd_occ_top2[["det"]])
rownames(grd_occ_top2_estimate)<-c("psi-intercept","col-intercept",
                                   "ext-intercept","det-intercept")
write.table(grd_occ_top2_estimate, file = "grd_occ_estimate_top2.txt", sep = "\t",
            row.names = T)
############----------------------------------------------------------
##Occupancy models for gharials at 5km scale
##Exploratory analysis
grl_cov_merged<-merge(gharial,covariate,by=c("reach_ID","replicate_segt"))
##distance along river
dist_occ_grl1<-ggplot(data=grl_cov_merged,aes(x=dist_km.x,y=gharial_2014occ))+
  geom_point()+theme_bw()
dist_occ_grl2<-ggplot(data=grl_cov_merged,aes(x=dist_km.x,y=gharial_2017occ))+
  geom_point()+theme_bw()

##fishing activity
fishingactivity_occ_grl1<-grl_cov_merged%>%filter(gharial_2014occ==1)%>%
  ggplot(aes(x=fishingactivity_2014))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2014)")+theme_bw()

fishingactivity_occ_grl2<-grl_cov_merged%>%filter(gharial_2017occ==1)%>%
  ggplot(aes(x=fishingactivity_2017))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2017)")+theme_bw()

##fishing boats
fishingboats_occ_grl1<-grl_cov_merged%>%filter(gharial_2014occ==1)%>%
  ggplot(aes(x=fishingboats_2014))+
  geom_histogram(binwidth = 1,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ_grl2<-grl_cov_merged%>%filter(gharial_2017occ==1)%>%
  ggplot(aes(x=fishingboats_2017))+
  geom_histogram(binwidth = 1,color="red")+
  labs(y="No. of detections",x="Fishing boats (2017)")+
  theme_bw()

##formatting variables to unmarked format---------
##formatting occu data
grl_occ1<-gharial%>%select(reach_ID,replicate_segt,gharial_2014occ)%>%pivot_wider(names_from = replicate_segt,values_from = gharial_2014occ)
grl_occ2<-gharial%>%select(reach_ID,replicate_segt,gharial_2017occ)%>%pivot_wider(names_from = replicate_segt,values_from = gharial_2017occ)
grl_occ_list<-list(grl_occ1,grl_occ2)
grl_occ<-grl_occ_list%>%reduce(full_join,by="reach_ID")
grl_occ_mat<-data.matrix(grl_occ[,2:11])

cov_fishingactivity_mat_grl<-cov_fishingactivity_mat[,6:15]
cov_fishingboats_mat_grl<-cov_fishingboats_mat[,6:15]

cov_fishing_grl<-list(fishingact_grl=cov_fishingactivity_mat_grl,fishingboat_grl=cov_fishingboats_mat_grl)

season_grl<-rep(c("Apr","Nov"),times=42)
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
summary(grl_occ_model2)

grl_occ_model3 <- colext(~1, ~total_fishingactivity, ~total_fishingactivity, ~1, grl_occ_df)
summary(grl_occ_model3)

grl_occ_model4 <- colext(~1, ~total_fishingboats, ~total_fishingboats, ~1, grl_occ_df)
summary(grl_occ_model4) 

grl_occ_model5 <- colext(~1, ~1, ~1, ~fishingact_grl, grl_occ_df)
summary(grl_occ_model5)

grl_occ_model6 <- colext(~1, ~1, ~1, ~fishingboat_grl, grl_occ_df)
summary(grl_occ_model6) 

grl_occ_models <- fitList('psi(.)gam(.)eps(.)p(.)' = grl_occ_model1,
                          'psi(dist_km)gam(.)eps(.)p(.)' = grl_occ_model2,
                          'psi(.)gam(fishingact)eps(fishingact)p(.)' = grl_occ_model3, 
                          'psi(.)gam(fishingboat)eps(fishingboat)p(.)' = grl_occ_model4,
                          'psi(.)gam(.)eps(.)p(fishingact)' = grl_occ_model5,
                          'psi(.)gam(.)eps(.)p(fishingboat)' = grl_occ_model6)
                          

grl_occ_models_AIC<-modSel(grl_occ_models)
grl_occ_models_AICtable<-grl_occ_models_AIC@Full[c("model","negLogLike","nPars","n","AIC","delta","AICwt","cumltvWt")]
write.table(grl_occ_models_AICtable, file = "GRL_occ_AIC_23.08.22.txt", sep = "\t",
            row.names = F)

grl_occ_top1<-summary(grl_occ_model4)
grl_occ_top1_estimate<-rbind(grl_occ_top1[["psi"]],grl_occ_top1[["col"]],
                             grl_occ_top1[["ext"]],grl_occ_top1[["det"]])
rownames(grl_occ_top1_estimate)<-c("psi-intercept","col-intercept","col-fishingboat",
                                   "ext-intercept","ext-fishingboat","det-intercept")
write.table(grl_occ_top1_estimate, file = "GRL_occ_estimate_top1.txt", sep = "\t",
            row.names = T)

grl_occ_top2<-summary(grl_occ_model3)
grl_occ_top2_estimate<-rbind(grl_occ_top2[["psi"]],grl_occ_top2[["col"]],
                             grl_occ_top2[["ext"]],grl_occ_top2[["det"]])
rownames(grl_occ_top2_estimate)<-c("psi-intercept","col-intercept","col-fishingact",
                                   "ext-intercept","ext-fishingact","det-intercept")
write.table(grl_occ_top2_estimate, file = "GRL_occ_estimate_top2.txt", sep = "\t",
            row.names = T)

grl_occ_top3<-summary(grl_occ_model1)
grl_occ_top3_estimate<-rbind(grl_occ_top3[["psi"]],grl_occ_top3[["col"]],
                             grl_occ_top3[["ext"]],grl_occ_top3[["det"]])
rownames(grl_occ_top3_estimate)<-c("psi-intercept","col-intercept",
                                   "ext-intercept","det-intercept")
write.table(grl_occ_top3_estimate, file = "GRL_occ_estimate_top3.txt", sep = "\t",
            row.names = T)

############----------------------------------------------------------
##Occupancy models for grd at 1km scale
##Adding covariate at 1km scale to account for spatial autocorrelation
magic_for(print, silent = TRUE)
##adding for 
for(i in 1:nrow(grd)){
  if(i==1){
    spatial_autocov_grd2010<-ifelse(grd$grd2010_occ[i+1]==0,0,1)
  } else if(i==nrow(grd)){
    spatial_autocov_grd2010<-ifelse(grd$grd2010_occ[i-1]==0,0,1)
  } else
    spatial_autocov_grd2010<-ifelse(grd$grd2010_occ[i-1]==0 & grd$grd2010_occ[i+1]==0,0,1)
  print(spatial_autocov_grd2010)
}

spatial_autocov_grd2010<-magic_result_as_dataframe()[,2] 

for(i in 1:nrow(grd)){
  if(i==1){
    spatial_autocov_grd2013<-ifelse(grd$grd2013_occ[i+1]==0,0,1)
  } else if(i==nrow(grd)){
    spatial_autocov_grd2013<-ifelse(grd$grd2013_occ[i-1]==0,0,1)
  } else
    spatial_autocov_grd2013<-ifelse(grd$grd2013_occ[i-1]==0 & grd$grd2013_occ[i+1]==0,0,1)
  print(spatial_autocov_grd2013)
}

spatial_autocov_grd2013<-magic_result_as_dataframe()[,2]

for(i in 1:nrow(grd)){
  if(i==1){
    spatial_autocov_grd2017<-ifelse(grd$grd2017_occ[i+1]==0,0,1)
  } else if(i==nrow(grd)){
    spatial_autocov_grd2017<-ifelse(grd$grd2017_occ[i-1]==0,0,1)
  } else
    spatial_autocov_grd2017<-ifelse(grd$grd2017_occ[i-1]==0 & grd$grd2017_occ[i+1]==0,0,1)
  print(spatial_autocov_grd2017)
}

spatial_autocov_grd2017<-magic_result_as_dataframe()[,2]

grd<-cbind(grd,spatial_autocov_grd2010,spatial_autocov_grd2013,spatial_autocov_grd2017)
grd$dist_km.s<-scale(grd$dist_km,center=T,scale = T)
grd_svocc_merged<-merge(grd,covariate,by=c("reach_ID","replicate_segt","dist_km"))
str(grd_svocc_merged)

##Building single-visit models using the 'detect' package
#For 2010
svocc_grd2010_m1<-svocc(grd2010_occ ~ dist_km.s | 1, grd_svocc_merged)
svocc_grd2010_m1_sum<-summary(svocc_grd2010_m1)
coef(svocc_grd2010_m1, "sta")
confint(svocc_grd2010_m1, model="sta")
coef(svocc_grd2010_m1, "det")
confint(svocc_grd2010_m1, model="det")

svocc_grd2010_m1.1<-svocc(grd2010_occ ~ dist_km.s | spatial_autocov_grd2010, grd_svocc_merged)
svocc_grd2010_m1_sum<-summary(svocc_grd2010_m1.1)
coef(svocc_grd2010_m1, "sta")
confint(svocc_grd2010_m1, model="sta")
coef(svocc_grd2010_m1, "det")
confint(svocc_grd2010_m1, model="det")

svocc_grd2010_m2<-svocc(grd2010_occ ~ dist_km.s | fishingactivity_2010, grd_svocc_merged)
svocc_grd2010_m2_sum<-summary(svocc_grd2010_m2)
coef(svocc_grd2010_m2, "sta")
confint(svocc_grd2010_m2,model="sta")
coef(svocc_grd2010_m2, "det")
confint(svocc_grd2010_m2,model="det")

svocc_grd2010_m3<-svocc(grd2010_occ ~ dist_km.s | fishingboats_2010, grd_svocc_merged)
svocc_grd2010_m3_sum<-summary(svocc_grd2010_m3)
coef(svocc_grd2010_m3, "sta")
confint(svocc_grd2010_m3,model="sta")
coef(svocc_grd2010_m3, "det")
confint(svocc_grd2010_m3,model="det")

svocc_grd2010_m4<-svocc(grd2010_occ ~ dist_km.s + spatial_autocov_grd2010| 1, grd_svocc_merged)
svocc_grd2010_m4_sum<-summary(svocc_grd2010_m4)
coef(svocc_grd2010_m4, "sta")
confint(svocc_grd2010_m4,model="sta")
coef(svocc_grd2010_m4, "det")
confint(svocc_grd2010_m4,model="det")

svocc_grd2010_m5<-svocc(grd2010_occ ~ dist_km.s + fishingactivity_2010| 1, grd_svocc_merged)
svocc_grd2010_m5_sum<-summary(svocc_grd2010_m5)
coef(svocc_grd2010_m5, "sta")
confint(svocc_grd2010_m5,model="sta")
coef(svocc_grd2010_m5, "det")
confint(svocc_grd2010_m5,model="det")

svocc_grd2010_m6<-svocc(grd2010_occ ~ dist_km.s + fishingboats_2010| 1, grd_svocc_merged)
svocc_grd2010_m6_sum<-summary(svocc_grd2010_m6)
coef(svocc_grd2010_m6, "sta")
confint(svocc_grd2010_m6,model="sta")
coef(svocc_grd2010_m6, "det")
confint(svocc_grd2010_m6,model="det")

svocc_grd2010_m7<-svocc(grd2010_occ ~ dist_km.s + fishingactivity_2010 + fishingboats_2010| 1, grd_svocc_merged)
svocc_grd2010_m7_sum<-summary(svocc_grd2010_m7)
coef(svocc_grd2010_m7, "sta")
confint(svocc_grd2010_m7,model="sta")
coef(svocc_grd2010_m7, "det")
confint(svocc_grd2010_m7,model="det")

svocc_grd2010_m8<-svocc(grd2010_occ ~ dist_km.s + fishingactivity_2010 + fishingboats_2010 + spatial_autocov_grd2010| fishingactivity_2010, grd_svocc_merged)
svocc_grd2010_m8_sum<-summary(svocc_grd2010_m8)
coef(svocc_grd2010_m8, "sta")
confint(svocc_grd2010_m8,model="sta")
coef(svocc_grd2010_m8, "det")
confint(svocc_grd2010_m8,model="det")

svocc.step(svocc_grd2010_m8, model="sta")
svocc.step(svocc_grd2010_m8, model="det")

##Exporting results - 2010
grd2010_svocc_top1_estimate<-rbind(svocc_grd2010_m5_sum[["sta"]],svocc_grd2010_m5_sum[["det"]])
rownames(grd2010_svocc_top1_estimate)<-c("psi-intercept","dist_km",
                                   "fishingactivity","det-intercept")
write.table(grd2010_svocc_top1_estimate, file = "grd2010_svocc_top1_estimate.txt", sep = "\t",
            row.names = T)

grd2010_svocc_top2_estimate<-rbind(svocc_grd2010_m1_sum[["sta"]],svocc_grd2010_m1_sum[["det"]])
rownames(grd2010_svocc_top2_estimate)<-c("psi-intercept","dist_km","det-intercept")
write.table(grd2010_svocc_top2_estimate, file = "grd2010_svocc_top2_estimate.txt", sep = "\t",
            row.names = T)

#For 2013
svocc_grd2013_full<-svocc(grd2013_occ ~ dist_km.s + fishingactivity_2014 + fishingboats_2014 + spatial_autocov_grd2013| fishingactivity_2014 + fishingboats_2014, grd_svocc_merged)
summary(svocc_grd2013_full)
coef(svocc_grd2013_full, "sta")
confint(svocc_grd2013_full,model="sta")
coef(svocc_grd2013_full, "det")
confint(svocc_grd2013_full,model="det")

svocc.step(svocc_grd2013_full, model="sta")
svocc.step(svocc_grd2013_full, model="det")

svocc_grd2013_m1<-svocc(grd2013_occ ~ dist_km.s|1,grd_svocc_merged)
svocc_grd2013_m1_sum<-summary(svocc_grd2013_m1)
coef(svocc_grd2013_m1, "sta")
confint(svocc_grd2013_m1, model="sta")
coef(svocc_grd2013_m1, "det")
confint(svocc_grd2013_m1, model="det")

svocc_grd2013_m2<-svocc(grd2013_occ ~ dist_km.s | fishingactivity_2014, grd_svocc_merged)
svocc_grd2013_m2_sum<-summary(svocc_grd2013_m2)
coef(svocc_grd2013_m2, "sta")
confint(svocc_grd2013_m2,model="sta")
coef(svocc_grd2013_m2, "det")
confint(svocc_grd2013_m2,model="det")

svocc_grd2013_m3<-svocc(grd2013_occ ~ dist_km.s | fishingboats_2014, grd_svocc_merged)
svocc_grd2013_m3_sum<-summary(svocc_grd2013_m3)
coef(svocc_grd2013_m3, "sta")
confint(svocc_grd2013_m3,model="sta")
coef(svocc_grd2013_m3, "det")
confint(svocc_grd2013_m3,model="det")

svocc_grd2013_m4<-svocc(grd2013_occ ~ dist_km.s + spatial_autocov_grd2013| 1, grd_svocc_merged)
svocc_grd2013_m4_sum<-summary(svocc_grd2013_m4)
coef(svocc_grd2013_m4, "sta")
confint(svocc_grd2013_m4,model="sta")
coef(svocc_grd2013_m4, "det")
confint(svocc_grd2013_m4,model="det")

svocc_grd2013_m5<-svocc(grd2013_occ ~ dist_km.s + fishingactivity_2014| 1, grd_svocc_merged)
svocc_grd2013_m5_sum<-summary(svocc_grd2013_m5)
coef(svocc_grd2013_m5, "sta")
confint(svocc_grd2013_m5,model="sta")
coef(svocc_grd2013_m5, "det")
confint(svocc_grd2013_m5,model="det")

svocc_grd2013_m6<-svocc(grd2013_occ ~ dist_km.s + fishingboats_2014| 1, grd_svocc_merged)
svocc_grd2013_m6_sum<-summary(svocc_grd2013_m6)
coef(svocc_grd2013_m6, "sta")
confint(svocc_grd2013_m6,model="sta")
coef(svocc_grd2013_m6, "det")
confint(svocc_grd2013_m6,model="det")

svocc_grd2013_m7<-svocc(grd2013_occ ~ dist_km.s + spatial_autocov_grd2013 + fishingactivity_2014 + fishingboats_2014| 1, grd_svocc_merged)
svocc_grd2013_m7_sum<-summary(svocc_grd2013_m7)
coef(svocc_grd2013_m7, "sta")
confint(svocc_grd2013_m7,model="sta")
coef(svocc_grd2013_m7, "det")
confint(svocc_grd2013_m7,model="det")

svocc.step(svocc_grd2013_m7, model="sta")
svocc.step(svocc_grd2013_m7, model="det")

##For 2017
svocc_grd2017_m1<-svocc(grd2017_occ ~ dist_km.s|1,grd_svocc_merged)
svocc_grd2017_m1_sum<-summary(svocc_grd2017_m1)
coef(svocc_grd2017_m1, "sta")
confint(svocc_grd2017_m1, model="sta")
coef(svocc_grd2017_m1, "det")
confint(svocc_grd2017_m1, model="det")

svocc_grd2017_m2<-svocc(grd2017_occ ~ dist_km.s|fishingactivity_2017, grd_svocc_merged)
svocc_grd2017_m2_sum<-summary(svocc_grd2017_m2)
coef(svocc_grd2017_m2, "sta")
confint(svocc_grd2017_m2, model="sta")
coef(svocc_grd2017_m2, "det")
confint(svocc_grd2017_m2, model="det")

svocc_grd2017_m3<-svocc(grd2017_occ ~ dist_km.s|fishingboats_2017, grd_svocc_merged)
svocc_grd2017_m3_sum<-summary(svocc_grd2017_m3)
coef(svocc_grd2017_m3, "sta")
confint(svocc_grd2017_m3, model="sta")
coef(svocc_grd2017_m3, "det")
confint(svocc_grd2017_m3, model="det")

svocc_grd2017_m4<-svocc(grd2017_occ ~ dist_km.s+spatial_autocov_grd2017|1, grd_svocc_merged)
svocc_grd2017_m4_sum<-summary(svocc_grd2017_m4)
coef(svocc_grd2017_m4, "sta")
confint(svocc_grd2017_m4, model="sta")
coef(svocc_grd2017_m4, "det")
confint(svocc_grd2017_m4, model="det")

svocc_grd2017_m5<-svocc(grd2017_occ ~ dist_km.s+fishingactivity_2017|1, grd_svocc_merged)
svocc_grd2017_m5_sum<-summary(svocc_grd2017_m5)
coef(svocc_grd2017_m5, "sta")
confint(svocc_grd2017_m5, model="sta")
coef(svocc_grd2017_m5, "det")
confint(svocc_grd2017_m5, model="det")

svocc_grd2017_m6<-svocc(grd2017_occ ~ dist_km.s+fishingboats_2017|1, grd_svocc_merged)
svocc_grd2017_m6_sum<-summary(svocc_grd2017_m6)
coef(svocc_grd2017_m6, "sta")
confint(svocc_grd2017_m6, model="sta")
coef(svocc_grd2017_m6, "det")
confint(svocc_grd2017_m6, model="det")

##Exporting results - 2017
grd2017_svocc_top2_estimate<-rbind(svocc_grd2017_m5_sum[["sta"]],svocc_grd2017_m5_sum[["det"]])
rownames(grd2017_svocc_top2_estimate)<-c("psi-intercept","dist_km",
                                         "fishingactivity","det-intercept")
write.table(grd2017_svocc_top2_estimate, file = "grd2017_svocc_top2_estimate.txt", sep = "\t",
            row.names = T)

############----------------------------------------------------------
##Occupancy models for gharials at 1km scale
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
svocc_gharial2014_m1_sum <- summary(svocc_gharial2014_m1)
coef(svocc_gharial2014_m1, "sta")
confint(svocc_gharial2014_m1, model="sta")
coef(svocc_gharial2014_m1, "det")
confint(svocc_gharial2014_m1, model="det")

svocc_gharial2014_m2 <- svocc(gharial_2014occ ~ dist_km_log | spatial_autocov_gharial2014, gharial_svocc_merged)
svocc_gharial2014_m2_sum <- summary(svocc_gharial2014_m2)
coef(svocc_gharial2014_m2, "sta")
confint(svocc_gharial2014_m2, model="sta")
coef(svocc_gharial2014_m2, "det")
confint(svocc_gharial2014_m2, model="det")

svocc_gharial2014_m3 <- svocc(gharial_2014occ ~ dist_km_log| fishingactivity_2014, gharial_svocc_merged)
svocc_gharial2014_m3_sum <- summary(svocc_gharial2014_m3)
coef(svocc_gharial2014_m3, "sta")
confint(svocc_gharial2014_m3, model="sta")
coef(svocc_gharial2014_m3, "det")
confint(svocc_gharial2014_m3, model="det")

svocc_gharial2014_m4 <- svocc(gharial_2014occ ~ dist_km_log| fishingboats_2014, gharial_svocc_merged)
svocc_gharial2014_m4_sum <- summary(svocc_gharial2014_m4)
coef(svocc_gharial2014_m4, "sta")
confint(svocc_gharial2014_m4, model="sta")
coef(svocc_gharial2014_m4, "det")
confint(svocc_gharial2014_m4, model="det")

svocc_gharial2014_m5 <- svocc(gharial_2014occ ~ dist_km_log+spatial_autocov_gharial2014| spatial_autocov_gharial2014, gharial_svocc_merged)
svocc_gharial2014_m5_sum <- summary(svocc_gharial2014_m5)
coef(svocc_gharial2014_m5, "sta")
confint(svocc_gharial2014_m5, model="sta")
coef(svocc_gharial2014_m5, "det")
confint(svocc_gharial2014_m5, model="det")

svocc_gharial2014_m6 <- svocc(gharial_2014occ ~ dist_km_log+fishingactivity_2014| 1, gharial_svocc_merged)
svocc_gharial2014_m6_sum <- summary(svocc_gharial2014_m6)
coef(svocc_gharial2014_m6, "sta")
confint(svocc_gharial2014_m6, model="sta")
coef(svocc_gharial2014_m6, "det")
confint(svocc_gharial2014_m6, model="det")

#For 2017
svocc_gharial2017_m1 <- svocc(gharial_2017occ ~ dist_km_log | 1, gharial_svocc_merged)
svocc_gharial2017_m1_sum <- summary(svocc_gharial2017_m1)
coef(svocc_gharial2014_m1, "sta")
confint(svocc_gharial2014_m1, model="sta")
coef(svocc_gharial2014_m1, "det")
confint(svocc_gharial2014_m1, model="det")

svocc_gharial2017_m2 <- svocc(gharial_2017occ ~ dist_km_log | spatial_autocov_gharial2017, gharial_svocc_merged)
svocc_gharial2017_m2_sum <- summary(svocc_gharial2017_m2)
coef(svocc_gharial2017_m2, "sta")
confint(svocc_gharial2017_m2, model="sta")
coef(svocc_gharial2017_m2, "det")
confint(svocc_gharial2017_m2, model="det")

svocc_gharial2017_m3 <- svocc(gharial_2017occ ~ dist_km_log | fishingactivity_2017, gharial_svocc_merged)
svocc_gharial2017_m3_sum <- summary(svocc_gharial2017_m3)
coef(svocc_gharial2017_m3, "sta")
confint(svocc_gharial2017_m3, model="sta")
coef(svocc_gharial2017_m3, "det")
confint(svocc_gharial2017_m3, model="det")

svocc_gharial2017_m4 <- svocc(gharial_2017occ ~ dist_km_log | fishingboats_2017, gharial_svocc_merged)
svocc_gharial2017_m4_sum <- summary(svocc_gharial2017_m4)
coef(svocc_gharial2017_m4, "sta")
confint(svocc_gharial2017_m4, model="sta")
coef(svocc_gharial2017_m4, "det")
confint(svocc_gharial2017_m4, model="det")

svocc_gharial2017_m5 <- svocc(gharial_2017occ ~ dist_km_log + spatial_autocov_gharial2017| 1, gharial_svocc_merged)
svocc_gharial2017_m5_sum <- summary(svocc_gharial2017_m5)
coef(svocc_gharial2017_m5, "sta")
confint(svocc_gharial2017_m5, model="sta")
coef(svocc_gharial2017_m5, "det")
confint(svocc_gharial2017_m5, model="det")

svocc_gharial2017_m6 <- svocc(gharial_2017occ ~ dist_km_log + fishingactivity_2017| 1, gharial_svocc_merged)
svocc_gharial2017_m6_sum <- summary(svocc_gharial2017_m6)
coef(svocc_gharial2017_m6, "sta")
confint(svocc_gharial2017_m6, model="sta")
coef(svocc_gharial2017_m6, "det")
confint(svocc_gharial2017_m6, model="det")

svocc_gharial2017_m7 <- svocc(gharial_2017occ ~ dist_km_log + fishingboats_2017| 1, gharial_svocc_merged)
svocc_gharial2017_m7_sum <- summary(svocc_gharial2017_m7)
coef(svocc_gharial2017_m7, "sta")
confint(svocc_gharial2017_m7, model="sta")
coef(svocc_gharial2017_m7, "det")
confint(svocc_gharial2017_m7, model="det")
