rm(list=ls())
library(unmarked)
library(AICcmodavg)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(xtable)
GRD<-read.csv("GRD_Occu_Gandak.csv")
summary(GRD)
covariate<-read.csv("Covariate_Gandak.csv",stringsAsFactors = T)
covariate<-covariate[-211,]
summary(covariate)

############------------------------------------------
##Occupancy models for GRD at 5km scale
##Exploratory analysis
GRD_cov_merged<-merge(GRD,covariate,by=c("reach_ID","replicate_segt"))
##distance along river
dist_occ1<-ggplot(data=GRD_cov_merged,aes(x=dist_km.x,y=grd2010_occ))+
           geom_point()+theme_bw()
dist_occ2<-ggplot(data=GRD_cov_merged,aes(x=dist_km.x,y=grd2013_occ))+
           geom_point()+theme_bw()
dist_occ3<-ggplot(data=GRD_cov_merged,aes(x=dist_km.x,y=grd2017_occ))+
           geom_point()+theme_bw()

##fishing activity
fishingactivity_occ1<-GRD_cov_merged%>%filter(grd2010_occ==1)%>%
  ggplot(aes(x=fishingactivity_2010))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2010)")+theme_bw()

fishingactivity_occ2<-GRD_cov_merged%>%filter(grd2013_occ==1)%>%
  ggplot(aes(x=fishingactivity_2014))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2014)")+theme_bw()

fishingactivity_occ3<-GRD_cov_merged%>%filter(grd2017_occ==1)%>%
  ggplot(aes(x=fishingactivity_2017))+
  geom_bar()+labs(y="No. of detections",x="Fishing acitivity (2017)")+theme_bw()

fishingactivity_occ<-ggarrange(fishingactivity_occ1, fishingactivity_occ2, fishingactivity_occ3, ncol = 1, nrow = 3)

ggsave("fishingactivity_occ.png", plot=fishingactivity_occ,width=40,height=40,unit="cm",dpi=500)    

##fishing boats
fishingboats_occ1<-GRD_cov_merged%>%filter(grd2010_occ==1)%>%
  ggplot(aes(x=fishingboats_2010))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ2<-GRD_cov_merged%>%filter(grd2013_occ==1)%>%
  ggplot(aes(x=fishingboats_2014))+
  geom_histogram(binwidth = 2,color="red")+
  labs(y="No. of detections",x="Fishing boats (2014)")+
  theme_bw()

fishingboats_occ3<-GRD_cov_merged%>%filter(grd2017_occ==1)%>%
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
GRD_occ1<-GRD%>%select(reach_ID,replicate_segt,grd2010_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2010_occ)
GRD_occ2<-GRD%>%select(reach_ID,replicate_segt,grd2013_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2013_occ)
GRD_occ3<-GRD%>%select(reach_ID,replicate_segt,grd2017_occ)%>%pivot_wider(names_from = replicate_segt,values_from = grd2017_occ)
GRD_occ_list<-list(GRD_occ1,GRD_occ2,GRD_occ3)
GRD_occ<-GRD_occ_list%>%reduce(full_join,by="reach_ID")
GRD_occ_mat<-data.matrix(GRD_occ[,2:16])

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
GRD_occ_df <- unmarkedMultFrame(y=GRD_occ_mat,siteCovs = covariate_occ5km[,2],
                         yearlySiteCovs=list(total_fishingactivity=covariate_occ5km[,3:5],
                                             total_fishingboats=covariate_occ5km[,6:8],
                                             season=covariate_occ5km[,9:11]
                                             ), 
                         obsCovs=cov_fishing,
                         numPrimary=3) 
summary(GRD_occ_df) 
GRD_occ_df@yearlySiteCovs[["season"]]<-relevel(GRD_occ_df@yearlySiteCovs[["season"]], ref = "Jan")  

##Models
GRD_occ_model1 <- colext(~1, ~1, ~1, ~1, GRD_occ_df)
summary(GRD_occ_model1)
backTransform(GRD_occ_model1,type="psi")
backTransform(GRD_occ_model1,type="col")
backTransform(GRD_occ_model1,type="ext")
backTransform(GRD_occ_model1,type="det")

GRD_occ_model2 <- colext(~1, ~total_fishingactivity, ~total_fishingactivity, ~1, GRD_occ_df)
summary(GRD_occ_model2)

GRD_occ_model3 <- colext(~1, ~total_fishingboats, ~total_fishingboats, ~1, GRD_occ_df)
summary(GRD_occ_model3) ##Nans produced

GRD_occ_model4 <- colext(~1, ~season-1, ~season-1, ~1, GRD_occ_df)
summary(GRD_occ_model4)

GRD_occ_model5 <- colext(~median_dist_km, ~1, ~1, ~1, GRD_occ_df)
summary(GRD_occ_model5)

GRD_occ_model6 <- colext(~median_dist_km, ~total_fishingboats, ~total_fishingboats, ~1, GRD_occ_df)
summary(GRD_occ_model6) ##Nans produced

GRD_occ_model7 <- colext(~median_dist_km, ~total_fishingactivity, ~total_fishingactivity, ~1, GRD_occ_df)
summary(GRD_occ_model7)

GRD_occ_model8 <- colext(~median_dist_km, ~total_fishingactivity+total_fishingboats, ~total_fishingactivity+total_fishingboats, ~1, GRD_occ_df)
summary(GRD_occ_model8)

GRD_occ_model9 <- colext(~median_dist_km, ~season-1, ~season-1, ~1, GRD_occ_df)
summary(GRD_occ_model9)

GRD_occ_model10 <- colext(~1, ~1, ~1, ~fishingact, GRD_occ_df) ##estimates close to zero
summary(GRD_occ_model10)

GRD_occ_model11 <- colext(~1, ~1, ~1, ~fishingboat, GRD_occ_df) ##estimates close to zero
summary(GRD_occ_model11)

GRD_occ_model12 <- colext(~1, ~season-1+total_fishingactivity, ~season-1+total_fishingactivity, ~1, GRD_occ_df)
summary(GRD_occ_model12)

GRD_occ_models <- fitList('psi(.)gam(.)eps(.)p(.)' = GRD_occ_model1,
                          'psi(.)gam(fishingact)eps(fishingact)p(.)' = GRD_occ_model2,
                          'psi(.)gam(season)eps(season)p(.)' = GRD_occ_model4, 
                          'psi(dist_km)gam(.)eps(.)p(.)' = GRD_occ_model5,
                          'psi(dist_km)gam(fishingact)eps(fishingact)p(.)' = GRD_occ_model7,
                          'psi(dist_km)gam(fishingact+fishingboat)eps(fishingact+fishingboat)p(.)' = GRD_occ_model8, 
                          'psi(dist_km)gam(season)eps(season)p(.)' = GRD_occ_model9, 
                          'psi(.)gam(.)eps(.)p(fishingact)' = GRD_occ_model10,
                          'psi(.)gam(.)eps(.)p(fishingboat)' = GRD_occ_model11,     
                          'psi(.)gam(fishingact+season)eps(fishingact+season)p(.)' = GRD_occ_model12)
                  
GRD_occ_models_AIC<-modSel(GRD_occ_models)
GRD_occ_models_AICtable<-GRD_occ_models_AIC@Full[c("model","negLogLike","nPars","n","AIC","delta","AICwt","cumltvWt")]
write.table(GRD_occ_models_AICtable, file = "GRD_occ_AIC_23.08.22.txt", sep = "\t",
            row.names = F)

GRD_occ_top1<-summary(GRD_occ_model4)
GRD_occ_top1_estimate<-rbind(GRD_occ_top1[["psi"]],GRD_occ_top1[["col"]],
                             GRD_occ_top1[["ext"]],GRD_occ_top1[["det"]])
rownames(GRD_occ_top1_estimate)<-c("psi-intercept","col-seasonJan","col-seasonApr",
                                   "ext-seasonJan","ext-seasonApr","det-intercept")
write.table(GRD_occ_top1_estimate, file = "GRD_occ_estimate_top1.txt", sep = "\t",
            row.names = T)

GRD_occ_top2<-summary(GRD_occ_model1)
GRD_occ_top2_estimate<-rbind(GRD_occ_top2[["psi"]],GRD_occ_top2[["col"]],
                             GRD_occ_top2[["ext"]],GRD_occ_top2[["det"]])
rownames(GRD_occ_top2_estimate)<-c("psi-intercept","col-intercept",
                                   "ext-intercept","det-intercept")
write.table(GRD_occ_top2_estimate, file = "GRD_occ_estimate_top2.txt", sep = "\t",
            row.names = T)
############------------------------------------------
##Occupancy models for gharials
gharial<-read.csv("Gharial_Occu_Gandak.csv")
summary(gharial)
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
