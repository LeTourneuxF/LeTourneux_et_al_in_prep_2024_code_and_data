
#This script generates figures for Appendix S4 of LeTourneux et al. 2024 

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(cowplot)
library(scales)

#useful function for 'not in'
'%!in%' <- function(x,y)!('%in%'(x,y))

setwd("~/Doc/MSs/MS_seasonal_survival/soumission_Ecology/script_and_data")

#Generate vector of occasions for each season
summer<-seq(1,119,4)
fall<-seq(2,119,4)
winter<-seq(3,119,4)
spring<-seq(4,119,4)


# Load and process encounter probabilites ####

#Load all encounters from most parcimonious model
Encounters<-read_csv2("M10_this_study.csv", col_types = 'ciiiiiidddd')%>%
  dplyr::rename(LCI='CI-', UCI='CI+')%>%                      #rename columns
  filter(Parameters %in% c('E','E2'))%>%                      #Keep only encounter estimates                             
  mutate(season=ifelse(Time%in%summer,'Summer',
                ifelse(Time%in%fall,'Fall',
                ifelse(Time %in% winter,'Winter',
                ifelse(Time%in%spring,'Spring','error')))),   #Associate a season to every estimate
         seas.graph=ifelse(Time%in%summer,1,
                    ifelse(Time%in%fall,2,
                    ifelse(Time %in% winter,3,
                    ifelse(Time%in%spring,4,'error')))))      #Associate a number to each season (for reformatting purposes)

# Appendix S4:Figure S1 ####
#Live encounter probabilities - collared birds
(col_obs<-Encounters%>%
  #Select collared birds 
  filter(To==3)%>% 
  #Associate palatable group names 
  mutate(gr=ifelse(From==3,'High','Low'), #Heterogeneity in encounter probability
         graph.seasons=factor(season, levels=c('Summer', 'Fall', 'Winter', 'Spring')))%>% #Season of observation
  #Back-compute years from occasion number 
  mutate(year=ifelse(Time %in% winter, round((Time-2)/4,dig=0)+1990,
              ifelse(Time %in% spring, round((Time-3)/4,dig=0)+1990,
              ifelse(Time %in% summer, round(Time/4,dig=0)+1990,
              ifelse(Time %in% fall, round((Time-1)/4,dig=0)+1990,0)))))%>%
  
  #Plot figure
  ggplot(aes(y=Estimates,x=year, col=gr))+
  geom_point(size=2)+
  geom_line()+
  geom_errorbar(aes(x=year, y=Estimates, ymin= LCI, ymax=UCI, width=0.5))+
  scale_y_continuous(limits=c(0,1), name = 'Encounter probability')+
  scale_x_continuous(name='')+
  scale_color_discrete(name = 'Observability \ngroup',type=c("#3e6084",'#7fa0c3'))+
  facet_wrap(~graph.seasons)+
  theme_classic()+
  theme(text = element_text(size=18),
        axis.text = element_text(size=16),
        panel.spacing=unit(20,'pt'),
        panel.grid.major = element_line(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border=element_rect(colour="black",size=1, fill=F)))

# Appendix S4:Figure S2 ####
#Live encounter probabilities - banded-only birds
(band_obs<-Encounters%>%
   #Select banded birds 
   filter(To==2)%>% 
   #Associate palatable group names 
   mutate(group=ifelse(From==1,'High','Low'), #Heterogeneity in encounter probability
          sex=ifelse(Group==1,'Males','Females'))%>% #Sex
   #Back-compute years from occasion number 
   mutate(year=ifelse(Time %in% winter, round((Time-2)/4,dig=0)+1990,
               ifelse(Time %in% spring, round((Time-3)/4,dig=0)+1990,
               ifelse(Time %in% summer, round(Time/4,dig=0)+1990,
               ifelse(Time %in% fall, round((Time-1)/4,dig=0)+1990,0)))))%>%
   
   #Plot figure
   ggplot(aes(y=Estimates,x=year, col=group, shape=sex))+
   geom_point(size=3)+
   geom_line()+
   geom_errorbar(aes(x=year, y=Estimates, ymin= LCI, ymax=UCI, width=0.5))+
   scale_y_continuous(limits=c(0,0.25), name= 'Encounter probability')+
   scale_x_continuous(name='')+
   scale_shape_discrete(name='Sex')+
   scale_color_discrete('Observability \ngroup',type=c("#f27d2b",'#f2af2b'))+
   theme_classic()+
   theme(text = element_text(size=18),
         axis.text = element_text(size=16),
         panel.spacing=unit(20,'pt'),
         panel.grid.major = element_line(),
         strip.background = element_blank(),
         strip.text = element_text(size = 20),
         legend.text = element_text(size = 16),
         panel.border=element_rect(colour="black",size=1, fill=F)))


# Appendix S4:Figure S3 ####

#Dead encounter probabilities
### Compute annual recovery probabilities for banded birds (using band effect on recovery prob) 
#Isolate band effect on recovery probability
band.effect.rec<-Encounters%>%
  #Keep estimates from 2nd step - effect of bands on recovery
  filter(Parameters=='E2')%>%
  #Effect changes with hunting regulation period
  mutate(p.effect=ifelse(Time<36,1,
                  ifelse(Time %in% c(36:74),2,
                  ifelse(Time>74,3,999))))%>%
  dplyr::select(b.effect=Estimates,p.effect)

#Isolate recovery probabilities
recov.band<-Encounters%>%
  #Year-dependent recovery probabilities for banded birds (must be corrected for 'band' effect)
  filter(To==4,From==19)%>%
  mutate(p.effect=ifelse(Time<36,1,
                  ifelse(Time%in%c(36:74),2,
                  ifelse(Time>74,3,999))))%>%
  #Joint with band effect ofr each period
  left_join(band.effect.rec, by='p.effect')%>%
  #Multiply probability by band effect to obtain annual recovery probabilities for banded birds
  mutate(Estimates=Estimates*b.effect,
         LCI=LCI*b.effect,
         UCI=UCI*b.effect)
  
#Build table with recoveries for bands and collars
(recov.mod<-Encounters%>%
  filter(Parameters=='E',
         To==4)%>%
  mutate(From=20)%>%
  bind_rows(recov.band)%>%
  #Correct season (recoveries are coded at next occasion, so must 'backtrack' one season)
  mutate(rl_seas=ifelse(season=='Summer','Spring',
                 ifelse(season=='Spring','Winter',
                 ifelse(season=='Winter','Fall','error'))))%>%
  filter(Time<119)%>%
  #Back-compute years from occasion number 
  mutate(year=ifelse(Time %in% winter, round((Time-2)/4,dig=0)+1990,
  ifelse(Time %in% spring, round((Time-3)/4,dig=0)+1990,
  ifelse(Time %in% summer, round(Time/4,dig=0)+1990,0))))%>%
  
  #Make seasons a factor to plot panels properly  
  mutate(graph.seasons=factor(rl_seas, levels=c('Fall', 'Winter', 'Spring')))%>%
    
  #Plot figure
  ggplot(aes(y=Estimates,x=year, col=factor(From)))+
  geom_errorbar(aes(x=year, ymin=LCI, ymax=UCI), width=0.5)+
  geom_point(size=2)+
  geom_line()+
  scale_y_continuous(limits=c(0,1), name='Recovery probability')+
  scale_x_continuous(limits=c(1989,2020), name='')+
  scale_color_discrete(labels=c('Rings','Collars'),name='Marker', type = c('#F28E2B',"#4E79A7"))+
  ggtitle(label='')+
  theme_classic()+
  facet_wrap(~graph.seasons)+
  theme(text = element_text(size=18),
        axis.text = element_text(size=16),
        panel.spacing=unit(20,'pt'),
        panel.grid.major = element_line(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border=element_rect(colour="black",size=1, fill=F)))




# Appendix S4:Figure S4 ####
#Load and process survival estimates

#Create table with occasion number for each season for each year
timing<-matrix(nrow = 30, ncol=4, data=c(NA,1:119), byrow=T)
timing<-as.data.frame(timing)
colnames(timing)<-c('springs','summers','falls','winters')
rownames(timing)<-1990:2019

#Create dataframe with unequal time intervals for each season
season<-c('fall','winter','spring','summer')
interval<-c(0.822,1.205,1.14,0.833)
UTI<-data.frame(season=character(4),interval=double(4))
UTI$season<-season
UTI$interval<-interval

#Load model results
#Load results - survival estimates per management actions period (M10)
S.s.p<-read_csv2("M10_this_study.csv", col_types = 'ciiiiiidddd')%>%
  dplyr::rename(LCI='CI-', UCI='CI+')%>%                            #rename columns
  mutate(model='M10')%>%
  filter(Parameters == 'S')%>%                                      #Keep only survival estimates
  mutate(Groups=ifelse(Time<36,'all birds',
                       ifelse(From==1,'Bands',
                              ifelse(From==3 , 'Collars','ERROR'))))%>%           #Associate palatable group names based on output information
  mutate(season=ifelse(Time%in%summer,'summer',
                       ifelse(Time%in%fall,'fall',
                              ifelse(Time %in% winter,'winter',
                                     ifelse(Time%in%spring,'spring','error')))))%>%     #Associate a season to every estimate
  left_join(UTI,by='season')%>%                                    #Join with unequal interval dataframe
  mutate(est_real=Estimates^interval,
         LCI_real=LCI^interval,
         UCI_real=UCI^interval)%>%                                  #Compute true survival over each season
  mutate(period=ifelse(Time %in% c(1:35), 1,
                       ifelse(Time %in% c(36:74),2,
                              ifelse(Time >74, 3,999))))%>%                       #Associate management action period to each estimate 1: before 1999 and changes in hunting regulations, 2: 1999-2008: special measures in Canada only; 3: 2009-2018: Special mesures in Canada and the USA 
  mutate(year=ifelse(period==1,1990,
                     ifelse(period==2,1998,
                            ifelse(period==3,2008,30000))))                       #Associate first year of period to each estimate for later reformatting

#Load results - survival estimates per season per year (M6); Similar steps as previous results file
S.s<-read_csv2("M6_this_study.csv", col_types = 'ciiiiiidddd')%>%
  dplyr::rename(LCI='CI-', UCI='CI+')%>%
  mutate(model="M6")%>%
  filter(Parameters == 'S')%>%
  mutate(Groups=ifelse(From==1,'Bands',
                       ifelse(From==3 , 'Collars','ERROR')))%>%
  mutate(season=ifelse(Time%in%summer,'summer',
                       ifelse(Time%in%fall,'fall',
                              ifelse(Time %in% winter,'winter',
                                     ifelse(Time%in%spring,'spring','error')))))%>%
  left_join(UTI,by='season')%>%
  mutate(est_real=Estimates^interval,
         LCI_real=LCI^interval,
         UCI_real=UCI^interval)

# Assign year to seasonal estimates from model output
S.s.df<-as.data.frame(S.s)
S.s$year<-NA
for(i in 1:nrow(S.s.df)){
  t.i<-S.s.df[i,'Time']
  if(t.i %in% timing$summers){
    S.s[i,'year']<-rownames(timing[which(timing$summers==t.i),])
  }else if(t.i %in% timing$falls){
    S.s[i,'year']<-rownames(timing[which(timing$falls==t.i),])
  }else if(t.i %in% timing$winters){
    S.s[i,'year']<-rownames(timing[which(timing$winters==t.i),])
  }else if(t.i %in% timing$springs){
    S.s[i,'year']<-rownames(timing[which(timing$springs==t.i),])
  }
}

#Make sure year is numeric
S.s$year<-as.numeric(S.s$year)

#Adapt dataframe to have 1 survival estimate per season per period for plotting (Esurge returns estimate for first of two periods only)
#Plug identical values for second period when seasonal survival is estimated jointly for two  or more periods
winter.dat<-S.s.p[which(S.s.p$Time==3),]
winter.dat$year<-c(1998)
winter.dat$period<-c(2)
winter.dat$Time<-c(35)
summer.dat<-S.s.p[c(which(S.s.p$Time==1),which(S.s.p$Time==1)),]
summer.dat$year<-c(1998,2008)
summer.dat$period<-c(2,3)
summer.dat$Time<-c(37,77)
fall.dat<-S.s.p[c(which(S.s.p$Time==38&S.s.p$From==1)),]
fall.dat$year<-c(2008)
fall.dat$period<-c(3)
fall.dat$Time<-c(78)
S.s.p<-S.s.p%>%
  bind_rows(winter.dat,summer.dat,fall.dat)%>%
  arrange(Time)


S.s.col<-S.s%>%
  mutate(Marker=ifelse(From==1&year<1999,'All birds',
                       ifelse(From==1 & year>1998,'Band',
                              ifelse(From==3,'Collar','ERROR'))))


colors_collar_graph<-c("#D37295","#F28E2B","#4E79A7")
##### Summer plot ####
(survie_ete_col<-ggplot(data = S.s.col%>% 
                          filter(year%!in% c(1990,2019),
                                 season=='summer'),
                        aes(x=year, 
                            y=est_real,
                            color=Marker))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3)+
   geom_line()+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[1], color=NA,
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)))))+
   
   #Shading link p2-p3 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)))))+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[1], lty=2)+
   
   
   #links between p1-p2 band
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p1-p2 collar
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 2 band
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 2 collar
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   #links between p2-p3 bands
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p2-p3 collars
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='summer')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 3 bands
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 3 collars
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='summer')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1999), From==1)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1999), From==1)%>%dplyr::select(est_real))),
     color=colors_collar_graph[2])+
   
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1999), From==3)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='summer',year %in% c(1999), From==3)%>%dplyr::select(est_real))),
     color=colors_collar_graph[3])+
   geom_point(size=1.75)+
   
   
   ggtitle(label='Summer')+
   
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003.5, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.5)+
   annotate(geom = 'text', x=2014.15, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   
   geom_vline(xintercept = c(1998.5, 2008.5), lty=1)+
   scale_color_manual(values=colors_collar_graph)+
   scale_x_continuous(name='', breaks=seq(1991,2018,3))+
   scale_y_continuous(name='Survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=13, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.1),
         legend.direction = 'horizontal',
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag.position = c(0.003,0.98),
         plot.tag = element_text(size=20),
         plot.margin = margin(t=2,r=2,l=0,b=5,unit = 'pt')))

##### Fall plot ####
(survie_automne_col<-ggplot(data = S.s.col%>% 
                              filter(year%!in%c(1990,2019),
                                     season=='fall'),
                            #Groups %!in% 'ERROR'), 
                            #Determine aesthetics (which variables, and what to base shape and color on)
                            aes(x=year, 
                                y=est_real,
                                color=Marker))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3)+
   geom_line()+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[1], color=NA,
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)))))+
   
   #Shading link p2-p3 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)))))+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[1], lty=2)+
   
   
   #links between p1-p2 band
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p1-p2 collar
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 2 band
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 2 collar
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   #links between p2-p3 bands
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p2-p3 collars
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='fall')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 3 bands
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 3 collars
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='fall')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   #And segments between p1 and p2
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1999), From==1)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1999), From==1)%>%dplyr::select(est_real))),
     color=colors_collar_graph[2])+
   
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1999), From==3)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='fall',year %in% c(1999), From==3)%>%dplyr::select(est_real))),
     color=colors_collar_graph[3])+
   
   
   geom_point(size=1.75)+
   
   
   
   ggtitle(label='Fall')+
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003.5, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.5)+
   annotate(geom = 'text', x=2014.15, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   
   geom_vline(xintercept = 1998.5, lty=1)+
   geom_vline(xintercept = 2008.5, lty=1)+
   
   scale_color_manual(values=colors_collar_graph)+
   scale_x_continuous(name='', breaks=seq(1991,2018,3))+
   scale_y_continuous(name='Survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=13, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.1),
         legend.direction = 'horizontal',
         plot.tag.position = c(0.003,0.98),
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag = element_text(size=20),
         plot.margin = margin(t=2,r=2,l=0,b=5,unit = 'pt')))

##### Winter plot ####
(survie_hiver_col<-ggplot(data = S.s.col%>% 
                            filter(year%!in% c(1990,2018),
                                   season=='winter'),
                          #Groups %!in% 'ERROR'), 
                          #Determine aesthetics (which variables, and what to base shape and color on)
                          aes(x=year, 
                              y=est_real,
                              color=Marker))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3)+
   geom_line()+
   
   #Shading average p1
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[1], color=NA,
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1999,2007,2007,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1999,2007,2007,1999),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2007,2008,2008,2007),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)))))+
   
   #Shading link p2-p3 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2007,2008,2008,2007),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2008,2018,2018,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)))))+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2008,2018,2018,2008),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[1], lty=2)+
   
   
   #links between p1-p2 band
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p1-p2 collar
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 2 band
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real)),
         xend = 2007, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 2 collar
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(est_real)),
         xend = 2007, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   #links between p2-p3 bands
   geom_segment(
     aes(x = 2007,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p2-p3 collars
   geom_segment(
     aes(x = 2007,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='winter')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 3 bands
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 3 collars
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='winter')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   
   
   #Then determine that we want points on top
   geom_point(size=1.75)+
   
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1999), From==1)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1999), From==1)%>%dplyr::select(est_real))),
     color=colors_collar_graph[2])+
   
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1999), From==3)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='winter',year %in% c(1999), From==3)%>%dplyr::select(est_real))),
     color=colors_collar_graph[3])+
   geom_point(size=1.75)+
   
   ggtitle(label='Winter')+
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.3)+
   annotate(geom = 'text', x=2013, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   
   geom_vline(xintercept = 1998.5, lty=1)+
   geom_vline(xintercept = 2007.5, lty=1)+
   
   scale_color_manual(values=colors_collar_graph)+
   scale_x_continuous(name='', breaks=seq(1991,2017,3))+
   scale_y_continuous(name='Survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=13, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.1),
         legend.direction = 'horizontal',
         plot.tag.position = c(0.003,0.98),
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag = element_text(size=20),
         plot.margin = margin(t=8,r=2,l=0,b=0,unit = 'pt')))

##### Spring plot####
(survie_printemps_col<-ggplot(data = S.s.col%>% 
                                filter(year%!in%c(1990,2019),
                                       season=='spring'),
                              #Groups %!in% 'ERROR'), 
                              #Determine aesthetics (which variables, and what to base shape and color on)
                              aes(x=year, 
                                  y=est_real,
                                  color=Marker))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3)+
   geom_line()+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[1], color=NA,
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)))))+
   #Shading average p2 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3 band
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)))))+
   
   #Shading link p2-p3 collar
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[2], color=NA,
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)))))+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=colors_collar_graph[3], color=NA,
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[1], lty=2)+
   
   
   #links between p1-p2 band
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p1-p2 collar
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 2 band
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 2 collar
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   #links between p2-p3 bands
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   #links between p2-p3 collars
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(1998), season=='spring')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   ##Period 3 bands
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[2], lty=2)+
   
   ##Period 3 collars
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==3,year %in% c(2008), season=='spring')%>%dplyr::select(est_real))),
     color=colors_collar_graph[3], lty=2)+
   
   
   #Then determine that we want points and lines on top
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1999), From==1)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1999), From==1)%>%dplyr::select(est_real))),
     color=colors_collar_graph[2])+
   
   geom_segment(
     aes(x = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1998))%>%dplyr::select(year)),
         y = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1998))%>%dplyr::select(est_real)),
         xend = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1999), From==3)%>%dplyr::select(year)),
         yend = as.numeric(S.s.col%>%filter(season=='spring',year %in% c(1999), From==3)%>%dplyr::select(est_real))),
     color=colors_collar_graph[3])+
   
   geom_point(size=1.75)+
   
   ggtitle(label='Spring')+
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003.5, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.5)+
   annotate(geom = 'text', x=2014.15, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   
   geom_vline(xintercept = 1998.5, lty=1)+
   geom_vline(xintercept = 2008.5, lty=1)+
   
   scale_color_manual(values=colors_collar_graph)+
   scale_x_continuous(name='', breaks=seq(1991,2018,3))+
   scale_y_continuous(name='Survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=13, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.1),
         legend.direction = 'horizontal',
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag.position = c(0.003,0.98),
         plot.tag = element_text(size=20),
         plot.margin = margin(t=8,r=2,l=0,b=0,unit = 'pt')))

##### Final figure ####
(F_saisons_col<-plot_grid(survie_ete_col, survie_automne_col, survie_hiver_col, survie_printemps_col, nrow = 2))


# Appendix S4:Figure S5 ####
#Load and process survival estimates
### Annual survival estimates from LeTourneux et al. 2022 Journal of Applied Ecology ####
# Survival by hunting period
result<-read_csv2('M17_LeTourneux_et_al_2022.csv', col_types = 'ciiiiiidddd')%>%
  dplyr::rename(LCI='CI-', UCI='CI+')%>%
  mutate(Time=Time+1989)

Sp<-result%>%
  filter(Parameters == 'S')%>%
  mutate(Period=ifelse(Time == 1990,1,
                       ifelse(Time==1998,2,
                              ifelse(Time == 2008 , 3,0))))



# Survival by year
result<-read_csv2('M14_LeTourneux_et_al_2022.csv', col_types = 'ciiiiiidddd')%>%
  dplyr::rename(LCI='CI-', UCI='CI+')%>%
  mutate(Time=Time+1989)

S<-result%>%
  filter(Parameters == 'S')%>%
  mutate(Period=ifelse(Time == 1990,'1990-1998',
                       ifelse(Time==1998, '1999-2008',
                              ifelse(Time==2008 , '2009-2016','ERROR'))))


### Seasonal survival estimates (this study) and other parameters ####
#Load survival estimates from output file (on logit scale: Betas)
S.s.betas<-read_csv2('M6_this_study_betas.csv')%>%
  mutate(season=ifelse(str_detect(beta,'ete'),'summer',
                       ifelse(str_detect(beta,'fall'),'fall',
                              ifelse(str_detect(beta,'wint'),'winter',
                                     ifelse(str_detect(beta,'spr'),'spring',NA)))))%>%
  filter(!is.na(t))%>%
  dplyr::select(Beta=Value,LCI=`Cl-`, UCI=`Cl+`,SE,year=t, season)

## Reconstruct annual survival probability from seasonal survival estimates ####
# Extract parameter values on logit scale
library(logitnorm)

beta.spr<-S.s.betas%>%
  mutate(year=year-1)%>%
  filter(season=='spring')

beta.wtr<-S.s.betas%>%
  filter(season=='winter')

beta.fall<-S.s.betas%>%
  filter(season=='fall')

beta.summer<-S.s.betas%>%
  filter(season=='summer')


#We will simulate distributions based on individual betas and their associated errors,
#This will allow us to propagate uncertainty of seasonal estimates to annual estimates
n.sims=5000
n.years=length(unique(beta.spr$year))
sim.spring<-matrix(ncol=n.sims,nrow=nrow(beta.spr))
sim.winter<-matrix(ncol=n.sims,nrow=nrow(beta.wtr))
sim.fall<-matrix(ncol=n.sims,nrow=nrow(beta.fall))
sim.summer<-matrix(ncol=n.sims,nrow=nrow(beta.summer))
sim.annual<-matrix(ncol=n.sims,nrow=n.years)

for(i in 1:n.years){
  #For each year,
  #Draw 5000 samples from logitnormal distribution with mu= mean beta and sigma= SE of beta
  sim.spring[i,]<-rlogitnorm(n=n.sims,mu=beta.spr$Beta[i], sigma = beta.spr$SE[i])
  sim.winter[i,]<-rlogitnorm(n=n.sims,mu=beta.wtr$Beta[i], sigma = beta.wtr$SE[i])
  sim.fall[i,]  <-rlogitnorm(n=n.sims,mu=beta.fall$Beta[i], sigma = beta.fall$SE[i])
  sim.summer[i,]<-rlogitnorm(n=n.sims,mu=beta.summer$Beta[i], sigma = beta.summer$SE[i])
  
  #Multiply draws from each season to reconstruct 5000 values of simulated annual survival (after having raised each seasonal estimate to relative length of interval)
  sim.annual[i,]<-(sim.spring[i,]^1.14)*(sim.winter[i,]^1.205)*(sim.fall[i,]^0.822)*(sim.summer[i,]^0.833)
}

#Compute uncertainty around annual estimates
SE.annual<-apply(sim.annual, 1, function(x){sd(x)})
UCI.sim<-apply(sim.annual,1,FUN=function(x){sort(x)[0.975*length(x)]})
LCI.sim<-apply(sim.annual,1,FUN=function(x){sort(x)[0.025*length(x)]})
mean.sim<-data.frame(Time=c(1990:2018),Estimates=rowMeans(sim.annual),LCI=LCI.sim, UCI=UCI.sim, method='Seasonal')

data.graph<-S%>%
  mutate(method='Annual')%>%
  filter(From==1)%>%
  dplyr::select(Time,Estimates,LCI,UCI,method)%>%
  bind_rows(mean.sim)

#Repeat same process for average survival per hunting period
#Load survival estimates from output file (on logit scale: Betas)
S.s.p.betas<-read_csv2('M5_this_study_betas.csv')%>%
  filter(str_detect(beta,'S_'))%>%
  mutate(season=ifelse(str_detect(beta,'ete'),'summer',
                       ifelse(str_detect(beta,'aut'),'fall',
                              ifelse(str_detect(beta,'hiv'),'winter',
                                     ifelse(str_detect(beta,'prin'),'spring',NA)))))%>%
  mutate(period=ifelse(str_detect(beta,'d1'),1,
                                  ifelse(str_detect(beta,'d2'),2,3)))%>%
  dplyr::select(Beta=Value,LCI=`Cl-`, UCI=`Cl+`,SE,period, season)

## Reconstruct annual survival probability from seasonal survival estimates ####
# Extract parameter values on logit scale
library(logitnorm)

beta.p.spr<-S.s.p.betas%>%
  filter(season=='spring')

beta.p.wtr<-S.s.p.betas%>%
  filter(season=='winter')

beta.p.fall<-S.s.p.betas%>%
  filter(season=='fall')

beta.p.summer<-S.s.p.betas%>%
  filter(season=='summer')


#We will simulate distributions based on individual betas and their associated errors,
#This will allow us to propagate uncertainty of seasonal estimates to annual estimates
n.sims=5000
n.periods=length(unique(beta.p.spr$period))
sim.p.spring<-matrix(ncol=n.sims,nrow=nrow(beta.p.spr))
sim.p.winter<-matrix(ncol=n.sims,nrow=nrow(beta.p.wtr))
sim.p.fall<-matrix(ncol=n.sims,nrow=nrow(beta.p.fall))
sim.p.summer<-matrix(ncol=n.sims,nrow=nrow(beta.p.summer))
sim.p.annual<-matrix(ncol=n.sims,nrow=n.periods)

for(i in 1:n.periods){
  #For each year,
  #Draw 5000 samples from logitnormal distribution with mu= mean beta and sigma= SE of beta
  sim.p.spring[i,]<-rlogitnorm(n=n.sims,mu=beta.p.spr$Beta[i],    sigma = beta.p.spr$SE[i])
  sim.p.winter[i,]<-rlogitnorm(n=n.sims,mu=beta.p.wtr$Beta[i],    sigma = beta.p.wtr$SE[i])
  sim.p.fall[i,]  <-rlogitnorm(n=n.sims,mu=beta.p.fall$Beta[i],   sigma = beta.p.fall$SE[i])
  sim.p.summer[i,]<-rlogitnorm(n=n.sims,mu=beta.p.summer$Beta[i], sigma = beta.p.summer$SE[i])
  
  #Multiply draws from each season to reconstruct 5000 values of simulated annual survival (after having raised each seasonal estimate to relative length of interval)
  sim.p.annual[i,]<-(sim.p.spring[i,]^1.14)*(sim.p.winter[i,]^1.205)*(sim.p.fall[i,]^0.822)*(sim.p.summer[i,]^0.833)
}

#Compute uncertainty around annual estimates
SE.p.annual<-apply(sim.p.annual, 1, function(x){sd(x)})
UCI.p.sim<-apply(sim.p.annual,1,FUN=function(x){sort(x)[0.975*length(x)]})
LCI.p.sim<-apply(sim.p.annual,1,FUN=function(x){sort(x)[0.025*length(x)]})
mean.p.sim<-data.frame(Period=c(1,2,3),Estimates=rowMeans(sim.p.annual),LCI=LCI.p.sim, UCI=UCI.p.sim, method='Seasonal')


#Bind dataframes with survival estimations from both methods
data.graph.p<-Sp%>%
  mutate(method='Annual')%>%
  filter(From==1)%>%
  dplyr::select(Period,Estimates,LCI,UCI,method)%>%
  bind_rows(mean.p.sim)

#Plot graph

col.annual<-'#CC0000'
# Create figure: Appendix 5: Figure 4 
(FigS4.4<-ggplot(data = data.graph%>% 
                       filter(Time<2017),
                     
                     aes(x=Time, y=Estimates, col=method))+
    
   geom_point(size=1.75)+
   geom_line()+
   geom_errorbar(aes(x=Time, ymin=LCI, ymax=UCI), width=0.3)+
   #Polygons for CI around period mean
   #Shading average p1
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=col.annual,col=NA,
                aes(x=c(1989.5,1997,1997,1989.5),y=c(data.graph.p%>%filter(Period==1, method=='Annual')%>%pull(UCI),
                                                     data.graph.p%>%filter(Period==1, method=='Annual')%>%pull(UCI),
                                                     data.graph.p%>%filter(Period==1, method=='Annual')%>%pull(LCI),
                                                     data.graph.p%>%filter(Period==1, method=='Annual')%>%pull(LCI))))+
   #Shading link p1-p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=col.annual,col=NA,
                aes(x=c(1997,1998,1998,1997),y=c(data.graph.p%>%filter(Period==1, method=='Annual')%>%pull(UCI),
                                                 data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(UCI),
                                                 data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(LCI),
                                                 data.graph.p%>%filter(Period==1, method=='Annual')%>%pull(LCI))))+
   #Shading average p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=col.annual,col=NA,
                aes(x=c(1998,2007,2007,1998),y=c(data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(UCI),
                                                 data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(UCI),
                                                 data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(LCI),
                                                 data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(LCI))))+
   #Shading link p2-p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=col.annual,col=NA,
                aes(x=c(2007,2008,2008,2007),y=c(data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(UCI),
                                                 data.graph.p%>%filter(Period==3, method=='Annual')%>%pull(UCI),
                                                 data.graph.p%>%filter(Period==3, method=='Annual')%>%pull(LCI),
                                                 data.graph.p%>%filter(Period==2, method=='Annual')%>%pull(LCI))))+

   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill=col.annual,col=NA,
                aes(x=c(2008,2017.5,2017.5,2008),y=c(data.graph.p%>%filter(Period==3, method=='Annual')%>%pull(UCI),
                                                     data.graph.p%>%filter(Period==3, method=='Annual')%>%pull(UCI),
                                                     data.graph.p%>%filter(Period==3, method=='Annual')%>%pull(LCI),
                                                     data.graph.p%>%filter(Period==3, method=='Annual')%>%pull(LCI))))+

 
    #Shading average p1
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#4E79A7',col=NA,
                 aes(x=c(1989.5,1997,1997,1989.5),y=c(data.graph.p%>%filter(Period==1, method=='Seasonal')%>%pull(UCI),
                                                      data.graph.p%>%filter(Period==1, method=='Seasonal')%>%pull(UCI),
                                                      data.graph.p%>%filter(Period==1, method=='Seasonal')%>%pull(LCI),
                                                      data.graph.p%>%filter(Period==1, method=='Seasonal')%>%pull(LCI))))+
    #Shading link p1-p2
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#4E79A7',col=NA,
                 aes(x=c(1997,1998,1998,1997),y=c(data.graph.p%>%filter(Period==1, method=='Seasonal')%>%pull(UCI),
                                                  data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(LCI),
                                                  data.graph.p%>%filter(Period==1, method=='Seasonal')%>%pull(LCI))))+
    #Shading average p2
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#4E79A7',col=NA,
                 aes(x=c(1998,2007,2007,1998),y=c(data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(LCI),
                                                  data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(LCI))))+
    #Shading link p2-p3
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#4E79A7',col=NA,
                 aes(x=c(2007,2008,2008,2007),y=c(data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  data.graph.p%>%filter(Period==3, method=='Seasonal')%>%pull(UCI),
                                                  data.graph.p%>%filter(Period==3, method=='Seasonal')%>%pull(LCI),
                                                  data.graph.p%>%filter(Period==2, method=='Seasonal')%>%pull(LCI))))+
    
    #Shading average p3
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#4E79A7',col=NA,
                 aes(x=c(2008,2017.5,2017.5,2008),y=c(data.graph.p%>%filter(Period==3, method=='Seasonal')%>%pull(UCI),
                                                      data.graph.p%>%filter(Period==3, method=='Seasonal')%>%pull(UCI),
                                                      data.graph.p%>%filter(Period==3, method=='Seasonal')%>%pull(LCI),
                                                      data.graph.p%>%filter(Period==3, method=='Seasonal')%>%pull(LCI))))+
   

  geom_line()+
    geom_errorbar(aes(x=Time, ymin=LCI, ymax=UCI), width=0.3)+
    geom_point(size=1.75)+
    
    scale_color_manual(values=c(col.annual,'#4E79A7'),name='Dataset')+
  
  annotate(geom = 'text', x=1993, y=0.35, label = 'Historical hunting\nregulations')+
    annotate(geom = 'text', x=2002.5, y=0.35, label = 'Special hunting\nregulations in\nCanada only' )+
    annotate(geom = 'text', x=2012.25, y=0.35, label = 'Special hunting\nregulations in\nCanada and the USA' )+
    
    geom_vline(xintercept = 1997.5, lty=1)+
    geom_vline(xintercept = 2007.5, lty=1)+
    #scale_color_manual(values=c("#4E79A7"))+
    scale_x_continuous(name='', breaks=seq(1990,2017,3))+
    scale_y_continuous(name='Survival probability', limits=c(0,1))+
    coord_cartesian(ylim=c(0.25,1))+
    theme_classic()+
    theme(axis.text=element_text(size=12, colour="black"),
          axis.title=element_text(size=13, colour="black", face="bold"),
          text = element_text(size=12, colour="black"),
          legend.text=element_text(size=10, colour="black"),
          legend.title=element_text(size=13, colour="black")))





