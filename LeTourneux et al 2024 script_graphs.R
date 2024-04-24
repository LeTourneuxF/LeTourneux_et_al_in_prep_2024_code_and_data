
library(tidyverse)
library(ggplot2)
library(cowplot)
library(scales)


setwd("~/script_and_data")

#useful function for 'not in'
'%!in%' <- function(x,y)!('%in%'(x,y))


# Figure 2 main text ####
# Load and process results
#Create table with occasion number for each season for each year
timing<-matrix(nrow = 30, ncol=4, data=c(NA,1:119), byrow=T)
timing<-as.data.frame(timing)
colnames(timing)<-c('springs','summers','falls','winters')
rownames(timing)<-1990:2019

#Occasions for each season
summer<-seq(1,119,4)
fall<-seq(2,119,4)
winter<-seq(3,119,4)
spring<-seq(4,119,4)

#Create dataframe with unequal time intervals for each season
season<-c('fall','winter','spring','summer')
interval<-c(0.822,1.205,1.14,0.833)
UTI<-data.frame(season=character(4),interval=double(4))
UTI$season<-season
UTI$interval<-interval

## Load Seasonal survival estimates ####
# Load survival estimates per management actions period (M10)
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

#Load survival estimates per season per year (M6); Similar steps as previous results file
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


# Assign year to seasonal estimates
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

#Adapt dataframe to have 1 survival estimate per season per period for plotting (Esurge returns estimate for first of two ore more periods only)
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


## Plot seasonal survival estimates ####
### Summer plot
(survie_ete<-ggplot(data = S.s%>% 
                      filter(year%!in% c(1990,2019),
                             season=='summer',
                             From==1),
                    aes(x=year, 
                        y=est_real))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3, col="#4E79A7")+
   geom_line(col="#4E79A7")+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(LCI_real)))))+
   # #Shading average p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   
   #links between p1-p2
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='summer')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   ##Period 2
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   #links between p2-p3
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='summer')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   ##Period 3
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='summer')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   #Then determine that we want points on top
   geom_point(size=1.75, col="#4E79A7")+
   #geom_label(aes(x=1993, y=0.7, label = "Text Over Line"), fill = "yellow",label.r = unit(0,units = 'pt'))+
   #annotate(geom = 'rect', xmin = 1990.5, xmax=1995.5, ymin=0.675, ymax=0.72, fill='white' )+
   
   ggtitle(label='Summer')+
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003.5, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.5)+
   annotate(geom = 'text', x=2014.15, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   geom_vline(xintercept = c(1998.5, 2008.5), lty=1)+
   #scale_color_manual(values=c("#4E79A7"))+
   scale_x_continuous(name='', breaks=seq(1991,2018,3))+
   scale_y_continuous(name='Seasonal survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=15, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.07),
         legend.direction = 'horizontal',
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag.position = c(0.003,0.98),
         plot.tag = element_text(size=20)))

### Fall plot
(survie_automne<-ggplot(data = S.s%>% 
                          filter(year%!in%c(1990,2019),
                                 From == 1, 
                                 season=='fall'),
                        #Groups %!in% 'ERROR'), 
                        #Determine aesthetics (which variables, and what to base shape and color on)
                        aes(x=year, 
                            y=est_real))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3, col="#4E79A7")+
   geom_line(col="#4E79A7")+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(LCI_real)))))+
   # #Shading average p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   
   #links between p1-p2
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='fall')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   ##Period 2
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   #links between p2-p3
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='fall')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   ##Period 3
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='fall')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   #Then determine that we want points on top
   geom_point(size=1.75, col="#4E79A7")+
   
   ggtitle(label='Fall')+
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003.5, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.5)+
   annotate(geom = 'text', x=2014.15, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   geom_vline(xintercept = 1998.5, lty=1)+
   geom_vline(xintercept = 2008.5, lty=1)+
   
   #scale_color_manual(values=c("#4E79A7"))+
   scale_x_continuous(name='', breaks=seq(1991,2018,3))+
   scale_y_continuous(name='Seasonal survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=15, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.07),
         legend.direction = 'horizontal',
         plot.tag.position = c(0.003,0.98),
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag = element_text(size=20)))

### Winter plot
(survie_hiver<-ggplot(data = S.s%>% 
                        filter(year%!in% c(1990,2018),
                               From == 1,
                               season=='winter'),
                      #Groups %!in% 'ERROR'), 
                      #Determine aesthetics (which variables, and what to base shape and color on)
                      aes(x=year, 
                          y=est_real))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3, col="#4E79A7")+
   geom_line(col="#4E79A7")+
   
   #Shading average p1
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(LCI_real)))))+
   # #Shading average p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1999,2007,2007,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2007,2008,2008,2007),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2008,2017,2017,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   
   #links between p1-p2
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='winter')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   ##Period 2
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real)),
         xend = 2007, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   #links between p2-p3
   geom_segment(
     aes(x = 2007,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='winter')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   ##Period 3
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(est_real)),
         xend = 2017, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='winter')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   #Then determine that we want points on top
   geom_point(size=1.75, col="#4E79A7")+
   
   ggtitle(label='Winter')+
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.3)+
   annotate(geom = 'text', x=2013, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   geom_vline(xintercept = 1998.5, lty=1)+
   geom_vline(xintercept = 2007.5, lty=1)+
   
   #scale_color_manual(values=c("#4E79A7"))+
   scale_x_continuous(name='', breaks=seq(1991,2017,3))+
   scale_y_continuous(name='Seasonal survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=15, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.07),
         legend.direction = 'horizontal',
         plot.tag.position = c(0.003,0.98),
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag = element_text(size=20)))


### Spring plot
(survie_printemps<-ggplot(data = S.s%>% 
                            filter(year%!in%c(1990,2019),
                                   From == 1,
                                   season=='spring'),
                          #Groups %!in% 'ERROR'), 
                          #Determine aesthetics (which variables, and what to base shape and color on)
                          aes(x=year, 
                              y=est_real))+
   geom_errorbar(aes(x=year, ymin=LCI_real, ymax=UCI_real), width=0.3, col="#4E79A7")+
   geom_line(col="#4E79A7")+
   
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1991,1998,1998,1991),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(LCI_real)))))+
   #Shading link p1-p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1998,1999,1999,1998),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(LCI_real)))))+
   # #Shading average p2
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(1999,2008,2008,1999),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)))))+
   #Shading link p2-p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2008,2009,2009,2008),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(LCI_real)))))+
   
   #Shading average p3
   geom_polygon(data=data.frame(c(1,2,3,4)),
                alpha=0.2, fill='#F28E2B',
                aes(x=c(2009,2018,2018,2009),y=c(as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(UCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)),
                                                 as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(LCI_real)))))+
   
   #Period 1
   geom_segment(
     aes(x = 1991,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>% dplyr::select(est_real)),
         xend = 1998, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   
   #links between p1-p2
   geom_segment(
     aes(x = 1998,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1990), season=='spring')%>%dplyr::select(est_real)),
         xend = 1999, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   ##Period 2
   geom_segment(
     aes(x = 1999,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real)),
         xend = 2008, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   
   #links between p2-p3
   geom_segment(
     aes(x = 2008,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(1998), season=='spring')%>%dplyr::select(est_real)),
         xend = 2009, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   ##Period 3
   geom_segment(
     aes(x = 2009,    y = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(est_real)),
         xend = 2018, yend = as.numeric(S.s.p%>%filter(From==1,year %in% c(2008), season=='spring')%>%dplyr::select(est_real))),
     color='#F28E2B', lty=2)+
   #Then determine that we want points and lines on top
   geom_point(size=1.75, col="#4E79A7")+
   
   ggtitle(label='Spring')+
   annotate(geom = 'text', x=1994.25, y=0.65, label = 'Historical\nhunting\nregulations', size=5.5)+
   annotate(geom = 'text', x=2003.5, y=0.65, label = 'Special hunting\nregulations in\nCanada only', size=5.5)+
   annotate(geom = 'text', x=2014.15, y=0.65, label = 'Special hunting\nregulations in\nCanada and\nthe USA', size=5.5)+
   
   geom_vline(xintercept = 1998.5, lty=1)+
   geom_vline(xintercept = 2008.5, lty=1)+
   
   #scale_color_manual(values=c("#4E79A7"))+
   scale_x_continuous(name='', breaks=seq(1991,2018,3))+
   scale_y_continuous(name='Seasonal survival probability', limits=c(0,1))+
   coord_cartesian(ylim=c(0.6,1))+
   theme_classic()+
   theme(axis.text=element_text(size=12, colour="black"),
         axis.title=element_text(size=15, colour="black", face="bold"),
         legend.text=element_text(size=12, colour="black"),
         legend.title=element_text(size=13, colour="black"),
         legend.position = c(0.5,1.07),
         legend.direction = 'horizontal',
         plot.title.position='panel',
         plot.title = element_text(size=14,color='black', face='bold',hjust=0.5),
         plot.tag.position = c(0.003,0.98),
         plot.tag = element_text(size=20)))

###Final figure
(Figure2<-plot_grid(survie_ete, survie_automne, survie_hiver, survie_printemps, nrow = 2))

ggsave(plot=Figure2, filename='Fig2.pdf', height=12, width=12)



# Figure 3 main text ####

# ### Get seasonal mortality estimates for all years ####
# Transform survival estimates into mortality to facilitate comparison
S.s.mort<-S.s%>%
  filter(From==1)%>%
  mutate(mortality=1-Estimates,
         LCI_m=1-UCI,
         UCI_m=1-LCI,
         year=ifelse(season=='spring',year-1,year),
         n.geese=2000)%>%
  dplyr::select(year, mortality,  season, UCI_m,LCI_m,n.geese)%>%
  pivot_wider(names_from = season, values_from = c(mortality,LCI_m,UCI_m), values_fill=0)%>%
  mutate(period=ifelse(year %in% c(1999:2007),2,3))


lm.comp<-lm(mortality_spring~mortality_winter, data=S.s.mort%>%filter(year>1998,year<2019))

#Check assumptions, not bad
plot(lm.comp)

#We could just look at the relationship between mean estimates... clearly some compensation in here
summary(lm.comp)

#But we could also incorporate uncertainty in model estimates into this relationship to make things cleaner..
library(logitnorm)

#We will incorporate this error by bootstrapping, i.e., simulating many datasets from the distribution of each parameter's mean estimate and standard error

#First, extract betas to generate randomized samples for bootstrap from distribution of parameter on real scale
S.s.betas<-read_csv2('M6_this_study_betas.csv')%>%
  mutate(season=ifelse(str_detect(beta,'ete'),'summer',
                       ifelse(str_detect(beta,'fall'),'fall',
                              ifelse(str_detect(beta,'wint'),'winter',
                                     ifelse(str_detect(beta,'spr'),'spring',NA)))))%>%
  filter(!is.na(t))%>%
  dplyr::select(Beta=Value,LCI=`Cl-`, UCI=`Cl+`,SE,year=t, season)

#Extract betas of spring survival 
beta.spr<-S.s.betas%>%
  mutate(year=year-1)%>% # remove 1 from year for springs as dataset displays calendar years, and we want to match winter(year) with spring(year + 1)
  filter(season=='spring',
         year>1998,
         year<2019)

#Extract betas of winter survival 
beta.wtr<-S.s.betas%>%
  filter(season=='winter',
         year>1998,
         year<2019)

#We will simulate 5000 datasets, should give us a representative picture
n.sims=5000

#Simulation parameters (here number of years only)
n.years=length(unique(beta.spr$year))

#Matrix to store spring survival estimates (rows are years, columns are simulations)
sim.spring<-matrix(ncol=n.sims,nrow=nrow(beta.spr))
#Matrix to store winter survival estimates (rows are years, columns are simulations)
sim.winter<-matrix(ncol=n.sims,nrow=nrow(beta.wtr))

#Simulate datasets of spring and winter survival (5000x each year)
for(i in 1:n.years){
  
  sim.spring[i,]<-1-rlogitnorm(n=n.sims,mu=beta.spr$Beta[i], sigma = beta.spr$SE[i])
  
  sim.winter[i,]<-1-rlogitnorm(n=n.sims,mu=beta.wtr$Beta[i], sigma = beta.wtr$SE[i])
  
}

#Effect of winter mortality on spring mortality

#Create empty matrices to store relationships between spring and winter mortality for simulated values
#Slope of relationship
slopes_WS<-matrix(ncol=1,nrow=n.sims)
#Intercept of relationship
intercepts_WS<-matrix(ncol=1,nrow=n.sims)
#Range of observed values of winter mortality to make predictions for each simulated relationship
xrange_WS<-seq(from=(range(S.s.mort%>%filter(year>1998,year<2019)%>%dplyr::select(mortality_winter))[1])-0.003, to=(range(S.s.mort%>%filter(year>1998,year<2019)%>%dplyr::select(mortality_winter))[2])+0.003, length.out=20)
#Matrix to store predictions from each simulated relationship
preds_WS<-matrix(nrow=length(xrange_WS), ncol=n.sims)


#Extract the coefficients of the simulated relationships
for(i in 1:n.sims){
  
  coef_sim_WS<-coef(lm(sim.spring[,i]~sim.winter[,i]))
  slopes_WS[i]<-as.numeric(coef_sim_WS[2])
  intercepts_WS[i]<-as.numeric(coef_sim_WS[1])
  
}

#Make predictions over the range of potential mortalities for each relationship
for(i in 1:n.sims){
  
  preds_WS[,i]<-intercepts_WS[i]+slopes_WS[i]*xrange_WS
  
}


#Extract metrics from simulated relationships
#Central 95% values of relationships
CI_WS<-sort(slopes_WS)[c((0.025*n.sims),(0.975*n.sims))]
#Mean slope from simulated relationships
mean_slope_WS<-mean(slopes_WS)
#Select central 95% of slopes distribution to plot to figure as 95%CI
select_slopes_WS<-which(slopes_WS>CI_WS[1]&slopes_WS<CI_WS[2])
#Mean intercept from simulated relationships
mean_intercept_WS<-mean(intercepts_WS)

#Compute predictions from the mean slope and mean intercept and store in dataframe along with values of winter mortalities used for predictions
preds.mean_WS<-mean_intercept_WS+mean_slope_WS*xrange_WS
preds.mean_WS<-as.data.frame(preds.mean_WS)
preds.mean_WS$xval<-xrange_WS

#Check proportion of slopes that are not significant... not many
length(which(slopes_WS>0))/length(slopes_WS)


#Keep predicted values of central 95% of relationships only
preds_to_samp_WS<-preds_WS[,c(select_slopes_WS)]
#Create vector to store upper and lower values of predictions from selected relationships
preds.mean_WS$UCI<-NA
preds.mean_WS$LCI<-NA


#Sample max and min predicted values for each point of the range of winter survival estimates considered
for(i in 1:nrow(preds.mean_WS)){
  
  preds.mean_WS$UCI[i]<-max(preds_to_samp_WS[i,])
  preds.mean_WS$LCI[i]<-min(preds_to_samp_WS[i,])
  
}

#Pivot dataframe for plotting
preds_WS<-as.data.frame(preds_WS)
preds_WS$dummy<-'dummy'
preds_long_WS<-pivot_longer(preds_WS[,c(select_slopes_WS,ncol(preds_WS))], cols=-dummy )
preds_long_WS<-preds_long_WS%>%arrange(name)%>%
  mutate(xval=rep(xrange_WS, times=(nrow(preds_long_WS)/length(xrange_WS))))%>%print(n=100)

#Plot figure
(Fig3<-ggplot(data=S.s.mort%>%filter(year>1998,year<2019),
                  aes(y=mortality_spring,
                      x=mortality_winter))+
    #Mean relationship from simulated relationships
    geom_line(data=preds.mean_WS, aes(x=(xval), y=(preds.mean_WS)), col='black')+
    
    #Min and max predicted values from the central 95% of simulated relationship slopes
    geom_ribbon(data=preds.mean_WS, aes(x=xval, y=preds.mean_WS, ymax=UCI,ymin=LCI), alpha=0.2)+
    
    
    #Error associated to each estimate (on X and Y)
    geom_errorbar(aes(ymax=LCI_m_spring,ymin=UCI_m_spring), width=0.002,alpha=0.3)+
    geom_errorbar(aes(xmin=UCI_m_winter,xmax=LCI_m_winter), width=0.002,alpha=0.3)+
    
    #Individual mortality estimates for winter and spring
    geom_point(size=2.5)+
    geom_point(data=S.s.mort%>%filter(year>2007,year<2019),
               aes(y=mortality_spring,
                   x=mortality_winter), col='white', size=1.25)+
    
    #Some more plotting parameters
    coord_cartesian(ylim=c(0,0.13),xlim=c(0,0.1))+
    scale_y_continuous(name='Spring mortality')+
    scale_x_continuous(name="Winter mortality")+
    theme_classic()+
    
    #ggtitle(label='Correlation spring-winter mortalities 1999-2019')+
    
    #geom_smooth(method='lm',se = F, lty=2)+
    theme_classic()+
    theme(legend.position="none",
          axis.text=element_text(size=15, colour="black"),
          axis.title=element_text(size=16, colour="black", face="bold"),
          text = element_text(size=12, colour="black"),
          legend.text=element_text(size=10, colour="black"),
          legend.title=element_text(size=13, colour="black")))

ggsave(plot=Fig3, file='fig3.pdf', height=8, width=8)


# Figure 4 main text ####

#Load and process survival estimates
### Seasonal survival estimates (this study) ####
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


#Repeat same process for average survival per hunting period
#Load survival estimates from output file (on logit scale: Betas)
S.s.p.betas<-read_csv2('M10_this_study_betas.csv')%>%
  filter(str_detect(beta,'S_'))%>%
  mutate(season=ifelse(str_detect(beta,'ete'),'summer',
                       ifelse(str_detect(beta,'aut'),'fall',
                              ifelse(str_detect(beta,'hiv'),'winter',
                                     ifelse(str_detect(beta,'prin'),'spring',NA)))))%>%
  mutate(period=ifelse(str_detect(beta,'d1'),1,
                       ifelse(str_detect(beta,'d2'),2,3)))%>%
  dplyr::select(Beta=Value,LCI=`Cl-`, UCI=`Cl+`,SE,period, season)


#Duplicate values of seasonal survival by period to have one estimate per period to simplify code:
winter.dat<-S.s.p.betas[which(S.s.p.betas$season=='winter'&S.s.p.betas$period==1),]
winter.dat$period<-2

summer.dat<-S.s.p.betas[c(which(S.s.p.betas$season=='summer'),which(S.s.p.betas$season=='summer')),]
summer.dat$period<-c(1,2)

fall.dat<-S.s.p.betas[c(which(S.s.p.betas$season=='fall'&S.s.p.betas$period==2)),]
fall.dat$period<-c(3)

S.s.p.betas<-S.s.p.betas%>%
  bind_rows(winter.dat,summer.dat,fall.dat)%>%
  arrange(period)



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



#Plot graph

# Plot Figure 4 
(Fig4<-ggplot(data = mean.sim%>% 
                   filter(Time<2018),
                 
                 aes(x=Time, y=Estimates))+
    
    geom_point(size=1.75)+
    geom_line()+
    geom_errorbar(aes(x=Time, ymin=LCI, ymax=UCI), width=0.3)+
    
    #Lines for period means
    #Period 1
    geom_segment(
      aes(x = 1989.75,    y = as.numeric(mean.p.sim%>%filter(Period==1)%>% dplyr::select(Estimates)),
          xend = 1997, yend = as.numeric(mean.p.sim%>%filter(Period==1)%>%dplyr::select(Estimates))),
      color='#F28E2B', lty=2)+
    
    
    #links between p1-p2
    geom_segment(
      aes(x = 1997,    y = as.numeric(mean.p.sim%>%filter(Period==1)%>%dplyr::select(Estimates)),
          xend = 1998, yend = as.numeric(mean.p.sim%>%filter(Period==2)%>%dplyr::select(Estimates))),
      color='#F28E2B', lty=2)+
    
    ##Period 2
    geom_segment(
      aes(x = 1998,    y = as.numeric(mean.p.sim%>%filter(Period==2)%>%dplyr::select(Estimates)),
          xend = 2007, yend = as.numeric(mean.p.sim%>%filter(Period==2)%>%dplyr::select(Estimates))),
      color='#F28E2B', lty=2)+
    
    #links between p2-p3
    geom_segment(
      aes(x = 2007,    y = as.numeric(mean.p.sim%>%filter(Period==2)%>%dplyr::select(Estimates)),
          xend = 2008, yend = as.numeric(mean.p.sim%>%filter(Period==3)%>%dplyr::select(Estimates))),
      color='#F28E2B', lty=2)+
    ##Period 3
    geom_segment(
      aes(x = 2008,    y = as.numeric(mean.p.sim%>%filter(Period==3)%>%dplyr::select(Estimates)),
          xend = 2017.25, yend = as.numeric(mean.p.sim%>%filter(Period==3)%>%dplyr::select(Estimates))),
      color='#F28E2B', lty=2)+
    
    
    
    #Polygons for CI around period mean
    #Shading average p1
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#F28E2B',col=NA,
                 aes(x=c(1989.5,1997,1997,1989.5),y=c(mean.p.sim%>%filter(Period==1, method=='Seasonal')%>%pull(UCI),
                                                      mean.p.sim%>%filter(Period==1, method=='Seasonal')%>%pull(UCI),
                                                      mean.p.sim%>%filter(Period==1, method=='Seasonal')%>%pull(LCI),
                                                      mean.p.sim%>%filter(Period==1, method=='Seasonal')%>%pull(LCI))))+
    #Shading link p1-p2
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#F28E2B',col=NA,
                 aes(x=c(1997,1998,1998,1997),y=c(mean.p.sim%>%filter(Period==1, method=='Seasonal')%>%pull(UCI),
                                                  mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(LCI),
                                                  mean.p.sim%>%filter(Period==1, method=='Seasonal')%>%pull(LCI))))+
    #Shading average p2
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#F28E2B',col=NA,
                 aes(x=c(1998,2007,2007,1998),y=c(mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(LCI),
                                                  mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(LCI))))+
    #Shading link p2-p3
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#F28E2B',col=NA,
                 aes(x=c(2007,2008,2008,2007),y=c(mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(UCI),
                                                  mean.p.sim%>%filter(Period==3, method=='Seasonal')%>%pull(UCI),
                                                  mean.p.sim%>%filter(Period==3, method=='Seasonal')%>%pull(LCI),
                                                  mean.p.sim%>%filter(Period==2, method=='Seasonal')%>%pull(LCI))))+
    
    #Shading average p3
    geom_polygon(data=data.frame(c(1,2,3,4)),
                 alpha=0.2, fill='#F28E2B',col=NA,
                 aes(x=c(2008,2017.5,2017.5,2008),y=c(mean.p.sim%>%filter(Period==3, method=='Seasonal')%>%pull(UCI),
                                                      mean.p.sim%>%filter(Period==3, method=='Seasonal')%>%pull(UCI),
                                                      mean.p.sim%>%filter(Period==3, method=='Seasonal')%>%pull(LCI),
                                                      mean.p.sim%>%filter(Period==3, method=='Seasonal')%>%pull(LCI))))+
    
    
    
    
    geom_line(col='#4E79A7')+
    geom_errorbar(aes(x=Time, ymin=LCI, ymax=UCI), width=0.3,col='#4E79A7')+
    geom_point(size=1.75, col='#4E79A7')+
    
    annotate(geom = 'text', x=1993, y=0.33, label = 'Historical hunting\nregulations', size=5.5)+
    annotate(geom = 'text', x=2002.5, y=0.33, label = 'Special hunting\nregulations in\nCanada only', size=5.5 )+
    annotate(geom = 'text', x=2013, y=0.33, label = 'Special hunting\nregulations in\nCanada and the USA', size=5.5 )+
    
    geom_vline(xintercept = 1997.5, lty=1)+
    geom_vline(xintercept = 2007.5, lty=1)+
    #scale_color_manual(values=c("#F28E2B"))+
    scale_x_continuous(name='', breaks=seq(1990,2017,3))+
    scale_y_continuous(name='Annual survival probability', limits=c(0,1))+
    coord_cartesian(ylim=c(0.25,1))+
    theme_classic()+
    theme(axis.text=element_text(size=15, colour="black"),
          axis.title=element_text(size=16, colour="black", face="bold"),
          text = element_text(size=12, colour="black"),
          legend.text=element_text(size=10, colour="black"),
          legend.title=element_text(size=13, colour="black")))


ggsave(plot=Fig4, filename = 'Fig4.pdf',height = 7, width=7)




