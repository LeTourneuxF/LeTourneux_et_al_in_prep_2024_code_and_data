library(tidyverse)
library(ggplot2)
library(cowplot)
library(scales)


setwd("~/Doc/MSs/MS_seasonal_survival/soumission_Ecology/script_and_data")

#Same approach as Figure 3 but for the relationships between fall-winter and fall-spring

# Appendix S5:Figures S1 and S2 ####
#First, extract betas to generate randomized samples for bootstrap
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

#Extract betas of fall survival 
beta.fall<-S.s.betas%>%
  filter(season=='fall',
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
#Matrix to store fall survival estimates (rows are years, columns are simulations)
sim.fall<-matrix(ncol=n.sims,nrow=nrow(beta.fall))

#Simulate datasets of spring and winter survival (5000x each year)
for(i in 1:n.years){
  
  sim.spring[i,]<-1-rlogitnorm(n=n.sims,mu=beta.spr$Beta[i], sigma = beta.spr$SE[i])
  
  sim.winter[i,]<-1-rlogitnorm(n=n.sims,mu=beta.wtr$Beta[i], sigma = beta.wtr$SE[i])
  
  sim.fall[i,]<-1-rlogitnorm(n=n.sims,mu=beta.fall$Beta[i], sigma = beta.fall$SE[i])
  
}


#Effect of fall mortality on winter mortality
#Create empty matrices to store relationships between winter and fall mortality for simulated values
#Slope of relationship
slopes_FW<-matrix(ncol=1,nrow=n.sims)
#Intercept of relationship
intercepts_FW<-matrix(ncol=1,nrow=n.sims)
#Range of observed values of fall mortality to make predictions for each simulated relationship
xrange_FW<-seq(from=(range(1-invlogit(beta.fall$Beta))[1])-0.003, to=(range(range(1-invlogit(beta.fall$Beta)))[2])+0.003, length.out=20)
#Matrix to store predictions from each simulated relationship
preds_FW<-matrix(nrow=length(xrange_FW), ncol=n.sims)

#Effect of fall mortality on spring mortality
#Create empty matrices to store relationships between spring and fall mortality for simulated values
#Slope of relationship
slopes_FS<-matrix(ncol=1,nrow=n.sims)
#Intercept of relationship
intercepts_FS<-matrix(ncol=1,nrow=n.sims)
#Range of observed values of fall mortality to make predictions for each simulated relationship
xrange_FS<-seq(from=(range(1-invlogit(beta.fall$Beta))[1])-0.003, to=(range(range(1-invlogit(beta.fall$Beta)))[2])+0.003, length.out=20)
#Matrix to store predictions from each simulated relationship
preds_FS<-matrix(nrow=length(xrange_FS), ncol=n.sims)


#Extract the coefficients of the simulated relationships
for(i in 1:n.sims){
  
  coef_sim_FW<-coef(lm(sim.winter[,i]~sim.fall[,i]))
  slopes_FW[i]<-as.numeric(coef_sim_FW[2])
  intercepts_FW[i]<-as.numeric(coef_sim_FW[1])
  
  coef_sim_FS<-coef(lm(sim.spring[,i]~sim.fall[,i]))
  slopes_FS[i]<-as.numeric(coef_sim_FS[2])
  intercepts_FS[i]<-as.numeric(coef_sim_FS[1])
  
  
}

#Make predictions over the range of potential mortalities for each relationship
for(i in 1:n.sims){
  
  preds_FW[,i]<-intercepts_FW[i]+slopes_FW[i]*xrange_FW
  
  preds_FS[,i]<-intercepts_FS[i]+slopes_FS[i]*xrange_FS
  
}


#Extract metrics from simulated relationships

#Fall on winter
#Central 95% values of relationships
CI_FW<-sort(slopes_FW)[c((0.025*n.sims),(0.975*n.sims))]
#Mean slope from simulated relationships
mean_slope_FW<-mean(slopes_FW)
#Select central 95% of slopes distribution to plot to figure as 95%CI
select_slopes_FW<-which(slopes_FW>CI_FW[1]&slopes_FW<CI_FW[2])
#Mean intercept from simulated relationships
mean_intercept_FW<-mean(intercepts_FW)

#Compute predictions from the mean slope and mean intercept and store in dataframe along with values of winter mortalities used for predictions
preds.mean_FW<-mean_intercept_FW+mean_slope_FW*xrange_FW
preds.mean_FW<-as.data.frame(preds.mean_FW)
preds.mean_FW$xval<-xrange_FW

#Fall on spring
#Central 95% values of relationships
CI_FS<-sort(slopes_FS)[c((0.025*n.sims),(0.975*n.sims))]
#Mean slope from simulated relationships
mean_slope_FS<-mean(slopes_FS)
#Select central 95% of slopes distribution to plot to figure as 95%CI
select_slopes_FS<-which(slopes_FS>CI_FS[1]&slopes_FS<CI_FS[2])
#Mean intercept from simulated relationships
mean_intercept_FS<-mean(intercepts_FS)

#Compute predictions from the mean slope and mean intercept and store in dataframe along with values of winter mortalities used for predictions
preds.mean_FS<-mean_intercept_FS+mean_slope_FS*xrange_FS
preds.mean_FS<-as.data.frame(preds.mean_FS)
preds.mean_FS$xval<-xrange_FS


#Keep predicted values of central 95% of relationships only
preds_to_samp_FW<-preds_FW[,c(select_slopes_FW)]
#Create vector to store upper and lower values of predictions from selected relationships
preds.mean_FW$UCI<-NA
preds.mean_FW$LCI<-NA

#Keep predicted values of central 95% of relationships only
preds_to_samp_FS<-preds_FS[,c(select_slopes_FS)]
#Create vector to store upper and lower values of predictions from selected relationships
preds.mean_FS$UCI<-NA
preds.mean_FS$LCI<-NA

#Sample max and min predicted values for each point of the range of winter survival estimates considered
for(i in 1:nrow(preds.mean_FS)){
  
  preds.mean_FW$UCI[i]<-max(preds_to_samp_FW[i,])
  preds.mean_FW$LCI[i]<-min(preds_to_samp_FW[i,])
  
  preds.mean_FS$UCI[i]<-max(preds_to_samp_FS[i,])
  preds.mean_FS$LCI[i]<-min(preds_to_samp_FS[i,])
}

#Pivot dataframe for plotting
preds_FW<-as.data.frame(preds_FW)
preds_FW$dummy<-'dummy'
preds_long_FW<-pivot_longer(preds_FW[,c(select_slopes_FW,ncol(preds_FW))], cols=-dummy )
preds_long_FW<-preds_long_FW%>%arrange(name)%>%
  mutate(xval=rep(xrange_FW, times=(nrow(preds_long_FW)/length(xrange_FW))))%>%print(n=100)

preds_FS<-as.data.frame(preds_FS)
preds_FS$dummy<-'dummy'
preds_long_FS<-pivot_longer(preds_FS[,c(select_slopes_FS,ncol(preds_FS))], cols=-dummy )
preds_long_FS<-preds_long_FS%>%arrange(name)%>%
  mutate(xval=rep(xrange_FS,times=(nrow(preds_long_FS)/length(xrange_FS))))%>%print(n=100)



#Transform betas into mortality vals
Ssmort_F<-S.s.betas%>%
  filter(season %in% c('fall'))%>%
  mutate(mortality_fall=1-invlogit(Beta),
         LCI_m_fall = 1-invlogit(UCI),
         UCI_m_fall = 1-invlogit(LCI))%>%
  dplyr::select(year,mortality_fall,LCI_m_fall,UCI_m_fall)

Ssmort_W<-S.s.betas%>%
  filter(season %in% c('winter'))%>%
  mutate(mortality_winter=1-invlogit(Beta),
         LCI_m_winter = 1-invlogit(UCI),
         UCI_m_winter = 1-invlogit(LCI))%>%
  dplyr::select(mortality_winter,LCI_m_winter,UCI_m_winter)

Ssmort_S<-S.s.betas%>%
  filter(season %in% c('spring'))%>%
  mutate(mortality_spring=1-invlogit(Beta),
         LCI_m_spring = 1-invlogit(UCI),
         UCI_m_spring = 1-invlogit(LCI))%>%
  dplyr::select(mortality_spring,LCI_m_spring,UCI_m_spring)

S.s.mort<-Ssmort_F%>%
  filter(year<2019)%>%
  bind_cols(Ssmort_W,Ssmort_S)
  



(final_FW<-ggplot(data=S.s.mort%>%filter(year>1998,year<2019),
                  aes(y=mortality_winter,
                      x=mortality_fall))+
    #Mean relationship from simulated relationships
    geom_line(data=preds.mean_FW, aes(x=(xval), y=(preds.mean_FW)), col='black', lty=2)+
    
    #Min and max predicted values from the central 95% of simulated relationship slopes
    geom_ribbon(data=preds.mean_FW, aes(x=xval, y=preds.mean_FW, ymax=UCI,ymin=LCI), alpha=0.2)+
    
    #Individual mortality estimates for winter and spring
    geom_point()+
    
    #Error associated to each estimate (on X and Y)
    geom_errorbar(aes(ymax=UCI_m_winter,ymin=LCI_m_winter), width=0.002,alpha=0.3)+
    geom_errorbar(aes(xmin=UCI_m_fall,xmax=LCI_m_fall), width=0.002,alpha=0.3)+
    
    #Some more plotting parameters
    coord_cartesian(ylim=c(0,0.12),xlim=c(0,0.18))+
    scale_y_continuous(name='Winter mortality')+
    scale_x_continuous(name="Fall mortality")+
    theme_classic()+
    
    ggtitle(label='Correlation fall-winter mortalities 1999-2019')+
    
    theme(legend.position='none'))


(final_FS<-ggplot(data=S.s.mort%>%filter(year>1998,year<2019),
                  aes(y=mortality_spring,
                      x=mortality_fall))+
    #Mean relationship from simulated relationships
    geom_line(data=preds.mean_FS, aes(x=(xval), y=(preds.mean_FS)), col='black', lty=2)+
    
    #Min and max predicted values from the central 95% of simulated relationship slopes
    geom_ribbon(data=preds.mean_FS, aes(x=xval, y=preds.mean_FS, ymax=UCI,ymin=LCI), alpha=0.2)+
    
    #Individual mortality estimates for winter and spring
    geom_point()+
    
    #Error associated to each estimate (on X and Y)
    geom_errorbar(aes(ymax=LCI_m_spring,ymin=UCI_m_spring), width=0.002,alpha=0.3)+
    geom_errorbar(aes(xmin=UCI_m_fall,xmax=LCI_m_fall), width=0.002,alpha=0.3)+
    
    #Some more plotting parameters
    coord_cartesian(ylim=c(0,0.15),xlim=c(0,0.18))+
    scale_y_continuous(name='Spring mortality')+
    scale_x_continuous(name="Fall mortality")+
    theme_classic()+
    
    ggtitle(label='Correlation fall-spring mortalities 1999-2019')+
    
    theme(legend.position='none'))

# Appendix S5:Figure S3 ####

#Use betas loded earlier to compute annual survival estimates to put in relationship with annual harvest rate
beta.spr<-S.s.betas%>%
  mutate(year=year-1)%>%
  filter(season=='spring',
         year<2018)

beta.wtr<-S.s.betas%>%
  filter(season=='winter',
         year<2018)

beta.fall<-S.s.betas%>%
  filter(season=='fall',
         year<2018)

beta.summer<-S.s.betas%>%
  filter(season=='summer',
         year<2018)

#Load harvest data and keep only years btwn 1990-2017
harvest_dat<-read_csv2('harvest_rate_GSG.csv')
hr<-harvest_dat%>%
  filter(Year %in% beta.spr$year)%>%
  pull(HR)

#Following chunks identical to Script for Appendix 4:fig S5
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

#Compute mean and uncertainty around annual estimates
SE.annual<-apply(sim.annual, 1, function(x){sd(x)})
UCI.sim<-apply(sim.annual,1,FUN=function(x){sort(x)[0.975*length(x)]})
LCI.sim<-apply(sim.annual,1,FUN=function(x){sort(x)[0.025*length(x)]})
mean.sim<-data.frame(Time=c(1990:2017),Estimates=rowMeans(sim.annual),LCI=LCI.sim, UCI=UCI.sim, HR=hr)

#With same approach as Fig S1 and S2 in this script, compute slope of relationship while accounting for uncertainty in survival
slopes_s.hr_p23     <-rep(NA, times=n.sims)
intercepts_s.hr_p23 <-rep(NA, times=n.sims)

#Keep only years since establishment of spring hunting
years_p23<-which(beta.spr$year>1997)

#Compute slopes of relationships for all simulated annual survival estimates
for(i in 1:n.sims){
  
  #Compute lm 
  coef_s.hr_p23<-coef(lm(sim.annual[years_p23,i]~hr[years_p23]))
  slopes_s.hr_p23[i]<-as.numeric(coef_s.hr_p23[2])
  intercepts_s.hr_p23[i]<-as.numeric(coef_s.hr_p23[1])
  
}

#Range over which to predict effect of harvest rate on annual survival
xrange_s.hr_p23<-seq(from=min(hr[years_p23])-0.003, to=max(hr[years_p23])+0.003, length.out=20)
#Matrix to store predictions
preds_s.hr_p23<-matrix(nrow=length(xrange_s.hr_p23), ncol=n.sims)

#Make predictions over the range of observed harvest rates
for(i in 1:n.sims){preds_s.hr_p23[,i]<-intercepts_s.hr_p23[i]+slopes_s.hr_p23[i]*xrange_s.hr_p23}

#Extract 95% CIs
CI_s.hr_p23<-sort(slopes_s.hr_p23)[c((0.025*n.sims),(0.975*n.sims))]

#Extract mean slope
mean_slope_s.hr_p23<-mean(slopes_s.hr_p23)

#Keep only 95% central slopes for CI
select_slopes_s.hr_p23<-which(slopes_s.hr_p23>CI_s.hr_p23[1]&slopes_s.hr_p23<CI_s.hr_p23[2])

#Mean intercept of relationship
mean_intercept_s.hr_p23<-mean(intercepts_s.hr_p23)

preds.mean_s.hr_p23<-data.frame(preds.mean_s.hr_p23=mean_intercept_s.hr_p23+mean_slope_s.hr_p23*xrange_s.hr_p23)

preds.mean_s.hr_p23$xval<-xrange_s.hr_p23






preds_to_samp_s.hr_p23<-preds_s.hr_p23[,c(select_slopes_s.hr_p23)]
preds.mean_s.hr_p23$UCI<-NA
preds.mean_s.hr_p23$LCI<-NA

for(i in 1:nrow(preds.mean_s.hr_p23)){
  
  preds.mean_s.hr_p23$UCI[i]<-max(preds_to_samp_s.hr_p23[i,])
  preds.mean_s.hr_p23$LCI[i]<-min(preds_to_samp_s.hr_p23[i,])
  
  
}



preds_s.hr_p23<-as.data.frame(preds_s.hr_p23)

preds_s.hr_p23$dummy<-'dummy'

preds_long_s.hr_p23<-pivot_longer(preds_s.hr_p23[,c(select_slopes_s.hr_p23,ncol(preds_s.hr_p23))], cols=-dummy )


preds_long_s.hr_p23<-preds_long_s.hr_p23%>%arrange(name)%>%
  mutate(xval=rep(xrange_s.hr_p23, times=(nrow(preds_long_s.hr_p23)/length(xrange_s.hr_p23))))%>%print(n=100)




(final_s.hr_p23<-ggplot(data=mean.sim%>%filter(Time>1997),
                        aes(y=Estimates,
                            x=HR))+
    
    #geom_line(data=preds_long_s.hr_p23, aes(x=xval,y=value,col=name), alpha=0.1)+
    
    geom_line(data=preds.mean_s.hr_p23, aes(x=(xval), y=(preds.mean_s.hr_p23)), col='black', lty=2, lwd=0.75)+
    #geom_line(data=preds.mean_s.hr_p23, aes(x=(xval), y=(LCI)), lty=3)+
    #geom_line(data=preds.mean_s.hr_p23, aes(x=(xval), y=(UCI)), lty=3)+
    
    geom_ribbon(data=preds.mean_s.hr_p23, aes(x=xval, y=preds.mean_s.hr_p23, ymax=UCI,ymin=LCI), alpha=0.2)+
    geom_errorbar(aes(ymin=LCI,ymax=UCI), width=0.001)+
    
    geom_point(size=2)+
    geom_point(data=mean.sim%>%filter(Time>2007),
               aes(y=Estimates,
                   x=HR), size=1, col='white')+
    
    scale_y_continuous(name='Annual survival probability')+
    scale_x_continuous(name="Annual harvest rate")+
    theme_classic()+
    
    #ggtitle(label='Relationship Survival vs. annual harvest')+
    
    #geom_smooth(method='lm',se = F, lty=2)+
    theme(legend.position='none'))

#ggsave(final_s.hr_p23, file=)
ggsave(final_s.hr_p23, file='~/Doc/MSs/MS_seasonal_survival/figures_ms/Fig_annual_surv_vs_harvest_p23.jpg',width=7,height=4.5)


