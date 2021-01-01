

library(dplyr)
library(tidyr)
library(reshape2)

reg_merge <- function(FB,SW,POP,...,mp=100000,decimals=2,sig=0.95){
  subgroup <- quos(...)
  
  recat <- function(n) {
    icdd <- as.integer(car::recode(n,
                                   " 0:0.9=1;    1:2.9=2;    3:6.9=3;    7:8.9=4;    9:9.9=5;    10:10.9=6;  11:11.9=7;  12:13.9=8; 
 14:14.9=9;  15:15.9=10; 16:16.9=11; 17:17.9=12; 18:18.9=13; 19:20.9=14; 21:21.9=15; 22:22.9=16; 
 23:24.9=17; 25:25.9=18; 26:29.9=59; 30:31.9=19; 32:32.9=20; 33:34.9=21; 35:36.9=59; 40:41.9=23; 
 42:42.9=59; 43:43.9=24; 44:44.9=25; 45:45.9=26; 46:46.9=27; 47:47.9=28; 48:48.9=59; 49:49.9=28; 
 50:50.9=29; 51:51.9=30; 52:52.9=31; 53:53.9=32; 54:54.9=33; 55:55.9=34; 56:56.9=35; 57:57.9=36; 
 58:58.9=37; 59:59.9=59; 60:60.9=38; 61:61.9=39; 62:62.9=40; 63:63.9=41; 64:64.9=42; 65:65.9=43; 
 66:66.9=44; 67:67.9=45; 68:68.9=46; 69:69.9=47; 70:72.9=48; 73:73.9=49; 74:74.9=50; 75:75.9=51; 
 76:80.9=59; 81:81.9=52; 82:85.9=53; 86:87.9=59; 88:88.9=54; 89:89.9=59; 90:90.9=55; 91:91.9=56; 
 92:94.9=57; 95:95.9=58; 96:96.9=53; 97:97.9=59; else=NA; "))
    return(icdd)
  }
  
  recat2 <- function(n) {
    icdd <- as.integer(car::recode(n,
                                   " 0:10.0=101; 10.2:10.9=101; 12:12.9=101; 13:14.9=101; 11:11.9=102; 15:15.9=103; 16:16.9=104; 18:21.9=105; 
 22:22.9=106;   23:23.9=107; 24:24.9=107; 25:25.9=108;    10.1=109; 32:32.9=109; 33:33.9=110; 34:34.9=110; 
 40:41.9=111;   50:50.9=112; 53:53.9=113; 54:54.9=114; 56:56.9=115; 61:61.9=116; 64:66.9=117; 67:67.9=118; 
 68:68.9=117;   70:72.9=119; 73:73.9=120; 82:85.9=121; 96:96.9=121; 88:88.9=121; 90:90.9=121; 91:95.9=122;
 else=NA;
 "))
    return(icdd)
  }
  
  recat3 <- function(n) {
    icdd <- as.integer(car::recode(n,
                                   " 0:14.9=201; 15:26.9=202; 30:39.9=203; 40:44.9=204; 50:50.9=205; 
   51:58.9=206; 60:63.9=207; 64:68.9=208; 69:75.9=209; 81:96.9=210;
  else=NA;"))
    return(icdd)
  }
  
  
  
  
  # Deal with  FB data.
  # Check the existence of key variables.
  "inciden" %in% colnames(FB) 
  "birthda" %in% colnames(FB)
  
  
  FB <-FB %>%
    mutate(year = as.numeric(format(inciden,'%Y')),
           age  = as.integer(age),
           sex  = as.integer(sex),
           agegrp=cut(age,c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),labels=seq(1,19,1),right=FALSE),
           icdd=as.numeric(gsub("[^0-9\\.]", "",icd10)),
           basi=as.numeric(basi)
    )%>%
    mutate(icd1=recat(icdd),
           icd2=recat2(icdd),
           icd3=recat3(icdd))%>%
    select(year,age,sex,agegrp,icdd,basi,icd1,icd2,icd3)  
  
  
  # Deal with  SW data.
  SW <-SW %>%
    mutate(year = as.numeric(format(inciden,'%Y')),
           age= as.integer(age),
           sex= as.integer(sex),
           agegrp=cut(age,c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),labels=seq(1,19,1),right=FALSE),
           icdd=as.numeric(gsub("[^0-9\\.]", "",icd10)))%>%
    mutate(icd1=recat(icdd),
           icd2=recat2(icdd),
           icd3=recat3(icdd))  
  
  POP<-POP%>%
    mutate(
      agegrp  = as.integer(agegrp),
      agegrp=factor(agegrp,labels=seq(1,19,1)))
  
  
  
  data<-data.frame(sex=c(rep(1:2,47),1),icdd=c(1:63,101:122,201:210),agegrp=factor(c(rep(1:19,5))))%>%
    expand(sex,icdd,agegrp)
  
  
  #Define function which count the number in each category variable including sex,age groups,disease categrory.
  reg_count <- function(data,...,var_new=fbs){
    group <- quos(...)
    bind_rows(
      data %>% count(!!!group,agegrp,icd1,name=var_new) %>% rename(icdd=icd1),
      data %>% count(!!!group,agegrp,icd2,name=var_new) %>% rename(icdd=icd2),
      data %>% count(!!!group,agegrp,icd3,name=var_new) %>% rename(icdd=icd3),
      data %>% count(!!!group,agegrp,name=var_new) %>% mutate(icdd=62))
  }
  
  res<-list(
    data,
    reg_count(FB,!!!subgroup,var_new="fbs"),
    reg_count(SW,!!!subgroup,var_new="sws"),
    FB %>% filter(basi%in%c(5,6,7))%>%reg_count(!!!subgroup,var_new="mv"),
    FB %>% filter(basi==0) %>% reg_count(!!!subgroup,var_new="dco")
  ) %>% purrr::reduce(left_join,by=c("sex","agegrp","icdd")) %>%
    left_join(POP,by=c("sex","agegrp"))
  
  res[is.na(res)]=0
  
  bind_rows(
    res,
    res%>%
      group_by(agegrp,icdd)%>%
      mutate(fbs=sum(fbs),sws=sum(sws),mv=sum(mv),dco=sum(dco),rks=sum(rks),sex=3)%>%
      distinct(agegrp,icdd, .keep_all=TRUE) 
  ) -> res
  
  
  res<-bind_rows(
    res%>%
      filter(agegrp%in%c(1,2))%>%
      group_by(sex,icdd)%>%
      mutate(fbs=sum(fbs),sws=sum(sws),mv=sum(mv),dco=sum(dco),rks=sum(rks),agegrp=factor(2))%>%
      distinct(sex,icdd, .keep_all=TRUE),
    res%>%filter(!(agegrp%in%c(1,2)))
  )
  
  std_pop<-data.frame(
    agegrp=factor(c(seq(2,19,1))),
    segi=c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500),
    c2000 = c(5551,7255,10091,8291,7611,9464,10246,8784,6538,6882,5094,3732,3356,2799,2058,1282,643,322),
    w2000 = c(8857,8687,8597,8467,8217,7927,7607,7148,6588,6038,5368,4548,3719,2959,2209,1520,910,635)
  )
  
  res<-res%>%left_join(std_pop,by=c("agegrp"))
  res<-res%>%group_by(!!!subgroup,icdd) %>%
    mutate(
      fhj=sum(fbs),
      shj=sum(sws),
      rhj=sum(rks),
      ihj=fhj/rhj,
      dhj=shj/rhj,
      iws_segi=sum(segi/sum(segi)*(fbs/rks)),
      dws_segi=sum(segi/sum(segi)*(sws/rks)),
      ics2000=sum(c2000/sum(c2000)*(fbs/rks)),
      dcs2000=sum(c2000/sum(c2000)*(sws/rks)),
      iws2000=sum(w2000/sum(w2000)*(fbs/rks)),
      dws2000=sum(w2000/sum(w2000)*(sws/rks)),
      ihj_var=fhj/rhj^2,
      dhj_var=shj/rhj^2,
      wts_segi=segi/sum(segi),
      wts_c2000=c2000/sum(c2000),
      wts_w2000=w2000/sum(w2000),
      iws_segi_var=sum(as.numeric((wts_segi^2)*(fbs/(rks )^2))),
      dws_segi_var=sum(as.numeric((wts_segi^2)*(sws/(rks )^2))),
      ics2000_var=sum(as.numeric((wts_c2000^2)*(fbs/(rks )^2))),
      dcs2000_var=sum(as.numeric((wts_c2000^2)*(sws/(rks )^2))),
      iws2000_var=sum(as.numeric((wts_w2000^2)*(fbs/(rks )^2))),
      dws2000_var=sum(as.numeric((wts_w2000^2)*(sws/(rks )^2)))
    ) %>%
    distinct(!!!subgroup,icdd, .keep_all=TRUE) %>%
    mutate(
      ihj_lower=mp*qgamma((1-sig)/2, shape=ihj^2/(ihj_var))/(ihj/ihj_var),
      ihj_upper=mp*qgamma(1-((1-sig)/2), shape=1+ihj^2/(ihj_var))/(ihj/ihj_var),
      dhj_lower=mp*qgamma((1-sig)/2, shape=dhj^2/(dhj_var))/(dhj/dhj_var),
      dhj_upper=mp*qgamma(1-((1-sig)/2), shape=1+dhj^2/(dhj_var))/(dhj/dhj_var),
      iws_segi_lower=mp*qgamma((1-sig)/2, shape=iws_segi^2/(iws_segi_var))/(iws_segi/iws_segi_var),
      iws_segi_upper=mp*qgamma(1-((1-sig)/2), shape=1+iws_segi^2/(iws_segi_var))/(iws_segi/iws_segi_var),
      dws_segi_lower=mp*qgamma((1-sig)/2, shape=dws_segi^2/(iws_segi_var))/(iws_segi/iws_segi_var),
      dws_segi_upper=mp*qgamma(1-((1-sig)/2), shape=1+dws_segi^2/(dws_segi_var))/(dws_segi/dws_segi_var),
      ics2000_lower=mp*qgamma((1-sig)/2, shape=ics2000^2/(ics2000_var))/(ics2000/ics2000_var),
      ics2000_upper=mp*qgamma(1-((1-sig)/2), shape=1+ics2000^2/(ics2000_var))/(ics2000/ics2000_var),
      dcs2000_lower=mp*qgamma((1-sig)/2, shape=dcs2000^2/(dcs2000_var))/(dcs2000/dcs2000_var),
      dcs2000_upper=mp*qgamma(1-((1-sig)/2), shape=1+dcs2000^2/(dcs2000_var))/(dcs2000/dcs2000_var),
      iws2000_lower=mp*qgamma((1-sig)/2, shape=iws2000^2/(iws2000_var))/(iws2000/iws2000_var),
      iws2000_upper=mp*qgamma(1-((1-sig)/2), shape=1+iws2000^2/(iws2000_var))/(iws2000/iws2000_var),
      dws2000_lower=mp*qgamma((1-sig)/2, shape=dws2000^2/(dws2000_var))/(dws2000/dws2000_var),
      dws2000_upper=mp*qgamma(1-((1-sig)/2), shape=1+dws2000^2/(dws2000_var))/(dws2000/dws2000_var)
    ) %>%
    mutate(
      ihj=mp*ihj,
      dhj=mp*dhj,
      iws_segi=mp*iws_segi,
      dws_segi=mp*dws_segi,
      ics2000=mp*ics2000,
      dcs2000=mp*dcs2000,
      iws2000=mp*iws2000,
      dws2000=mp*dws2000
    ) %>%
    mutate(
      ihj=round(ihj, digits=decimals),
      ihj_lower=round(ihj_lower, digits=decimals),
      ihj_upper=round(ihj_upper, digits=decimals),
      dhj=round(dhj, digits=decimals),
      dhj_lower=round(dhj_lower, digits=decimals),
      dhj_upper=round(dhj_upper, digits=decimals),
      iws_segi=round(iws_segi, digits=decimals),
      iws_segi_lower=round(iws_segi_lower, digits=decimals),
      iws_segi_upper=round(iws_segi_upper, digits=decimals),
      dws_segi=round(dws_segi, digits=decimals),
      dws_segi_lower=round(dws_segi_lower, digits=decimals),
      dws_segi_upper=round(dws_segi_upper, digits=decimals),
      ics2000=round(ics2000, digits=decimals),
      ics2000_lower=round(ics2000_lower, digits=decimals),
      ics2000_upper=round(ics2000_upper, digits=decimals),
      dcs2000=round(dcs2000, digits=decimals),
      dcs2000_lower=round(dcs2000_lower, digits=decimals),
      dcs2000_upper=round(dcs2000_upper, digits=decimals),
      iws2000=round(iws2000, digits=decimals),
      iws2000_lower=round(iws2000_lower, digits=decimals),
      iws2000_upper=round(iws2000_upper, digits=decimals),
      dws2000=round(dws2000, digits=decimals),
      dws2000_lower=round(dws2000_lower, digits=decimals),
      dws2000_upper=round(dws2000_upper, digits=decimals)
    ) %>%
    select(!!!subgroup,icdd, fhj, shj,rhj,
           ihj,ihj_lower,ihj_upper,
           dhj,dhj_lower,dhj_upper,
           iws_segi,iws_segi_lower,iws_segi_upper,
           dws_segi,dws_segi_lower,dws_segi_upper,
           ics2000,ics2000_lower,ics2000_upper,
           dcs2000,dcs2000_lower,dcs2000_upper,
           iws2000,iws2000_lower,iws2000_upper,
           dws2000,dws2000_lower,dws2000_upper)  
  res[is.na(res)]=0
  
  return(res)
  
}





