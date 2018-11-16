library(shiny)
library(ggplot2)
library(plotly)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
library(magrittr)


  theme_set(theme_bw())


  dirpth <- "C:/GitHub/WHATapp"


# Set up country colours
  cnt.cols <- setNames(c("#FFCC00","#330099","#000000","#FF00CC","#0066FF","#FFFF00","#FFFFCC","#99CC99","#33FF00","#FF6600","#3366FF","#663333","#6699FF","#9966CC","#990000","#CCFFCC","#003200","#CCFFFF","#3333FF","#666666","#FF6600","#99FF33","#CC00FF","#660000","#FF6699","#3300CC","#00CCFF","#00FF99","#CCCC33","#FFFF66","#9999FF","#330000","#6633FF","#FF99FF","#CCFF99"),
                       c("AU", "BZ","CK","CN","EC","EP","ES","FJ","FM","ID","JP","KI","KR","MH","NC","NR","NU","NZ","PF","PG","PH","PT","PW","SB","SN","SU","SV","TO","TV","TW","US","VN","VU","WF","WS"))

  cnt.codes <- list(cnt=c("AS","AU","CN","CA","CK","EU","FM","FJ","PF","GU","ID","JP","KI","KR","MH","NR","NC","NZ","NU","MP","PW","PG","PH","WS","SB","TW","TK","TO","TV","US","VU","WF"),
                    nm=c("American Samoa","Australia","China","Canada","Cook Islands","European Union","FSM","Fiji","French Polynesia","Guam","Indonesia","Japan","Kiribati","Korea","Marshall Islands","Nauru","New Caledonia","New Zealand","Niue","Northern Mariana Islands","Palau","Papua New Guinea","Philippines","Samoa","Solomon Islands","Taiwan","Tokelau","Tonga","Tuvalu","US","Vanuatu","Wallis and Futuna"))

  cnt.switches <- list(old=c("GL","PX","LN","NF","MA","HB","JT","JV","PY","WK","ES","PT"),
                       new=c("KI","KI","KI","AU","NC","US","US","US","US","US","EU","EU"))

  HSind <- c("H4","H5","I1","I2","I3","I4","I5","I6","I7","I8","I9","IW")

  HScoord <- list(lon=c(181,195,140,165,155,185,215,165,195,173,202,130),
                  lat=c(-4,-5,3.5,-4.5,15,15,-5,25,-22,-15,-14,-25))
  
#  cnt.vec <- c("AS","AU","CN","CA","CK","EU","FM","FJ","PF","GU","ID","JP","KI","KR","MH","NR","NC","NZ","NU","MP","PW","PG","PH","WS","SB","TW","TK","TO","TV","US","VU","WF")
  

#dat <- read.csv(paste0(getwd(), "/shiny/Data/Data_Matrix.csv"), header=TRUE)


  anncat <- read.table(file=paste0(dirpth, "/PS_ANNCATEST_EEZ_WCPFC.txt"), header=TRUE, sep=",", stringsAsFactors=FALSE)

  
#____________________________________________________________________________________________________________  
# With all the HS included between 20N and 20S

# Summarise and output PS effort by year and flag for direct export to the app
  dat <- anncat %>% filter(eez %in% HSind, eez != "I6", eez != "I7", yy > 2001, yy < 2018) %>% mutate(tunmt=bet_mt+skj_mt+yft_mt) %>% group_by(yy, Flag=flag) %>%
                    summarise(Days=sum(days), betmt=sum(bet_mt), skjmt=sum(skj_mt), yftmt=sum(yft_mt), tunmt=sum(tunmt))

  flag.ind <- match(dat$Flag, cnt.switches$old)

  dat$Flag <- ifelse(dat$Flag %in% cnt.switches$old, cnt.switches$new[flag.ind], dat$Flag)

  dat %<>% filter(Flag %in% cnt.codes$cnt)

  exp.dat <- expand.grid(yy=2002:2017, Flag=cnt.codes$cnt)

  Ind.flg <- match(paste(exp.dat$yy, exp.dat$Flag), paste(dat$yy, dat$Flag))

  exp.dat$Days <- dat$Days[Ind.flg]
  exp.dat$Catch <- dat$tunmt[Ind.flg]
  exp.dat[is.na(exp.dat)] <- 0

  tmp <- exp.dat %>% select(-Catch) %>% spread(key=Flag, value=Days)
  write.table(t(tmp), file=paste0(dirpth, "/WHATapp/shiny/Data/Annual_PS_Effort.csv"), sep=",", col.names=FALSE)
  
  tmp <- exp.dat %>% select(-Days) %>% spread(key=Flag, value=Catch)
  write.table(t(tmp), file=paste0(dirpth, "/WHATapp/shiny/Data/Annual_PS_Catch.csv"), sep=",", col.names=FALSE)  
  


  
#____________________________________________________________________________________________________________  
# With all the HS between 20N and 20S except for I1, I2, I8 and I9 which are removed as PSing has largely been banned there

  # Summarise and output PS effort by year and flag for direct export to the app
  dat <- anncat %>% filter(eez %in% HSind, !(eez %in% c("I1","I2","I6","I7","I8","I9")), yy > 2001, yy < 2018) %>% mutate(tunmt=bet_mt+skj_mt+yft_mt) %>% group_by(yy, Flag=flag) %>%
                    summarise(Days=sum(days), betmt=sum(bet_mt), skjmt=sum(skj_mt), yftmt=sum(yft_mt), tunmt=sum(tunmt))
  
  flag.ind <- match(dat$Flag, cnt.switches$old)
  
  dat$Flag <- ifelse(dat$Flag %in% cnt.switches$old, cnt.switches$new[flag.ind], dat$Flag)
  
  dat %<>% filter(Flag %in% cnt.codes$cnt)
  
  exp.dat <- expand.grid(yy=2002:2017, Flag=cnt.codes$cnt)
  
  Ind.flg <- match(paste(exp.dat$yy, exp.dat$Flag), paste(dat$yy, dat$Flag))
  
  exp.dat$Days <- dat$Days[Ind.flg]
  exp.dat$Catch <- dat$tunmt[Ind.flg]
  exp.dat[is.na(exp.dat)] <- 0
  
  tmp <- exp.dat %>% select(-Catch) %>% spread(key=Flag, value=Days)
  write.table(t(tmp), file=paste0(dirpth, "/WHATapp/shiny/Data/Annual_PS_Effort-I1.csv"), sep=",", col.names=FALSE)
  
  tmp <- exp.dat %>% select(-Days) %>% spread(key=Flag, value=Catch)
  write.table(t(tmp), file=paste0(dirpth, "/WHATapp/shiny/Data/Annual_PS_Catch-I1.csv"), sep=",", col.names=FALSE)  
  
  
#____________________________________________________________________________________________________________


















