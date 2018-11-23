###############################################################################################
############          Pacific plot showing the EEZs and high seas regions          ############
############                           using ggplot2                               ############
############                      By Steve Brouwer 13/06/2018                      ############
###############################################################################################

setwd("C:/Users/stephenb.000/Documents/R/maps/")
library(survival)
library(maps)
library(maptools)
library(mapdata)
library(ggplot2)
library(plyr)
library(grid)

all_states <- map_data("world2Hires") # if you want to go above 180 use world2Hires else worldHires

#eez overlay
#eez        <- read.table("C:/Users/stephenb.000/Documents/R/plotting maps test/ggplot_maps/EZNEW2.txt", sep=" ", header=F, skip=0) #All regions
eez        <- read.csv("C:/Users/stephenb.000/Documents/R/maps/EEZ_final_nz_fix.csv") #All regions
#eezlimits  <- read.csv("C:/Users/stephenb.000/Documents/R/plotting maps test/ggplot_maps/pna_eez.csv") #PNA regions
#eez_TKA  <- read.csv("C:/Users/stephenb.000/Documents/R/plotting maps test/ggplot_maps/tka_eez.csv") #TTA regions
#HS<-read.csv("C:/Users/stephenb.000/Documents/R/maps/Only_IW_all2.csv")
minami<-read.csv("C:/Users/stephenb.000/Documents/R/maps/minami_tori.csv")

#HS<-read.csv("C:/Users/stephenb.000/Documents/R/maps/Only_IW_all2.csv")
I1<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I1.csv")
I2<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I2.csv")
I3<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I3.csv")
I4<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I4.csv")
I5<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I5.csv")
I6<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I6_2.csv")
I7<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I7_2.csv")
I8<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I8.csv")
I9<-read.csv("C:/Users/stephenb.000/Documents/R/maps/I9.csv")
H4<-read.csv("C:/Users/stephenb.000/Documents/R/maps/H4.csv")
H5<-read.csv("C:/Users/stephenb.000/Documents/R/maps/H5.csv")

#different made up colours for the sea
grecol    <- rgb(red=0,green=100, blue=0, alpha=95, maxColorValue=255) #green colour
blucol    <- rgb(red=60,green=200, blue=175, alpha=70, maxColorValue=255)

# All states for the map.
states     <- subset(all_states, region %in% 
                       c("American Samoa", "Australia", "Canada","China","Cook Islands","Fiji", "French Polynesia", 
                         "Guam", "Hawaii", "Indonesia","Japan","Kiribati", "Marshall Islands", "Mexico", "Micronesia",
                         "Nauru", "New Caledonia", "New Zealand", "South Korea","North Korea" , "Niue", "Northern Mariana Islands","Palau", 
                         "Papua New Guinea", "Philippines", "Samoa", "Solomon Islands","Tokelau", "Tonga", "Tuvalu", 
                         "USA", "USSR", "Vanuatu","Panama","Mongolia" , "Chile","Argentina", "New Caledonia","Belize",
                         "Nicaragua","Ecuador","Honduras","Costa Rica","Colombia", "Uruguay", "Brazil", 
                         "Peru", "Guatemala","Venezuela","Bolivia","Paraguay" ))

# region labels
i3_x<-c(133,156,172)
i3_y<-c(16,17,19)
i7_x<-c(163,175,200)
i7_y<-c(-28,-28,-28)
reg_lab<-c("I1","I2",rep("I3",3),"I4","I5","I6",rep("I7",3),"I8","I9","H4","H5")
reg_labx<-c(141,161,i3_x,181,200,167,i7_x,173, 203,181,195)
reg_laby<-c(  3,  1,i3_y, 10, 12, 29,i7_y,-15, -13, -5, -5)

p          <- ggplot() +
  geom_polygon(data=H4, aes(H4[,4],H4[,2]), size=1, fill="brown") + 
  geom_polygon(data=H5, aes(H5[,4],H5[,2]), size=1 , fill="green") + 
  
  geom_polygon(data=I1, aes(I1[,4],I1[,2]), size=1 , fill="blue") + 
  geom_polygon(data=I2, aes(I2[,4],I2[,2]), size=1 , fill="red") + 
  geom_polygon(data=I3, aes(I3[,4],I3[,2]), size=1 , fill="darkgreen") + 
  geom_polygon(data=I4, aes(I4[,4],I4[,2]), size=1 , fill="orange") + 
  geom_polygon(data=I5, aes(I5[,4],I5[,2]), size=1 , fill="magenta") + 
  geom_polygon(data=I6, aes(I6[,4],I6[,2]), size=1 , fill="cyan4") + 
  geom_polygon(data=I7, aes(I7[,4],I7[,2]), size=1 , fill="greenyellow") + 
  geom_polygon(data=I8, aes(I8[,4],I8[,2]), size=1 , fill="purple") + 
  geom_polygon(data=I9, aes(I9[,4],I9[,2]), size=1 , fill="yellow") + 
  
  geom_polygon(data=minami, aes(minami[,2], minami[,3]), colour="white", fill="white") + 
  geom_polygon(data=eez, aes(eez[,2], eez[,3]), colour="white", fill=grecol) + 
   
  geom_polygon(data=states, aes(x=long, y=lat, group = group),colour ="black",  fill="grey10" ) + 
  geom_segment(aes(x = c(230,230.5,210), y = c(-40,0,0), xend = c(230,209.7,210), yend = c(0,0,40)),  color="red", size=2)+ # the WCPFC boundary
  
  annotate("text", label = reg_lab, x = reg_labx, y = reg_laby, size = 5, colour = "black")+
  
  #coord_cartesian(xlim =c(140,210),ylim=c(-38,0)) + # for albacore
  coord_cartesian(xlim =c(130,250),ylim=c(-35,30)) + 
  theme(panel.background = element_rect(fill=blucol), 
        plot.title = element_text(size = 20),
        axis.text.x = element_text(vjust =1, size = 14, colour = "black"),
        axis.text.y = element_text(hjust =1, size = 14,  colour = "black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14,angle=90)) +  
  xlab('Longitude') + ylab('Latitude') 
p

#windows(20,13) 
#p
#savePlot("WCPFC_alb_HS_regions", type="png")#

windows(20,16) 
p
savePlot("WCPFC_HS_eez_final_with_I6_2_I7_2", type="png")
#savePlot("WCPFC_HS_eez_final", type="png")
#savePlot("WCPFC_HS_eez_fine", type="png")
#savePlot("WCPFC_HS_eez_not_so_fine", type="png")
