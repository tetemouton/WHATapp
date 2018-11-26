library(ggplot2)
library(R4MFCL)
library(pkpkg)
library(data.table)
library(scales)
library(dplyr) 
library(xtable)
library(tidyr)
library(stringr)
library(grid)
library(lubridate)
library(RColorBrewer)
library(zoo)
library(maps)
library(mapdata)
library(maptools)
library(magrittr)
library(ggplot2)


theme_set(theme_bw())

setwd("C:/GitHub/WHATapp/WHATapp/shiny/Data/HighSeasMaps/")

#library(survival)



# #different made up colours for the sea
 grecol    <- rgb(red=0,green=100, blue=0, alpha=95, maxColorValue=255) #green colour
 blucol    <- rgb(red=60,green=200, blue=175, alpha=70, maxColorValue=255)
# 
# # All states for the map.
# states     <- subset(all_states, region %in% 
#                        c("American Samoa", "Australia", "Canada","China","Cook Islands","Fiji", "French Polynesia", 
#                          "Guam", "Hawaii", "Indonesia","Japan","Kiribati", "Marshall Islands", "Mexico", "Micronesia",
#                          "Nauru", "New Caledonia", "New Zealand", "South Korea","North Korea" , "Niue", "Northern Mariana Islands","Palau", 
#                          "Papua New Guinea", "Philippines", "Samoa", "Solomon Islands","Tokelau", "Tonga", "Tuvalu", 
#                          "USA", "USSR", "Vanuatu","Panama","Mongolia" , "Chile","Argentina", "New Caledonia","Belize",
#                          "Nicaragua","Ecuador","Honduras","Costa Rica","Colombia", "Uruguay", "Brazil", 
#                          "Peru", "Guatemala","Venezuela","Bolivia","Paraguay" ))



#____________________________________________________________________________________________________________  
# Mapping data and control  

load("Pacific-region-to-keep.RData", verbose=TRUE)

all_states <- map_data('world2Hires')
reg2keep <- c(reg2keep, 'Vietnam')
states <- all_states %>% filter(region %in% reg2keep)

eez.outline <- read.csv("EEZ_final_nz_fix.csv") #All regions
eez.outline$group <- 1

all_states <- map_data("world2Hires") # if you want to go above 180 use world2Hires else worldHires

eez <- read.csv("EEZ_2016.csv") #All regions

minami<-read.csv("minami_tori.csv")

I1<-read.csv("I1.csv")
I2<-read.csv("I2.csv")
I3<-read.csv("I3.csv")
I4<-read.csv("I4.csv")
I5<-read.csv("I5.csv")
I6<-read.csv("I6_2.csv")
I7<-read.csv("I7_2.csv")
I8<-read.csv("I8.csv")
I9<-read.csv("I9.csv")
H4<-read.csv("H4.csv")
H5<-read.csv("H5.csv")


#____________________________________________________________________________________________________________  

# region labels
i3_x<-c(133,156,172)
i3_y<-c(16,17,19)
i7_x<-c(163,175,200)
i7_y<-c(-28,-28,-28)
reg_lab<-c("I1","I2",rep("I3",3),"I4","I5","I6",rep("I7",3),"I8","I9","H4","H5")
reg_labx<-c(141,161,i3_x,181,200,167,i7_x,173, 203,181,195)
reg_laby<-c(  3,  1,i3_y, 10, 12, 29,i7_y,-15, -13, -5, -5)

tmpreg <- data.frame(a=c(110,280,280,110,110), b=c(40,40,-40,-40,40))



windows(3000,2000)

# 
# p          <- ggplot() +
#   geom_polygon(data=H4, aes(H4[,4],H4[,2]), size=1, fill="brown") +
#   geom_polygon(data=H5, aes(H5[,4],H5[,2]), size=1 , fill="green") +
# 
#   geom_polygon(data=I1, aes(I1[,4],I1[,2]), size=1 , fill="blue") +
#   geom_polygon(data=I2, aes(I2[,4],I2[,2]), size=1 , fill="red") +
#   geom_polygon(data=I3, aes(I3[,4],I3[,2]), size=1 , fill="darkgreen") +
#   geom_polygon(data=I4, aes(I4[,4],I4[,2]), size=1 , fill="orange") +
#   geom_polygon(data=I5, aes(I5[,4],I5[,2]), size=1 , fill="magenta") +
#   geom_polygon(data=I6, aes(I6[,4],I6[,2]), size=1 , fill="cyan4") +
#   geom_polygon(data=I7, aes(I7[,4],I7[,2]), size=1 , fill="greenyellow") +
#   geom_polygon(data=I8, aes(I8[,4],I8[,2]), size=1 , fill="purple") +
#   geom_polygon(data=I9, aes(I9[,4],I9[,2]), size=1 , fill="yellow") +
# 
#   geom_polygon(data=minami, aes(minami[,2], minami[,3]), colour="white", fill="white") +
#   geom_polygon(data=eez, aes(eez[,2], eez[,3]), colour="white", fill=grecol) +
# 
#   geom_polygon(data=states, aes(x=long, y=lat, group = group),colour ="black",  fill="grey10" ) +
#   geom_segment(aes(x = c(230,230.5,210), y = c(-40,0,0), xend = c(230,209.7,210), yend = c(0,0,40)),  color="red", size=2)+ # the WCPFC boundary
# 
#   annotate("text", label = reg_lab, x = reg_labx, y = reg_laby, size = 5, colour = "black") +
# 
#   #coord_cartesian(xlim =c(140,210),ylim=c(-38,0)) + # for albacore
#   coord_cartesian(xlim =c(130,250),ylim=c(-35,30)) +
#   theme(panel.background = element_rect(fill=blucol),
#         plot.title = element_text(size = 20),
#         axis.text.x = element_text(vjust =1, size = 14, colour = "black"),
#         axis.text.y = element_text(hjust =1, size = 14,  colour = "black"),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14,angle=90)) +
#   xlab('Longitude') + ylab('Latitude')
# p


pl <- ggplot() +
  geom_polygon(data=tmpreg, aes(a,b), size=1, fill=alpha("skyblue", 0.3)) +  
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
  geom_polygon(data=eez, aes(x=POINT_X, y=POINT_Y), colour="black", fill=alpha("skyblue", 0.5)) +
  geom_path(data=eez, aes(x=POINT_X, y=POINT_Y), colour=alpha("black", 0.5)) + xlab("Longitude") + ylab("Latitude") +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="wheat", colour=alpha("black", 0.3)) +
  annotate("text", label = reg_lab, x = reg_labx, y = reg_laby, size = 5, colour = "black") +
  coord_equal(xlim=c(130,250), ylim=c(-35,30)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
        axis.text=element_text(size=14), axis.title=element_text(size=14), legend.position=c(0.05,0.25), legend.background=element_rect(fill=alpha("wheat", 0.5)))
print(pl)
savePlot("HighSeas_Area_Defs_Plot", type="png")
dev.off()













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
