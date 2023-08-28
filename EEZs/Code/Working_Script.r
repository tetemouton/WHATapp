library(rgdal) # for `ogrInfo()` and `readOGR()`
library(tools) # for `file_path_sans_ext()`
library(dplyr) # for `inner_join()`, `filter()`, `summarise()`, and the pipe operator (%>%)
library(ggplot2) # for `fortify()` and for plotting
library(sf) # for `point.in.polygon()` and `spDists()`
library(tidyr) # for `gather()`
library(readr) # for `write_tsv()`
library(tidyverse)
library(magrittr)
library(lubridate)
library(R4MFCL)

  theme_set(theme_bw())

  base.dir <- "C:/GitRep/WHATapp/EEZs"


  cnt.keep <- read.csv(file=paste0(base.dir, "/Data/PacCountries.csv"), header=TRUE)


  eez <- st_read(paste0(base.dir, "/Data/World_EEZ_Files/World_EEZ_v10_2018_0_360.shp"))

  area.df <- data.frame(Cnt=eez$Territory1, Area=eez$Area_km2)
  write.csv(area.df, file=paste0(base.dir, "/Data/EEZ_areas.csv"), row.names=FALSE)

  arch <- st_read(paste0(base.dir, "/Data/World_Arch_Files/eez_archipelagic_waters_v2_2018_0_360.shp"))

  windows()
    pl <- ggplot() + geom_sf(data=eez, aes(fill=Territory1)) + geom_sf(data=arch, fill="grey") +
                 theme(legend.position="none") + coord_sf(xlim=c(100,240), ylim=c(-30,30))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/Pacific_EEZs.png"), type="png")
  dev.off()
  
  
  land <- st_read(paste0(base.dir, "/Data/EEZ_land_union/EEZ_land_v2_201410.shp"))
  
  land$geometry <- st_set_crs(land$geometry + c(180,0), 4326)
  
  
  windows()
    pl <- ggplot() + geom_sf(data=land, aes(fill=Country)) +
                 theme(legend.position="none")#+ coord_sf(xlim=c(100,240), ylim=c(-30,30))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/World_Land_Union.png"), type="png")
  dev.off()
  
  
 #pac.keep <- c("Vanuatu","Marshall Islands","Indonesia","Papua New Guinea","Solomon Islands","Tuvalu","Gilbert Islands","Philippines","Fiji")
  
 #pac_arc <-   arch[arch$Territory1 %in% pac.keep,]
  
 tmp <- st_area(eez) 
 area.df.est <-  area.df %>% mutate(Est=tmp, Est.km=Est/1000000)
 write.csv(area.df.est, file=paste0(base.dir, "/Data/EEZ_areas_est.csv"), row.names=FALSE)
  
  
 tmp <- st_area(arch) 
 arc.df <- data.frame(area=arch$Territory1, Area=arch$Area_km2, Est=tmp) %>% mutate(Est.km=Est/1000000) 
 write.csv(arc.df, file=paste0(base.dir, "/Data/Arch_areas_est.csv"), row.names=FALSE) 
 

 pac.eez <- eez[eez$Territory1 %in% cnt.keep$Ctry,]
 pac.arc <- arch[arch$Territory1 %in% cnt.keep$Ctry,]
 
 
#____________________________________________________________________________________________________________
# Calculate and plot the areas for the PS scenarios - between 20S and 20N
 
# Deal with the complicated shape of the easter border which I assume follows the continental shelf mostly (made shapefile in google earth)
  tmppoly <- st_read(paste0(base.dir, "/Data/Eastern_Borders/Eastern_Borders-polygon.shp"))
   
 # WCPFC-CA bounds between -20 and 20
  bnd.bx <- c("POLYGON((105 20,210 20,210 -4,230 -4,230 -20,141 -20,141 -10,105 -10,105 20))") %>% 
            st_as_sfc(crs=st_crs(eez)) %>% 
            st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
 

  HS.bx <- c("POLYGON((123 20,128.5 7,140 0,155 0,160 -4,172 -12,170 -16,175 -20,175 -12,177 -8,171 -1,166 -1,166 0,169 0,168 3.5,159 4,155 0.2,151 5,130 7,140 12,143 12,144 19.99,146.5 19.95,146.5 10,160 10,163 14,170 16,180 -8,182 -11,186 -11,182 2,186 0,190 -1,190.5 -10,200 -8,200 -12,197 -17,203 -16,206.5 -10,201 -10,201 -7,202.5 -2,198 -2,199 3,195 7,200 7,204 2,204.5 -1,208 -10,212 -8,211 -12,220 -12,217 -6,221 -6,223 -10,222 -15,225 -20,230 -20,230 -4,210 -4,210 20,207 20,204 17,198 20,191 20,192.5 15,189 15,190 20,169 20,169 16.5,166 16.5,165.5 20,123 20))") %>% 
           st_as_sfc(crs=st_crs(eez)) %>% 
           st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
 
 
 #wcp.bnds <- st_intersection(tmppoly, bnd.bx)
  wcp.bnds <- st_difference(bnd.bx, tmppoly)

  
  windows(3000,2000)
    pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
                 geom_sf(data=tmppoly, fill=alpha("blue", 0.2)) + theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_BuildingBlocks.png"), type="png")
 
    pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                 theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Areas_WOverlap.png"), type="png")
    
    pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                 geom_sf(data=pac.arc, fill=alpha("black",0.4)) + theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Areas-Arch_WOverlap.png"), type="png")
    
    
    wcp.eezs <- st_intersection(pac.eez, wcp.bnds)
 
    pl <- ggplot() + geom_sf(data=wcp.eezs, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                 theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Areas_NoOverlap.png"), type="png")
    
    
    wcp.arcs <- st_intersection(wcp.bnds, pac.arc)
    
    pl <- ggplot() + geom_sf(data=pac.arc, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                 theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-22,22))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Arch_WOverlap.png"), type="png")
   
    pl <- ggplot() + geom_sf(data=wcp.arcs, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                 theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Arch_NoOverlap.png"), type="png")

    pl <- ggplot() + geom_sf(data=wcp.eezs, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                 geom_sf(data=wcp.arcs, fill=alpha("black",0.4)) + theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Areas-Arch_NoOverlap.png"), type="png")
    
  dev.off()
     
   # Output the areas of EEZs within this 20-20 region
   tmp <- st_area(wcp.eezs) 
   tmp.ars <-  data.frame(Cnt=wcp.eezs$Territory1) %>% mutate(Est=tmp, Est.km=Est/1000000)
   write.csv(tmp.ars, file=paste0(base.dir, "/Data/EEZ_areas_est_20-20WCPFCca.csv"), row.names=FALSE) 
   
   # Output the archepelagic areas of EEZs within this 20-20 region (this is to subtract from the above to get relevant area)
   tmp <- st_area(wcp.arcs) 
   tmp.ars <-  data.frame(Cnt=wcp.arcs$Territory1) %>% mutate(Est=tmp, Est.km=Est/1000000)
   write.csv(tmp.ars, file=paste0(base.dir, "/Data/ARC_areas_est_20-20WCPFCca.csv"), row.names=FALSE) 
   
   
#____________________________________________________________________________________________________________   
# Calculate and plot the areas for the LL scenarios - the wider WCPO  
   
   
  # Deal with the complicated shape of the easter border which I assume follows the continental shelf mostly (made shapefile in google earth)
  tmppoly <- st_read(paste0(base.dir, "/Data/Eastern_Borders/Eastern_Borders-polygon.shp"))
   
  # WCPFC-CA bounds between -20 and 20
  bnd.bx.ll <- c("POLYGON((105 20,110 20,110 50,210 50,210 -4,230 -4,230 -50,141 -50,141 -10,105 -10,105 20))") %>% 
               st_as_sfc(crs=st_crs(eez)) %>% 
               st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   
   
  # HS.bx <- c("POLYGON((123 20,128.5 7,140 0,155 0,160 -4,172 -12,170 -16,175 -20,175 -12,177 -8,168 3.5,159 4,155 0.2,151 5,130 7,140 12,143 12,144 19.99,146.5 19.95,146.5 10,160 10,163 14,170 16,180 -8,182 -11,186 -11,182 2,186 0,190 -1,190.5 -10,200 -8,200 -12,197 -17,203 -16,206.5 -10,201 -10,201 -7,202.5 -2,198 -2,199 3,195 7,200 7,204 2,204.5 -1,208 -10,212 -8,211 -12,220 -12,217 -6,221 -6,223 -10,222 -15,225 -20,230 -20,230 -4,210 -4,210 20,207 20,204 17,198 20,191 20,192.5 15,189 15,190 20,169 20,169 16.5,166 16.5,165.5 20,123 20))") %>% 
  #          st_as_sfc(crs=st_crs(eez)) %>% 
  #          st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   
   
  #wcp.bnds <- st_intersection(tmppoly, bnd.bx)
  wcp.bnds.ll <- st_difference(bnd.bx.ll, tmppoly)
   
   
  windows(3000,3000)
    pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=bnd.bx.ll, fill=alpha("grey", 0.2)) +
                 geom_sf(data=tmppoly, fill=alpha("blue", 0.2)) + theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-50,50))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_LL_BuildingBlocks.png"), type="png")
   
    pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                 theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-50,50))
    print(pl)
    savePlot(file=paste0(base.dir, "/Plots/EEZareas_LL_Areas_WOverlap.png"), type="png")
   
   pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                geom_sf(data=pac.arc, fill=alpha("black",0.4)) + theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-50,50))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/EEZareas_LL_Areas-Arch_WOverlap.png"), type="png")


   wcp.eezs.ll <- st_intersection(pac.eez, wcp.bnds.ll)

   pl <- ggplot() + geom_sf(data=wcp.eezs.ll, aes(fill=Territory1)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-50,50))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/EEZareas_LL_Areas_NoOverlap.png"), type="png")


   wcp.arcs.ll <- st_intersection(wcp.bnds.ll, pac.arc)

   pl <- ggplot() + geom_sf(data=pac.arc, aes(fill=Territory1)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-50,50))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/EEZareas_LL_Arch_WOverlap.png"), type="png")

   pl <- ggplot() + geom_sf(data=wcp.arcs, aes(fill=Territory1)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-50,50))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/EEZareas_LL_Arch_NoOverlap.png"), type="png")

   pl <- ggplot() + geom_sf(data=wcp.eezs.ll, aes(fill=Territory1)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                geom_sf(data=wcp.arcs.ll, fill=alpha("black",0.4)) + theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-50,50))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/EEZareas_LL_Areas-Arch_NoOverlap.png"), type="png")

   dev.off()

   # Output the areas of EEZs within this 20-20 region
   tmp <- st_area(wcp.eezs.ll)
   tmp.ars <-  data.frame(Cnt=wcp.eezs.ll$Territory1) %>% mutate(Est=tmp, Est.km=Est/1000000)
   write.csv(tmp.ars, file=paste0(base.dir, "/Data/EEZ_areas_est_LLWCPFCca.csv"), row.names=FALSE)

   # Output the archepelagic areas of EEZs within this 20-20 region (this is to subtract from the above to get relevant area)
   tmp <- st_area(wcp.arcs.ll)
   tmp.ars <-  data.frame(Cnt=wcp.arcs.ll$Territory1) %>% mutate(Est=tmp, Est.km=Est/1000000)
   write.csv(tmp.ars, file=paste0(base.dir, "/Data/ARC_areas_est_LLWCPFCca.csv"), row.names=FALSE)


#____________________________________________________________________________________________________________ 
# Save whole workspace so can be quickly reloaded if needed  
  save.image(file=paste0(base.dir, "/Data/Geo_Data_Objects_tmp.RData"))   
   
  load(paste0(base.dir, "/Data/Geo_Data_Objects_tmp.RData"))    
   
   
#____________________________________________________________________________________________________________    
# Calculate adjacency of purse seine based on random sets - a proxy for EEZ buffer area

   min.dist <- 100   # Only keep a sample if it is less than this many nautical miles of the nearest EEZ
   min.metres <- 1852*min.dist   # Convert to kilometres as this is the unit used by R
   
   comb.eezs <- st_union(wcp.eezs)
   
   tmp.HS <- st_difference(wcp.bnds, comb.eezs)
   #tmp.HS <- st_intersection(wcp.bnds, wcp.eezs)
   
   #all.HS <- st_intersection(HS.bx, tmp.HS)
   #save(all.HS, file="C:/EEZs/Data/HS_regions_20-20.RData")
   
   load(paste0(base.dir, "/Data/HS_regions_20-20.RData"))

   
   windows(3000,2000)
     pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.2)) + geom_sf(data=wcp.bnds, fill=alpha("wheat", 0.3), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=wcp.eezs, fill=alpha(grey(0.9), 0.99), colour=alpha("black", 0.4)) + 
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas.png"), type="png")
   dev.off()
   
   # wcp.HS <- st_difference(wcp.bnds, tmp.HS)
   # 
   # 
   # pl <- ggplot() + geom_sf(data=wcp.HS, fill=alpha("blue", 0.1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
   #   theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-22,22))
   # print(pl)
   
   
   
   # pl <- ggplot() + geom_sf(data=wcp.eezs, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
   #              theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-22,22))
   # print(pl)
   # savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_AreasEEZs.png"), type="png")
   
   tmp.buf <- st_buffer(wcp.eezs, 1)

   windows(3000,2000)      
   pl <- ggplot() + geom_sf(data=tmp.buf, fill=alpha("blue", 0.3)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                geom_sf(data=wcp.eezs, fill=alpha("red", 0.5)) + theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_Buffermap.png"), type="png")
   dev.off()
   
   
   # Test of how to separate, plot and analyses points in the high seas
   Nsamp <- 8000
   
   
   rnd.pts <- data.frame(longitude=runif(Nsamp, 123, 231),
                         latitude=runif(Nsamp, -20.5, 20.5))
   
   
   pt.sf <- st_as_sf(rnd.pts, coords=c("longitude","latitude"), crs=4326, agr="constant")
   
   windows(3000,2000)   
   pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                geom_sf(data=pt.sf, colour=alpha("red", 0.7)) +
                theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_AllRandPnts.png"), type="png")
   dev.off()
   
   
   pts.in <- st_within(pt.sf, all.HS, sparse=FALSE)
   
   pts.keep <- pt.sf[pts.in[,1],]
   
   windows(3000,2000)   
   pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                geom_sf(data=wcp.eezs, fill=alpha("grey", 0.8), colour=alpha("black", 0.4)) + 
                geom_sf(data=pt.sf, colour=alpha("red", 0.7)) + geom_sf(data=pts.keep, colour=alpha("green", 0.95)) +
                theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_AllPnts-In-Out.png"), type="png")
   dev.off()
   
   windows(3000,2000)
   pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.3)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.1), colour=alpha("black",0.6), size=1) +
                geom_sf(data=wcp.eezs, fill=alpha("grey", 0.8), colour=alpha("black", 0.4)) + geom_sf(data=pts.keep, fill=alpha("red", 0.99), colour="red", shape=21, size=1) +
                theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_JustInPnts.png"), type="png")
   dev.off()

   wcp.eezs.sl <- wcp.eezs[1:(0.5*length(wcp.eezs$Territory1)),]
   
   
   tmmp <- st_distance(pts.keep, wcp.eezs.sl)

   close.eez <- data.frame(eez=as.character(wcp.eezs$Territory1)[apply(tmmp, 1, function(x) which(x == min(x)))], Dist=apply(tmmp, 1, min))
   write.csv(close.eez, paste0(base.dir, "/Plots/Closest_EEZ_Random.csv"))
   
   cl.eez.tab <- close.eez %>% group_by(eez) %>% summarise(Nsamps=n()) %>% mutate(Nsamps/sum(Nsamps))
   write.csv(cl.eez.tab, paste0(base.dir, "/Plots/Closest_EEZ_Random_Tab.csv"))
   
   
   pts.close <- pts.keep[close.eez$Dist <= min.metres,]
   
   
   windows(3000,2000)   
   pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
     geom_sf(data=pts.keep, colour=alpha("green", 0.95)) + geom_sf(data=pts.close, colour=alpha("red", 0.99)) +
     geom_sf(data=wcp.eezs, fill=alpha("grey", 0.8), colour=alpha("black", 0.4)) + 
     theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_AllPnts-In-Out_Close.png"), type="png")
   dev.off()
   
   
   close.eez.lim <- filter(close.eez, Dist <= min.metres)
   write.csv(close.eez.lim, paste0(base.dir, "/Plots/Closest_EEZ_Random_lim.csv"))
   
   cl.eez.tab.lim <- close.eez.lim %>% group_by(eez) %>% summarise(Nsamps=n()) %>% mutate(Nsamps/sum(Nsamps))
   write.csv(cl.eez.tab, paste0(base.dir, "/Plots/Closest_EEZ_Random_Tab_lim.csv"))
   
   
#____________________________________________________________________________________________________________    
# Calculate adjacency based on actual sets available from operational database
   
   yrs.keep <- 2016:2018
   
   
   # Read in the logsheet data
   load("C:/Phoenix_Isl_Analyses/2019/Extracts/OPPS_EEZ_DATA.RData", verbose=TRUE)      # Load set-level operational LL data
   load("C:/Phoenix_Isl_Analyses/2019/Extracts/OPPS_EEZ_TRIP-DATA.RData", verbose=TRUE) # Load trip-level operational LL data  
   
   opdat  %<>% mutate(latd=as.numeric(substring(lat_long, 1, 2)), latd=latd+as.numeric(substring(lat_long, 3, 8))/60, latd=ifelse(substring(lat_long, 9, 9) == "S", -1 * latd, latd),
                      lond=as.numeric(substring(lon_long, 1, 3)), lond=lond+as.numeric(substring(lon_long, 4, 9))/60, lond=ifelse(substring(lon_long, 10, 10) == 'W', 360-lond, lond),
                      lubdt=ymd(logdate), yy=year(lubdt), mm=month(lubdt), dd=day(lubdt))
   
   
   opps <- left_join(opdat, opdat_trip, by="trip_id", suffix=c("","y")) %>% mutate(ez_rep=ifelse(ez_id %in% c("GL","PX","LN"), "KI", as.character(ez_id)))
   
   opps.select <- opps %>% filter(yy %in% yrs.keep, s_act_id %in% 1:2) %>% select(lond, latd) %>% rename(longitude=lond, latitude=latd)
   
   ps.pnts <- st_as_sf(opps.select, coords=c("longitude","latitude"), crs=4326, agr="constant")
   
   
   pts.in <- st_within(ps.pnts, all.HS, sparse=FALSE)

   pts.keep <- ps.pnts[pts.in[,1],]

   windows(3000,2000)   
     pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=ps.pnts, colour=alpha("red", 0.7)) + geom_sf(data=pts.keep, colour=alpha("green", 0.95)) +
                  geom_sf(data=wcp.eezs, fill=alpha("grey", 0.8), colour=alpha("black", 0.4)) + 
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_LogSheets-In-Out.png"), type="png")
   dev.off()
   
   windows(3000,2000)
     pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.3)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.1), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=wcp.eezs, fill=alpha("grey", 0.8), colour=alpha("black", 0.4)) + geom_sf(data=pts.keep, fill=alpha("red", 0.99), colour="red", shape=21, size=1) +
                  geom_sf(data=wcp.eezs, fill=alpha("grey", 0.8), colour=alpha("black", 0.4)) + 
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_Logsheets-JustInPnts.png"), type="png")
   dev.off()
   
   wcp.eezs.sl <- wcp.eezs[1:(0.5*length(wcp.eezs$Territory1)),]
   
   
   tmmp <- st_distance(pts.keep, wcp.eezs.sl)
   
   close.eez <- data.frame(eez=as.character(wcp.eezs$Territory1)[apply(tmmp, 1, function(x) which(x == min(x)))], Dist=apply(tmmp, 1, min))
   write.csv(close.eez, paste0(base.dir, "/Plots/Closest_EEZ_Logsheets.csv"))
   
   cl.eez.tab <- close.eez %>% group_by(eez) %>% summarise(Nsamps=n()) %>% mutate(Nsamps/sum(Nsamps))
   write.csv(cl.eez.tab, paste0(base.dir, "/Plots/Closest_EEZ_Logsheets_Tab.csv"))
   
   
   pts.close <- pts.keep[close.eez$Dist <= min.metres,]
   
   
   windows(3000,2000)   
     pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=pts.keep, colour=alpha("green", 0.95)) + geom_sf(data=pts.close, colour=alpha("red", 0.99)) +
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-22,22))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_20-20_Areas_AllPnts-In-Out_Close_Logsheets.png"), type="png")
   dev.off()
   
   
   close.eez.lim <- filter(close.eez, Dist <= min.metres)
   write.csv(close.eez.lim, paste0(base.dir, "/Plots/Closest_EEZ_Logsheets_lim.csv"))
   
   cl.eez.tab.lim <- close.eez.lim %>% group_by(eez) %>% summarise(Nsamps=n()) %>% mutate(Nsamps/sum(Nsamps))
   write.csv(cl.eez.tab, paste0(base.dir, "/Plots/Closest_EEZ_Logsheets_Tab_lim.csv"))
   
   
   
#____________________________________________________________________________________________________________ 
# Calculate adjacency of longline based on random sets - a proxy for EEZ buffer area
   
   min.dist <- 100   # Only keep a sample if it is less than this many nautical miles of the nearest EEZ
   min.metres <- 1852*min.dist   # Convert to metres as this is the unit used by R
   
   
   HS.bx.ll <- c("POLYGON((123 20,128.5 7,140 0,155 -1,160 -4,172 -12,170 -16,175 -20,175 -12,177 -8,171 -1,166 -1,166 0,169 0,168 3.5,159 4,155 0.2,151 5,130 7,140 12,143 12,143 22,146.5 22,146.5 10,160 10,163 14,170 16,180 -8,182 -11,186 -11,182 2,186 0,190 -1,190.5 -10,200 -8,200 -12,197 -17,203 -16,206.5 -10,201 -10,201 -7,202.5 -2,198 -2,199 3,195 7,200 7,204 2,204.5 -1,208 -10,212 -8,211 -12,220 -12,217 -6,221 -6,223 -10,222 -15,226 -22,229 -22,229 -26,220 -23,215 -23,217 -29,212 -26,192 -18,185 -24,175 -21,169 -23,171 -33.5,179.5 -35,180 -27,184 -27,179.9 -33,180.5 -41,186 -43,183 -46,172 -47,162 -48.5,172 -40,169.5 -36,168 -24,162 -22,158 -24,154 -25,160 -30,160 -33,155 -30,148 -44,143 -44,141 -41,141 -50,230 -50,230 -4,210 -4,210 50,150 50,150 44,143 39,143 23,139 23,139 32,134 31,132 24,138 20,137 18,134 22,129 24,126 22,123 20),(156 26,152 26,152 23,156 23,156 26),(165 21,168 21,168 18,165 18,165 21),(189 18,193 18,193 15,189 15,189 18),(180 27,180 31,208 21,206 17,200 19,180 27))") %>% 
                 st_as_sfc(crs=st_crs(eez)) %>% 
                 st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)    
       
       
   comb.eezs.ll <- st_union(wcp.eezs.ll)
   
   tmp.HS.ll <- st_difference(HS.bx.ll, comb.eezs.ll)
   #tmp.HS <- st_intersection(wcp.bnds, wcp.eezs)
   
   all.HS.ll <- st_intersection(HS.bx.ll, tmp.HS.ll)
   save(all.HS, file="C:/EEZs/Data/HS_regions_longline.RData")
   
   load(paste0(base.dir, "/Data/HS_regions_longline.RData"))
   

   comb.eezs.ll <- st_union(wcp.eezs)
   
   tmp.HS <- st_difference(wcp.bnds.ll, comb.eezs.ll)
   
   all.HS.ll <- st_intersection(HS.bx.ll, tmp.HS)
   save(all.HS.ll, file="C:/EEZs/Data/HS_regions_Longline.RData")
   
  
   windows(3000,2000)
     pl <- ggplot() + geom_sf(data=wcp.bnds.ll, fill=alpha("wheat", 0.3), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=all.HS.ll, fill=alpha("red", 0.7), colour=alpha("black", 0.4)) +
                  geom_sf(data=HS.bx.ll, fill=alpha("blue", 0.2)) +
                  geom_sf(data=comb.eezs.ll, fill=alpha("red", 0.1)) + 
                  theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-50,50))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_LL_Areas.png"), type="png")
   dev.off()
   
   windows(3000,2000)
     pl <- ggplot() + geom_sf(data=wcp.bnds.ll, fill=alpha("wheat", 0.3), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=all.HS.ll, fill=alpha("blue", 0.2), colour=alpha("black", 0.4)) +
                  #geom_sf(data=HS.bx.ll, fill=alpha("blue", 0.2)) +
                  geom_sf(data=wcp.eezs.ll, fill=alpha(grey(0.9), 0.99)) + 
                  theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-50,50))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_LL_Areas_JustHS.png"), type="png")
   dev.off()


   # Test of how to separate, plot and analyses points in the high seas
   
   
   Nsamp <- 5000
   
   
   rnd.pts <- data.frame(longitude=runif(Nsamp, 123, 231),
                         latitude=runif(Nsamp, -50.5, 50.5))
   
   
   pt.sf <- st_as_sf(rnd.pts, coords=c("longitude","latitude"), crs=4326, agr="constant")
   
   windows(3000,2000)   
     pl <- ggplot() + geom_sf(data=all.HS.ll, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=pt.sf, colour=alpha("red", 0.7)) +
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-52,52))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_Longline_Areas_AllRandPnts.png"), type="png")
   dev.off()
   
   wcp.eezs.ll <- wcp.eezs.ll[1:(0.5*length(wcp.eezs.ll$Territory1)),]
   
   pts.in <- st_within(pt.sf, all.HS.ll, sparse=FALSE)
   
   pts.keep <- pt.sf[pts.in[,1],]
   
   windows(3000,2000)   
     pl <- ggplot() + geom_sf(data=all.HS.ll, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=pt.sf, colour=alpha("red", 0.7)) + geom_sf(data=pts.keep, colour=alpha("green", 0.95)) +
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-52,52))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_Longline_Areas_AllPnts-In-Out.png"), type="png")
   dev.off()
   
   windows(3000,2000)
     pl <- ggplot() + geom_sf(data=all.HS.ll, fill=alpha("blue", 0.3)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.1), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=wcp.eezs.ll, fill=alpha("grey", 0.8), colour=alpha("black", 0.4)) + geom_sf(data=pts.keep, fill=alpha("red", 0.99), colour="red", shape=21, size=1) +
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-52,52))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_Longline_Areas_JustInPnts.png"), type="png")
   dev.off()
   
   wcp.eezs.sl <- wcp.eezs.ll#[1:(0.5*length(wcp.eezs.ll$Territory1)),]
   
   
   tmmp <- st_distance(pts.keep, wcp.eezs.sl)
   
   close.eez <- data.frame(eez=as.character(wcp.eezs.ll$Territory1)[apply(tmmp, 1, function(x) which(x == min(x)))], Dist=apply(tmmp, 1, min))
   write.csv(close.eez, paste0(base.dir, "/Plots/Closest_EEZ_Random_Longline.csv"))
   
   cl.eez.tab <- close.eez %>% group_by(eez) %>% summarise(Nsamps=n()) %>% mutate(Nsamps/sum(Nsamps))
   write.csv(cl.eez.tab, paste0(base.dir, "/Plots/Closest_EEZ_Random_Tab_Longline.csv"))
   
   
   pts.close <- pts.keep[close.eez$Dist <= min.metres,]
   
   
   windows(3000,2000)   
     pl <- ggplot() + geom_sf(data=all.HS.ll, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds.ll, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
                  geom_sf(data=pts.keep, colour=alpha("green", 0.95)) + geom_sf(data=pts.close, colour=alpha("red", 0.99)) +
                  theme(legend.position="none") + coord_sf(xlim=c(112,228), ylim=c(-52,52))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/HSareas_Longline_Areas_AllPnts-In-Out_Close.png"), type="png")
   dev.off()
   
   
   close.eez.lim <- filter(close.eez, Dist <= min.metres)
   write.csv(close.eez.lim, paste0(base.dir, "/Plots/Closest_EEZ_Random_lim_Longline.csv"))
   
   cl.eez.tab.lim <- close.eez.lim %>% group_by(eez) %>% summarise(Nsamps=n()) %>% mutate(Nsamps/sum(Nsamps))
   write.csv(cl.eez.tab, paste0(base.dir, "/Plots/Closest_EEZ_Random_Tab_lim_Longline.csv"))


#____________________________________________________________________________________________________________
# Set up the stock assessment boundaries and calculation proportion of stock in each EEZ - for SKJ PS
   
   # WCPFC-CA bounds between -20 and 20
   
   reg3 <- c("POLYGON((120 30,145 30,145 10,130 10,130 20,120 20,120 30))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   reg4 <- c("POLYGON((145 30,210 30,210 10,145 10,145 30))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   reg5 <- c("POLYGON((110 20,130 20,130 10,140 10,140 -20,110 -20,110 20))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   reg6 <- c("POLYGON((140 0,155 0,155 -5,160 -5,160 -20,140 -20,140 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   reg7 <- c("POLYGON((140 10,170 10,170 -20,160 -20,160 -5,155 -5,155 0,140 0,140 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   reg8 <- c("POLYGON((170 10,210 10,210 -20,170 -20,170 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   regOvLap <- c("POLYGON((210 -4,230 -4,230 -20,210 -20,210 -4))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   
   
   reg.ars.fn <- as.data.frame(rbind(st_area(reg3),st_area(reg4),st_area(reg5),st_area(reg6),st_area(reg7),st_area(reg8),st_area(regOvLap))) %>%
                               mutate(Est.km=V2/1000000, Reg=c("reg3","reg4","reg5","reg6","reg7","reg8","regOvLap"))
   write.csv(reg.ars.fn, file=paste0(base.dir, "/Plots/RegAreas/Overall_SKJ8_EEZ_areas.csv"), row.names=FALSE) 
   

  reg.areas <- function(dat=reg3, datnm="reg3"){
   
   # Full EEZs
   tmpez <- st_intersection(wcp.eezs, dat)
   tmp <- st_area(tmpez) 
   tmp.ars <-  data.frame(Cnt=as.character(tmpez$Territory1)) %>% mutate(Est=tmp, Est.km=Est/1000000)
   
   #tmp.ars <- tmp.ars[match(unique(tmp.ars$Cnt), tmp.ars$Cnt),]
   
   write.csv(tmp.ars, file=paste0(base.dir, "/Plots/RegAreas/SKJ8_EEZ_areas_", datnm, ".csv"), row.names=FALSE) 
   
   
   windows(3000,2000)
     pl <- ggplot() + geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
                 geom_sf(data=dat, colour=alpha("black", 0.9), size=2) + geom_sf(data=tmpez, aes(fill=Territory1)) +
                 theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
     print(pl)
     savePlot(file=paste0(base.dir, "/Plots/SKJ8_EEZ_Map_", datnm, ".png"), type="png")
   dev.off()
   
   # Archepelagic areas
   tmpez <- st_intersection(wcp.arcs, dat)
   tmp <- st_area(tmpez) 
   tmp.ars <-  data.frame(Cnt=as.character(tmpez$Territory1)) %>% mutate(Est=tmp, Est.km=Est/1000000)
   
   #tmp.ars <- tmp.ars[match(unique(tmp.ars$Cnt), tmp.ars$Cnt),]
   
   write.csv(tmp.ars, file=paste0(base.dir, "/Plots/RegAreas/SKJ8_EEZ_ARC_areas_", datnm, ".csv"), row.names=FALSE) 
   
   
   windows(3000,2000)
   pl <- ggplot() + geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
     geom_sf(data=dat, colour=alpha("black", 0.9), size=2) + geom_sf(data=tmpez, aes(fill=Territory1)) +
     theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
   print(pl)
   savePlot(file=paste0(base.dir, "/Plots/SKJ8_EEZ_ARC_Map_", datnm, ".png"), type="png")
   dev.off()
   
  }
   
  reg.areas(dat=reg3, datnm="reg3")
  reg.areas(dat=reg4, datnm="reg4")
  reg.areas(dat=reg5, datnm="reg5")
  reg.areas(dat=reg6, datnm="reg6")
  reg.areas(dat=reg7, datnm="reg7")
  reg.areas(dat=reg8, datnm="reg8")
  reg.areas(dat=regOvLap, datnm="regOvLap")
   
   
#____________________________________________________________________________________________________________
# Set up the stock assessment boundaries and calculation proportion of stock in each EEZ - for BET LL
  
  # WCPFC-CA bounds between -40 and 50
  
  reg1.ll <- c("POLYGON((120 50,170 50,170 10,140 10,140 20,120 20,120 50))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg2.ll <- c("POLYGON((170 50,210 50,210 10,170 10,170 50))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg3.ll <- c("POLYGON((140	10,170	10,170	-10,160	-10,160	-5,155	-5,155	0,140	0,140	10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg4.ll <- c("POLYGON((170 10,210 10,210 -10,170 -10,170 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg5.ll <- c("POLYGON((140 -10,170 -10,170 -40,140 -40,140 -20,150 -20,150 -15,140 -15,140 -10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg6.ll <- c("POLYGON((170 -10,210 -10,210 -40,170 -40,170 -10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg7.ll <- c("POLYGON((110 20,140 20,140 -10,110 -10,110 20))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg8.ll <- c("POLYGON((140 0,155 0,155 -5,160 -5,160 -10,140 -10,140 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  reg9.ll <- c("POLYGON((140 -15,150 -15,150 -20,140 -20,140 -15))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  regOvLap.ll <- c("POLYGON((210 -4,230 -4,230 -40,210 -40,210 -4))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  
  
  reg.ars.fn <- as.data.frame(rbind(st_area(reg1.ll),st_area(reg2.ll),st_area(reg3.ll),st_area(reg4.ll),st_area(reg5.ll),st_area(reg6.ll),st_area(reg7.ll),st_area(reg8.ll),st_area(reg9.ll),st_area(regOvLap.ll))) %>%
                              mutate(Est.km=V2/1000000, Reg=c("reg1.ll","reg2.ll","reg3.ll","reg4.ll","reg5.ll","reg6.ll","reg7.ll","reg8.ll","reg9.ll","regOvLap.ll"))
  write.csv(reg.ars.fn, file=paste0(base.dir, "/Plots/RegAreas/Overall_BET_EEZ_areas.csv"), row.names=FALSE) 
  
  
  reg.areas.ll <- function(dat=reg3.ll, datnm="reg3.ll"){
    
    # Full EEZs
    tmpez <- st_intersection(wcp.eezs.ll, dat)
    tmp <- st_area(tmpez) 
    tmp.ars <-  data.frame(Cnt=as.character(tmpez$Territory1)) %>% mutate(Est=tmp, Est.km=Est/1000000)
    
    #tmp.ars <- tmp.ars[match(unique(tmp.ars$Cnt), tmp.ars$Cnt),]
    
    write.csv(tmp.ars, file=paste0(base.dir, "/Plots/RegAreas/BET_EEZ_areas_LL_", datnm, ".csv"), row.names=FALSE) 
    
    
    windows(3000,2000)
      pl <- ggplot() + geom_sf(data=bnd.bx.ll, fill=alpha("grey", 0.2)) +
                   geom_sf(data=dat, colour=alpha("black", 0.9), size=2) + geom_sf(data=tmpez, aes(fill=Territory1)) +
                   theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-45,50))
      print(pl)
      savePlot(file=paste0(base.dir, "/Plots/BET_EEZ_Map_LL_", datnm, ".png"), type="png")
    dev.off()
    
    # Archepelagic areas
    tmpez <- st_intersection(wcp.arcs.ll, dat)
    tmp <- st_area(tmpez) 
    tmp.ars <-  data.frame(Cnt=as.character(tmpez$Territory1)) %>% mutate(Est=tmp, Est.km=Est/1000000)
    
    #tmp.ars <- tmp.ars[match(unique(tmp.ars$Cnt), tmp.ars$Cnt),]
    
    write.csv(tmp.ars, file=paste0(base.dir, "/Plots/RegAreas/BET_EEZ_ARC_areas_LL_", datnm, ".csv"), row.names=FALSE) 
    
    
    windows(3000,2000)
      pl <- ggplot() + geom_sf(data=bnd.bx.ll, fill=alpha("grey", 0.2)) +
                   geom_sf(data=dat, colour=alpha("black", 0.9), size=2) + geom_sf(data=tmpez, aes(fill=Territory1)) +
                   theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-45,50))
      print(pl)
      savePlot(file=paste0(base.dir, "/Plots/BET_EEZ_ARC_Map_LL_", datnm, ".png"), type="png")
    dev.off()
    
  }
  
  reg.areas.ll(dat=reg1.ll, datnm="reg1.ll")
  reg.areas.ll(dat=reg2.ll, datnm="reg2.ll")
  reg.areas.ll(dat=reg3.ll, datnm="reg3.ll")
  reg.areas.ll(dat=reg4.ll, datnm="reg4.ll")
  reg.areas.ll(dat=reg5.ll, datnm="reg5.ll")
  reg.areas.ll(dat=reg6.ll, datnm="reg6.ll")
  reg.areas.ll(dat=reg7.ll, datnm="reg7.ll")
  reg.areas.ll(dat=reg8.ll, datnm="reg8.ll")
  reg.areas.ll(dat=reg9.ll, datnm="reg9.ll")
  reg.areas.ll(dat=regOvLap.ll, datnm="regOvLap.ll")   
   
   
#____________________________________________________________________________________________________________   
# Spawning biomass densities by region for the biomass in EEZ allocation scenario   
  
  skj.rep <- read.rep("//penguin/assessments/skj/2019/assessment/Diagnostic/plot-07.par.rep")
  skj.sb <- skj.rep$AdultBiomass
  reg.mu.skj <- apply(skj.sb[169:188,], 2, mean)
  write.csv(reg.mu.skj, file=paste0(base.dir, "/Plots/RegAreas/Overall_SKJ_EEZ_SB-densities.csv"), row.names=FALSE) 
  
  
  bet.rep <- read.rep("//penguin/assessments/bet/2018/assessment/RefCase/plot-out.par.rep")
  bet.sb <- bet.rep$AdultBiomass
  reg.mu.bet <- apply(bet.sb[237:256,], 2, mean) 
  write.csv(reg.mu.bet, file=paste0(base.dir, "/Plots/RegAreas/Overall_BET_EEZ_SB-densities.csv"), row.names=FALSE) 
  

#____________________________________________________________________________________________________________   
   
   
   
   
   skj8.bnds <- st_multipolygon(list(reg3,reg4,reg5,reg6,reg7,reg8)) %>% st_as_sf()
   tmp <- st_set_crs(skj8.bnds, st_crs(eez))
     
   skj8.bnds <- st_multipolygon(list(reg3,reg4,reg5,reg6,reg7,reg8)) %>% 
                st_set_crs(st_crs(eez)) %>% 
                st_sf(field=c('xy'), geoms = ., stringsAsFactors=FALSE)
   
   
   skj8.bnds <- c("MULTIPOLYGON((105 20,210 20,210 -4,230 -4,230 -20,141 -20,141 -10,105 -10,105 20),(120 15,160 15,160 10,120 10,120 15))") %>% 
                st_as_sfc(crs=st_crs(eez)) %>% 
                st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   
   skj8.bnds <- c("MULTIPOLYGON((105 20,210 20,210 -4,230 -4,230 -20,141 -20,141 -10,105 -10,105 20),(120 15,160 15,160 10,120 10,120 15))") %>% 
     st_as_sfc(crs=st_crs(eez)) %>% 
     st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   
   windows(3000,2000)
   pl <- ggplot() + geom_sf(data=reg3) +#geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
     theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
   print(pl)
   
   
 
   
   #wcp.bnds <- st_intersection(tmppoly, bnd.bx)
   wcp.bnds <- st_difference(bnd.bx, tmppoly)
   
   
   windows(3000,2000)
   pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
     geom_sf(data=tmppoly, fill=alpha("blue", 0.2)) + theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
   print(pl)
   #savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_BuildingBlocks.png"), type="png")
   
   pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
     theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
   print(pl)
   #savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Areas_WOverlap.png"), type="png")
   
   pl <- ggplot() + geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
     geom_sf(data=pac.arc, fill=alpha("black",0.4)) + theme(legend.position="none") + coord_sf(xlim=c(105,230), ylim=c(-22,22))
   print(pl)
   #savePlot(file=paste0(base.dir, "/Plots/EEZareas_20-20_Areas-Arch_WOverlap.png"), type="png")
   
   
   wcp.eezs <- st_intersection(pac.eez, wcp.bnds)
   
   
#____________________________________________________________________________________________________________    
   
   
   
   
   
   
   
   
   
   
   pts.in <- st_within(ps.pnts, all.HS, sparse=FALSE)
   
   pts.keep <- ps.pnts[pts.in[,1],]
   
   
   pl <- ggplot() + geom_sf(data=wcp.eezs, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
     geom_sf(data=ps.pnts, colour=alpha("red", 0.3)) +
     theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-22,22))
   print(pl)
   
   
   
   
   pl <- ggplot() + geom_sf(data=wcp.eezs, aes(fill=Territory1), alpha=0.1) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
     geom_sf(data=pts.keep, colour=alpha("blue", 0.05)) +
     theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-22,22))
   print(pl)
   savePlot(file="C:/EEZs/Plots/HS_sets_adjacency_scenario.png", type="png")
   
   wcp.eezs
   
   
   tmmp <- st_distance(pts.keep, wcp.eezs)
   
   which(tmmp[1,] == min(tmmp[1,]))
   
   
   
   
   # Nsamp <- 1000
   # 
   # 
   # rnd.pts <- data.frame(longitude=runif(Nsamp, 123, 231),
   #                       latitude=runif(Nsamp, -20.5, 20.5))
   # 
   # 
   # pt.sf <- st_as_sf(rnd.pts, coords=c("longitude","latitude"), crs=4326, agr="constant")
   # 
   # 
   # pl <- ggplot() + geom_sf(data=all.HS, fill=alpha("blue", 0.7)) + geom_sf(data=wcp.bnds, fill=alpha("grey", 0.2), colour=alpha("black",0.6), size=1) +
   #   geom_sf(data=pt.sf, colour=alpha("red", 0.7)) +
   #   theme(legend.position="none") + coord_sf(xlim=c(100,230), ylim=c(-22,22))
   # print(pl)
   # 
   # 
   # 

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   regnm <- "reg3"
   
   
   
   reg.defs <- c("POLYGON((170 10,210 10,210 -20,170 -20,170 10))",
                 "POLYGON((120 50,210 50,210 10,120 10,120 50))")
   
   
   
   
   bnd.bx <- c(reg.defs[2]) %>% 
     st_as_sfc(crs=st_crs(eez)) %>% 
     st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
   
   
   as.reg <- st_intersection(bnd.bx, pac.eez)
   
   
   
   windows(3000,2000)
     pl <- ggplot() +
                  geom_sf(data=as.reg, aes(fill=Territory1)) + geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
                  theme(legend.position="none") + coord_sf(xlim=c(90,240), ylim=c(-25,50))
     print(pl)
   
   
     tmp <- st_area(as.reg) 
     tmp.ars <-  data.frame(Cnt=as.reg$Territory1) %>% mutate(Est=tmp, Est.km=Est/1000000)
     write.csv(tmp.ars, file=paste0("C:/EEZs/Data/EEZ_areas_",regnm, ".csv"), row.names=FALSE) 
     
   
   
   
   
   
   
   
   
  
   
   
   
   
   
   
   
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   
   
   
   
   
   
 
 
 
 
 tmp1 <- eez[eez$Territory1 == "Fiji",]
 tmp2 <- arch[arch$Territory1 == "Fiji",]
 
 windows(3000,2000)
 pl <- ggplot() +
   geom_sf(data=tmp1, aes(fill=Territory1)) + geom_sf(data=tmp2, fill="grey") +
   theme(legend.position="none") + coord_sf(xlim=c(160,190), ylim=c(-25,-10))
 print(pl)
 
  
 # tmp <- st_intersection(eez, arch)
 tmp3 <- st_intersection(tmp1, tmp2)
 
 tmp4 <- st_area(tmp3)
 
 
 diff1 <- st_difference(tmp1, tmp2)
 
 
 
 windows(3000,2000)
   pl <- ggplot() +
                geom_sf(data=tmp3, aes(fill=Territory1)) +
                theme(legend.position="none") + coord_sf(xlim=c(160,190), ylim=c(-28,-5))
   print(pl)
 
 
 windows(3000,2000)
   pl <- ggplot() +
                geom_sf(data=diff1, aes(fill=Territory1)) +
                theme(legend.position="none") + coord_sf(xlim=c(160,190), ylim=c(-28,-5))
   print(pl)
 
 
 bnd.bx <- c("POLYGON((160 -22,160 -14,175 -14,175 -22,160 -22))",
             "POLYGON((160 -22,160 -14,175 -14,175 -22,160 -22))") %>% 
           st_as_sfc(crs=st_crs(eez)) %>% 
           st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
 
 bnd.bx <- c("POLYGON((110 20,210 20,210 -4,230 -4,230 -20,140 -20,110 -20,110 20))") %>% 
           st_as_sfc(crs=st_crs(eez)) %>% 
           st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
 
 
 
 windows(3000,2000)
 pl <- ggplot() +
   geom_sf(data=tmp1, aes(fill=Territory1)) + geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
   theme(legend.position="none") + coord_sf(xlim=c(160,180), ylim=c(-25,-10))
 print(pl)
 
 
 tmp5 <- st_intersection(tmp1, bnd.bx)
 
 
 windows(3000,2000)
 pl <- ggplot() +
   geom_sf(data=tmp5) +
   theme(legend.position="none") + coord_sf(xlim=c(160,180), ylim=c(-25,-10))
 print(pl)
 
 tmp6 <- st_area(tmp5)/1000000
 
 
 
 
 
 
 
 
 
 

 
 
 windows(3000,2000)
 pl <- ggplot() +
   geom_sf(data=tmppoly) +
   theme(legend.position="none") + coord_sf(xlim=c(90,180), ylim=c(-25,25))
 print(pl)
 
 
 
 
 
 
 
 
 pac.diff <- data.frame() 
 
 for(i in 1:2){
   
   cnt <- as.character(pac.arc$Territory1[i])
   
   tmp.eez <- pac.eez[pac.eez$Territory1 == cnt,]
   tmp.arc <- pac.arc[pac.arc$Territory1 == cnt,]
   
   tmp.diff <- st_difference(tmp.eez, tmp.arc)
   
   if(i == 1){
     
     pac.diff <- tmp.diff
   } else {
     
     pac.diff <- rbind(pac.diff, tmp.diff)
   }
   
 }
 
 cnt <- as.character(pac.arc$Territory1[1])
 
 tmp.eez <- pac.eez[pac.eez$Territory1 == cnt,]
 tmp.arc <- pac.arc[pac.arc$Territory1 == cnt,]
 
 tmp.diff <- st_difference(tmp.eez, tmp.arc)
 
 
 cnt <- as.character(pac.arc$Territory1[9])
 
 tmp.eez <- pac.eez[pac.eez$Territory1 == cnt,]
 tmp.arc <- pac.arc[pac.arc$Territory1 == cnt,]
 
 tmp.diff1 <- st_difference(tmp.eez, tmp.arc)
 
 
 
 tmpp <- rbind(tmp.diff, tmp.diff1)
 
 
 
 windows(3000,2000)
 pl <- ggplot() +
   geom_sf(data=tmp.eez, aes(fill=Territory1)) + geom_sf(data=tmp.arc, fill=alpha("grey", 0.4)) +
   theme(legend.position="none") + coord_sf(xlim=c(160,180), ylim=c(-25,-10))
 print(pl)
 
 
 windows(3000,2000)
 pl <- ggplot() +
   geom_sf(data=tmp.diff, aes(fill=Territory1)) +
   theme(legend.position="none") + coord_sf(xlim=c(160,180), ylim=c(-25,25))
 print(pl)
 
 
 
 
 
 
 
 
 
 
 windows(3000,2000)
   pl <- ggplot() +
                geom_sf(data=pac.eez, aes(fill=Territory1)) + geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
                theme(legend.position="none") + coord_sf(xlim=c(100,240), ylim=c(-50,30))
   print(pl)
 
  pac.dif <- st_difference(pac.eez, pac.arc)   
   
 
 tmp2 <- st_intersection(pac.dif, bnd.bx)
 
 
 
 windows(3000,2000)
   pl <- ggplot() +
                geom_sf(data=tmp2, aes(fill=Territory1)) + #geom_sf(data=bnd.bx, fill=alpha("grey", 0.2)) +
                theme(legend.position="none") + coord_sf(xlim=c(100,240), ylim=c(-30,30))
   print(pl)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

#path.eez.world.v8 <- ("/Users/danielpalacios/Documents/DMP/dmp_data/World_EEZ/World_EEZ_v8_20140228_LR")
path.eez <- ("C:/EEZs/Data/World_EEZs")
#fnam.eez.world.v8 <- "World_EEZ_v8_2014.shp"
fnam.eez <- "eez_v10.shp"
#eez.world.v8 <- readOGR(dsn = path.eez.world.v8, 
#                        layer = file_path_sans_ext(fnam.eez.world.v8))
eez.world <- readOGR(dsn = path.eez, 
                        layer = file_path_sans_ext(fnam.eez))
# A Large SpatialLinesDataFrame object with 281 features and 23 fields (18.9 Mb)

# Extract the EEZ for the USA:
#dat.eez.usa2 <- eez.world.v8[eez.world.v8@data$Country == "United States", ]
# For v. 9 use $Territory1 instead of $Country:
dat.eez.usa2 <- eez.world.v9[eez.world.v9@data$Territory1 == "United States", ]
# A Formal class Large SpatialPolygonsDataFrame

# Fortify the shapefile data:
#dat.eez.usa2 <- fortify(dat.eez.usa2)
# `fortify.shape()` did not work for v. 8 so we had to use `fortify()`.
# message: Regions defined for each Polygons
dat.eez.usa2 <- fortify.shape(dat.eez.usa2) # a 10298x30 dataframe







