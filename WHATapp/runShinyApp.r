library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinyWidgets)


theme_set(theme_bw())

CCM_cols <- c("dodgerblue2","firebrick3","darkorchid","darkred","lightskyblue","yellow1","orange1","honeydew3","darkgoldenrod3","deeppink3","dodgerblue4","seagreen3","purple4","grey48","yellow3","black","pink","green","blue","olivedrab","dodgerblue1","seagreen1","darkgoldenrod4","azure","bisque1","tomato","wheat1","turquoise1","turquoise3","deeppink4","coral3","coral4")


dat <- read.csv(paste0(getwd(), "/shiny/Data/Data_Matrix.csv"), header=TRUE)

datrel <- dat
datrel[, -c(1,2)] <- as.data.frame(prop.table(as.matrix(datrel[, -c(1,2)]), 2))

datalt <- datrel
datalt[23,3] <- 0   # Allow removal of PH HS allocation from CMM
datalt[, -c(1,2)] <- as.data.frame(prop.table(as.matrix(datalt[, -c(1,2)]), 2)) 

effrt <- read.csv(paste0(getwd(), "/shiny/Data/Annual_PS_Effort.csv"), header=TRUE)
effrthsp <- read.csv(paste0(getwd(), "/shiny/Data/Annual_PS_Effort-I1.csv"), header=TRUE)

catch <- read.csv(paste0(getwd(), "/shiny/Data/Annual_LL_Catch.csv"), header=TRUE)

minyr <- 2003
maxyr <- 2018

message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

chrome.portable = file.path(getwd(), 'GoogleChromePortable/App/Chrome-bin/chrome.exe')


launch.browser = function(appUrl, browser.path=chrome.portable) {
  #browser()
  browser.path = chartr('/', '\\', browser.path)
  message('Browser path: ', browser.path)
  CMD = browser.path
  ARGS = sprintf('--app="%s"', appUrl)
  system2(CMD, args=ARGS, wait=FALSE)
  NULL
}


runApp("./shiny/", launch.browser=launch.browser)