library(shiny)
library(ggplot2)
library(plotly)
library(ggplot2)
library(tidyverse)


theme_set(theme_bw())

CCM_cols <- c("dodgerblue2","firebrick3","darkorchid","darkred","lightskyblue","yellow1","orange1","honeydew3","darkgoldenrod3","deeppink3","dodgerblue4","seagreen3","purple4","grey48","yellow3","black","pink","green","blue","olivedrab","dodgerblue1","seagreen1","darkgoldenrod4","azure","bisque1","tomato","wheat1","turquoise1","turquoise3","deeppink4","coral3","coral4")


dat <- read.csv(paste0(getwd(), "/shiny/Data/Data_Matrix.csv"), header=TRUE)

datsum <- apply(dat[, -c(1,2)], 2, sum)

datrel <- dat
datrel[, -c(1,2)] <- dat[, -c(1,2)]/datsum


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