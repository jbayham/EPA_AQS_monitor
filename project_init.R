#This script initializes the project and should be run at the beginning of each
#session

#########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
init.pacs(c("tidyverse",      #shortcut to many useful packages (eg, dplyr, ggplot)
            "conflicted",     #resolves function conflict across packages
            "lubridate",      #working with dates
            "hms",            #working with time
            "sf",             #for GIS
            "USAboundaries",  #easily access US maps in sf
            "mapview",        #quick leaflet maps
            "svDialogs",      #dialog boxes on all platforms
            "RSQLite",        #Connection to sqlite db
            "dbplyr",         #dplyr tools for database
            "rvest",          #utilities to scrape web data
            "RCurl",          #utilities to scrape web data
            "XML",            #utilities to scrape web data
            "haven",          #write to stata files
            "googledrive"     #for accessing googledrive
            
))

#GIS projections
project.epsg=5070

#Setting package::function priority with conflicted package
conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")

#Reference:
code.ref <- tibble(
  code=c(44201,42401,42101,42602,88101,88502,81102,"SPEC","PM10","HAPS","VOCs","NONO"),
  name=c("Ozone","S02","CO","N02","PM25_frm","PM25_nonfrm","PM10_mass","PM25_speciated","PM10_speciated","HAPS","VOCs","NONO")
)

#########################
#Loading project helper functions (all scripts within folder)
run.script("functions")


##########################################
##########################################
#Function to download the project data (on first run, google should prompt you to login with credentials)
#if data folder doesn't exist, build data
#get_data("url")


#folder.setup()


#dlgMessage("Do you need to pull the repo?")
