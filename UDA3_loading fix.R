if (!require("sf")) install.packages("sf")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("elevatr")) install.packages("elevatr")
if (!require("terra")) install.packages("terra")
if (!require("biscale")) install.packages("biscale")
if (!require("cowplot")) install.packages("cowplot")
if (!require("remotes")) install.packages("remotes")
if (!require("hoardr")) install.packages("hoardr")
if (!require("rgeoboundaries")) remotes::install_github("wmgeolab/rgeoboundaries")
if (!require("climateR")) remotes::install_github("mikejohnson51/climateR")
if (!require("xgboost")) install.packages("xgboost")
if (!require("caTools")) install.packages("caTools")
if (!require("ncdf4")) install.packages("ncdf4")

library(ncdf4)
library(tidyverse)
library(sf)
library(terra)
library(rgeoboundaries)
library(climateR)
library(biscale)
library(cowplot)
library(elevatr)
library(xgboost)
library(caTools)

# retrieve the data for each year in a for loop
Load_save <- function(mode,start,end, path){
    climData_temp <- list()
    climData_ppt <- list()

    nc_list <- list.files(path,pattern=".nc$") # get list of all .nc files in teh current working directory

    for (year in start:end){
        cat(paste0("Lade ",mode,"-Klimadaten von " ,year," ...\n"))
        url_tmax <- paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_tmax_",year,".nc")
        url_tmin <- paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_tmin_",year,".nc")
        url_ppt <- paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_",year,".nc")

        # instead of strsplit(url_tmax,"/",fixed=TRUE)[[1]][4] use basename(), much cleaner and safer (not relying on position); thx chatgpt
        # first extract the Vector with [[1]], since strsplit() returns a list
        # R wants TRUE instead of True ...
        filename_tmax = basename(url_tmax) 
        filename_tmin = basename(url_tmin)
        filename_ppt = basename(url_ppt)

        # print(filename_tmax) #for debugging

        if (filename_tmax %in% nc_list){
            r_tmax <- rast(filename_tmax)
        }
        if (filename_tmin %in% nc_list){
            r_tmin <- rast(filename_tmin)
        }
        if (filename_ppt %in% nc_list){
            r_ppt <- rast(filename_ppt)
        }
        # read and safe the files in order to avoid downloading with every new start of the script (since downloading takes 80 min just for the current data (2000-2020))
        else { 
            # retrieve the actual rasters from the urls (we have row, columns and layers -> typical for the .nc datetype, somewhat 3D arrays/dataframes)
            r_tmax <- rast(url_tmax)
            r_tmin <- rast(url_tmin)
            r_ppt <- rast(url_ppt)

            # saved to working directory: (retrievable via getwd(); automatically set in my VSCode mamba R environment)
            writeCDF(r_tmax,paste0(path,filename_tmax))
            writeCDF(r_tmin,paste0(path,filename_tmin))
            writeCDF(r_ppt,paste0(path,filename_ppt))
        }

        # check for matching crs [coordinate reference system, see GIS-lecture ;)] and if not reproject to WGS84 (the one the germany area is in) [typical standard world-crs, there are other crs more adjusted to europe or germany but we use WGS84 for now and it's just fine)
        if (crs(germany) != crs(r_tmax)) {
            r_tmax <- project(r_tmax, crs(germany))
        }
        if (crs(germany) != crs(r_tmin)) {
            r_tmin <- project(r_tmin, crs(germany))
        }
        if (crs(germany) != crs(r_ppt)) {
            r_ppt <- project(r_ppt, crs(germany))
        }
        r_tmin_germany <- r_tmin %>% crop(germany) %>% mask(germany)
        r_tmax_germany <- r_tmax %>% crop(germany) %>% mask(germany)
        r_ppt_germany <- r_ppt %>% crop(germany) %>% mask(germany)

        # can do this operation because every month is a own layer, and therefore the operation is handled on the month
        mean_temp_monthly <- (r_tmax_germany + r_tmin_germany) / 2
        annual_mean_temp  <- mean(mean_temp_monthly, na.rm = TRUE)
        annual_mean_ppt <- mean(r_ppt_germany, na.rm = TRUE)

        # save the created datesets for each year in a list with names (convert the number to a string/character)
        climData_temp[[as.character(year)]] <- annual_mean_temp
        climData_ppt[[as.character(year)]] <- annual_mean_ppt
    }
    # return list of 2 elements
    return(list(temp=climData_temp, ppt=climData_ppt))
}

germany <- geoboundaries(country = "Germany")
germany1 <- geoboundaries(country = "Germany", adm_lvl = "adm1")
plot(st_geometry(germany1), border = "gray")
plot(st_geometry(germany), add = TRUE, lwd = 2)

# need to set path for saving files to and read from since the files are in the range of 1.5-2GB and therefore to big for the workingdir which is set to the github repo
    # so around 80 GB of free space needed
p="..." # change to the path on your system

# result is a named list with temp and ppt as entries (which themselves are alos lists of different years)
# access e.g. via result_current$temp
# function must also be runned inside R env for once
result_current <- Load_save(mode="Gegenwarts", start=2000, end=2020, path=p)
result_history <- Load_save(mode="Vergangenheits", start=1960, end=1980, path=p)