.libPaths( c("/home/kevinw/packages", .libPaths()) )

library(parallel)
library(devtools, lib.loc="/home/kevinw/packages")
library(btw, lib.loc="/home/kevinw/packages")
library(ape)
library(dplyr, lib.loc="/home/kevinw/packages")
library(utils)
library(stringr, lib.loc="/home/kevinw/packages")
library(geiger)
library(coda)
library(qdapTools)

setwd("/home/kevinw/300_tanager_analysis")

dir <- getwd()

source("/home/kevinw/300_tanager_analysis/color_utility_functions.R")
tt <- read.nexus("/home/kevinw/300_tanager_analysis/MCC_Tree_SpNames.nex")
#indata = "/home/kevinw/300_tanager_analysis/all_patch_colors_summarized_calls.csv"

inpath <- paste0(dir, "/all_patch_colors_merged.csv")

patchdd <- read.csv(inpath, header=T)

#clean df to only include colors
patch_dfs <- clean_data(patchdd) 

species <- patch_dfs[[1]]["species"]

#loop through combinations of patches to call Bayestrait for patch comparisons
for(i in 1:length(patch_dfs)) {
  if(i != length(patch_dfs)) {
    for(j in (i+1):length(patch_dfs)) {
      patch1 = patch_dfs[[i]]
      patch2 = patch_dfs[[j]]
      #each entry in cdfs has 3 columns 
      #nested for loops construct cdfs list
      cdfs <- list()
      for(color1 in names(patch1[-1])) {
        for(color2 in names(patch2[-1])) {
          df <- cbind(species, patch1[color1], patch2[color2])
          names(df)[2] <- paste0(names(patch_dfs)[i], "_", color1)
          names(df)[3] <- paste0(names(patch_dfs)[j], "_", color2)
          cdfs <- c(cdfs, list(df))
        }
      }
      #call bayestraits to compare patch i and j in patch_dfs
      compareRates(cdfs, patch1 = names(patch_dfs)[i], patch2 = names(patch_dfs)[j])
    }
  }
}

