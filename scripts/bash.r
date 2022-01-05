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
library(qdapTools, lib.loc = "/home/kevinw/packages")

setwd("/home/kevinw/300_tanager_analysis")

dir <- getwd()
source(paste0(dir, "/color_utility_functions.R"))
tt <- read.nexus(paste0(dir, "/MCC_Tree_SpNames.nex"))
inpath <- paste0(dir, "/all_patch_colors_merged.csv")
patchdd <- read.csv(inpath, header=T)

#clean and transform patchdd to a list of patches and their colors
patch_dfs <- clean_data(patchdd) 
species <- patch_dfs[[1]]["species"]

# Total Number of Patch combinations
# total number of restricted/unrestricted
# Total number of final unconverged (List unconverged -> patch combination, color pair, ESS)
#   per patch_combinations 
#     restricted vs unrestricted 
#     number_MCMC_runs
#     restricted runs that don't converge 

total_independent_restrictions <- 0
total_dependent_restrictions <- 0
total_unconverged_parameters <- 0
total_unconverged_parameters_after_restrictions <- 0

file.create("Patch_Runs_Summary.txt")
file.create("Total_Summary.txt")

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
      results <- compareRates(cdfs, tt, patch1 = names(patch_dfs)[i], patch2 = names(patch_dfs)[j], silent = FALSE)
      
      sink("Patch_Runs_Summary.txt", append = TRUE)
      cat(paste0("Results for ", names(patch_dfs)[i], " and ", names(patch_dfs)[j], " MCMC analysis"))
      cat("\n")
      cat(paste0("Independent Restrictions: ", results$ind_res))
      cat("\n")
      cat(paste0("Dependent Restrictions: ", results$dep_res))
      cat("\n")
      cat(paste0("Unconverged Parameters: ", results$total_unconverged_parameters))
      cat("\n")
      cat(paste0("Unconverged Parameters after restrictions: ", results$unconverged_parameters_after_restrictions))
      cat("\n")
      cat("\n")
      cat("\n")
      sink()
      
      total_independent_restrictions <- total_independent_restrictions + results$ind_res
      total_dependent_restrictions <- total_dependent_restrictions + results$dep_res
      total_unconverged_parameters <- total_unconverged_parameters + results$total_unconverged_parameters
      total_unconverged_parameters_after_restrictions <- total_unconverged_parameters_after_restrictions + results$unconverged_parameters_after_restrictions
    }
  }
}

sink("Total_Summary.txt", append = TRUE)
cat("MCMC Patch Run Summary")
cat("\n")
cat("\n")
cat("\n")
cat(paste0("Total Independent Restrictions: ", total_independent_restrictions))
cat("\n")
cat(paste0("Total Dependent Restrictions: ", total_dependent_restrictions))
cat("\n")
cat(paste0("Total Unconverged Parameters: ", total_unconverged_parameters))
cat("\n")
cat(paste0("Total Unconverged Parameters after restrictions: ", total_unconverged_parameters_after_restrictions))
cat("\n")
sink()

