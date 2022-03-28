setwd("C:/Users/Kevin Wang/OneDrive - UCLA IT Services/alfarolab (1)/BayesTrait-Analysis/results/throat+breast")
library(coda)

bf_res_data <- "bf_res_restricted"
load(paste0(bf_res_data, ".RData"))

bf_res_entry <- bf_res_restricted[[1]]
print_posterior_histogram(bf_res_restricted[[2]], "brown", "throat", "brown", "breast")

print_posterior_histogram <- function(bf_res_entry, color1, patch1, color2, patch2) {
  independent <- FALSE
  if(bf_res_entry$bftest$BetterModel == "Model 1") {
    independent <- TRUE
  }
  
  if(independent) {
    ind_chain <- mcmc(bf_res_entry$ind_res$Log$results[c(-1, -3)][1:5])
    plot(ind_chain)
  }
  else {
    dep_chain <- mcmc(bf_res_entry$dep_res$Log$results[c(-1, -3)][1:9])
    plot(dep_chain)
  }
}
