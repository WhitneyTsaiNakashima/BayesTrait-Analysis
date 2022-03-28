library(stringr)
library('plot.matrix')

setwd("C:/Users/Kevin Wang/OneDrive - UCLA IT Services/alfarolab (1)/BayesTrait-Analysis/results/throat+breast")

results_csv <- read.csv("all_tanagers_color_bt_results.csv")
bf_res <- "bf_res_restricted.RData"
load(bf_res)

bf_res <- bf_res_restricted

create_matrix(results_csv)

length(results_csv)

create_matrix <- function(result_csv, bf_res) {
  patch1 <- str_split(results_csv[["color1"]][1], pattern = "_", n = Inf, simplify = FALSE)[[1]][1]
  patch2 <- str_split(results_csv[["color2"]][1], pattern = "_", n = Inf, simplify = FALSE)[[1]][1]
  
  first_color <- split_color(results_csv[["color1"]][1])
  color_vector <- c()
  
  row_names <- c(first_color)
  col_names <- c()
  first_iter <- TRUE
  num_rows <- 1
  num_cols <- 0
  
  for(i in 1:nrow(results_csv)) {
   curr_color <- split_color(results_csv[["color1"]][i])
   if(curr_color != first_color) {
     first_color <- curr_color
     row_names <- c(row_names, first_color)
     first_iter <- FALSE
     num_rows = num_rows + 1
   }
   if(first_iter) {
     curr_color2 <- split_color(results_csv[["color2"]][i])
     col_names <- c(col_names, curr_color2)
     num_cols = num_cols + 1
   }
   dependent <- "independent"
   if(results[["bestmodel"]][i] == "dep") {
     dependent <- "dependent"
   }
   color_vector <- c(color_vector, dependent)
  }
  matrix_results <- matrix(color_vector, nrow = num_rows, ncol = num_cols, byrow =TRUE, dimnames = list(row_names, col_names))
  par(mar=c(4.1, 4.1, 4.1, 4.1))
  plot(matrix_results, xlab = patch2, ylab = patch1, main = paste0("Dependent Runs for ", patch2 ," and ", patch1, " MCMC Analysis"))
}

split_color <- function(patch_color) {
  str_split(patch_color, pattern = "_", n = Inf, simplify = FALSE)[[1]][2]
}
  