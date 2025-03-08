## -----------------------------------------------------------------------------
##
## Project: Summer Research Project
##
## Script purpose: Functions for  EDA and Plotting 
##
## Date: 04/06/2024
## Author: BOD
##
## -----------------------------------------------------------------------------



## add_title_grid
#' Create a plot grid with an overall title 
#' 
#' @param list list of plots 
#' @param title the overall title 
#' 
#' @return final plot with title 
#' 
add_title_grid <- function(list, title){
  plots <- plot_grid(plotlist = list) 
  title <- ggdraw() + draw_label(paste(title), fontface='bold')
  grid <- plot_grid(title, plots, ncol=1, rel_heights=c(0.1, 1)) 
  return(grid)
}









