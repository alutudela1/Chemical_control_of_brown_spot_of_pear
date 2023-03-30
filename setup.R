knitr::opts_chunk$set(echo = T, warning = FALSE, message = FALSE, 
                      fig.height = 6, fig.width = 7)

# install.packages('BiocManager')
# install.packages('devtools')
# devtools::install_github("DoseResponse/drcData")
# devtools::install_github("DoseResponse/drc")
# devtools::install_github("DoseResponse/medrc")

pacman::p_load(
  tidyverse, 
  medrc # EC50 estimations and comparisons
)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

f <- function(x, y, z){
  case_when(
    z < pmin(x, y, na.rm = TRUE) ~ "above",
    z > pmax(x, y, na.rm = TRUE) ~ "below",
    between(z, pmin(x, y, na.rm = TRUE), pmax(x, y, na.rm = TRUE)) ~ "within"
  )
}
