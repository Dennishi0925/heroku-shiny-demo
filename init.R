# my_packages = c("shiny", 'dplyr')
# 
# install_if_missing = function(p) {
#   if (p %in% rownames(installed.packages()) == FALSE) {
#     install.packages(p)
#   }
# }
# 
# invisible(sapply(my_packages, install_if_missing))

#
# init.R
# shiny-heroku
#
# Created by blakiseskream on 5/7/2018
# MIT License and shit
#
# 
my_packages = c("shiny", 'shinythemes', 'shinyjs', 'shinydashboard',
                'shinyWidgets', 'DT', #'plotly', 'showtext', 
                'magrittr', 'dplyr', 'readr', 'stringr')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
