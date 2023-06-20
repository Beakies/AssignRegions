# Install the package and load into library

# install.packages("zen4R")
# library(zen4R)
library(here)
# 
# install.packages("osfr")
library(osfr)

#need authorization token, but this shouldnt be shared....
osf_auth("")

cr_project <- osf_retrieve_node("rnqs9")

x = osf_ls_files(cr_project)

osf_download(x, path = here("shapefiles"))

unzip("shapefiles/shapefiles.zip", exdir = "shapefiles")
