# Library packages
libraries<-c("gdalUtils", "link2GI" ,"glcm", "sf", "tools", "lattice", "ggplot2", "RColorBrewer", 
             "seriation", "ca", "FactoMineR", "vcd", "MASS", "TSP", "vegan",
             "gplots", "factoextra", "reshape2", "corrplot", "dplyr", "igraph", 
             "quantAAR", "tidyverse", "varnastats", "EnvStats", "car", "ggpubr",
             "Hmisc", "dlookr", "GGally", "ppcor", "oxcAAR")
# Install CRAN packages if needed
inst <- libraries %in% installed.packages()
if(length(libraries[!inst]) > 0) install.packages(libraries[!inst])

# Load library packages into session if required
lapply(libraries, require, character.only=TRUE)
