
# ------------------------------------------------------------------------------
# packages:

## Default repository
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})

check_pkg <- function(x)
{
  if (!require(x, character.only = TRUE, quietly = TRUE))
  {
    install.packages(x, dep = TRUE, verbose = FALSE, quiet = TRUE)
    if(!require(x, character.only = TRUE, quietly = TRUE)) stop("Package not found")
  }
}

check_pkg("tidyverse")
check_pkg("sf")
check_pkg("sp")        # required by adehabitatHR package
check_pkg("osmdata")   # osmdata may need to be compiled; if so, use install.packages() interactively
check_pkg("adehabitatHR")
check_pkg("leaflet")   
check_pkg("RColorBrewer")
check_pkg("spatstat")
check_pkg("terra")       # terra replaces the older raster package; no dependencies on rgeos & rgdal
check_pkg("ks")          # required by package spatialEco --> see below
check_pkg("tmap")        # Used for thematic mapping
check_pkg("here")        # Used for setting working directory
check_pkg("kableExtra")  # For nicer kable tables

