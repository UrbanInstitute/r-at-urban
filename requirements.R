# This file lists the packages required to run the R Users Group Website.

cran_packages <- c("tidycensus", 
                   "censusapi", 
                   "tidyverse", 
									 "sf", 
									 "tigris",
									 "mapview",
									 "gapminder", 
									 "RXKCD", 
									 "ggrepel", 
									 "forcats", 
									 "ggridges", 
									 "fivethirtyeight", 
									 "gghighlight", 
									 "AmesHousing", 
									 "wooldridge", 
									 "kableExtra", 
									 "here", 
									 "tidylog", 
									 "geofacet", 
									 "leaflet.extras2", 
									 "profvis", 
									 "microbenchmark", 
									 "future.apply", 
									 "furrr", 
									 "pryr", 
									 "remotes")
                  
github_packages <- c("UrbanInstitute/urbnmapr", 
										 "UrbanInstitute/urbnthemes")

install.packages(cran_packages)

remotes::install_github(github_packages)
