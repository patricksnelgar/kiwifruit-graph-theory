# master script to import all relevant data
# avoids the issue with dependancies when running scripts individually outside of the .Rmd

library(readr)
library(magrittr)
library(here)
library(dplyr)
library(lubridate)

# relational information for each of the quadrants
quadrant_info <- 
	data.frame(Quadrant = 1:36,
			   OffsetX = rep(c(1000, 500, 0, 0, 500, 1000), each = 6),
			   OffsetY = rep(c(2000, 1000, 0, 0, 1000, 2000), 6),
			   MultiplierX = rep(c(-1,1), each = 18),
			   MultiplierY = rep(rep(c(-1,1), each = 3), 6),
			   QuadrantFromLeader = rep(c(3:1, 1:3), 6), 
			   QuadrantFromTrunk = rep(c(3:1, 1:3), each = 6),
			   NorthSouth = rep(c("S", "N"), each = 18),
			   EastWest = rep(rep(
			   	c("W", "E"), 
			   	each = 3
			   ),
			   6)
	)

# x&y coords for overlaying the quadrant IDs
# only works when vine is plotted with leader running horizontal (swap x and y).
quadrant_labels <- data.frame(x = rep(c(-1250, -750, -250, 250, 750, 1250), each = 6),
							  y = rep(c(2500, 1500, 500, -500, -1500, -2500), 6),
							  label = 1:36)

flowering_dates <- read_csv(here("input/flowering_dates.csv")) %>%
	mutate(FloweringColour = tolower(FloweringColour),
		   FloweringDate = dmy(FloweringDate))


source(here("code/data_import/shoot_leaf_area_import_and_plot.R"))

source(here("code/data_import/architecture_data_import_and_merge.R"))
source(here("code/data_import/shoot_data_import_and_merge.R"))
source(here("code/data_import/fruit_data_import_and_merge.R"))

source(here("code/data_import/fruit_exclusions.R"))


rm(quadrant_info)
rm(quadrant_labels)
rm(flowering_dates)
rm(leaf_area_data)
