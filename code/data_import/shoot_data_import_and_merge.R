# This file handles importing the shoot dataset
# make sure there is no existing data
all_shoot_data <- NA

#' shoot data is now in a single stacked sheet
#' just need to rename columns & make UUIDs
#' 
#' adjusted y/n columns to be of type 'logical' to  avoid the issue of NA's in later condition checks
#' example of problem
#' if_else(c(TRUE, TRUE, NA, NA, TRUE), 1, 0)
#' vs
#' if_else(c(TRUE, TRUE, FALSE, FALSE, TRUE), 1, 0)
all_shoot_data <- 
	read_csv(here("input/all_shoots_stacked.csv")) %>%
	mutate(ShootUUID = paste(Vine, Shoot, sep = "-S")) %>%
	rename(VineUUID = 1, LeafLoss = 6,
		   FruitPerShoot = 7, ShootLength = 8,
		   ShootType = 9, IsPruned = 11,
		   HasRegrowth = 12, IsStrung = 13,
		   LengthIsEstimate = 14, ShootDiameter = 15,
		   Comments = 16) %>%
	mutate(IsPruned = if_else(IsPruned %in% "y", TRUE, FALSE),
		   HasRegrowth = if_else(HasRegrowth %in% "y", TRUE, FALSE),
		   IsStrung = if_else(IsStrung %in% "y", TRUE, FALSE),
		   LengthIsEstimate = if_else(LengthIsEstimate %in% "y", TRUE, FALSE)) %>%
	select(VineUUID, ShootUUID, LeafLoss, FruitPerShoot,
		   ShootLength, IsPruned, 
		   HasRegrowth, IsStrung, LengthIsEstimate, ShootDiameter, Comments)

#  Shoot types by length only
#'all_shoot_data <- within(all_shoot_data, ShootTypeVeryCoarse <- 
#'					if_else(ShootLength<35, "short",	
#'						   if_else(ShootLength>=180, "long", "medium")))


#   Shoot types by coarse category
#' Changed all if_else to be if_else for efficiency and preservation of types:
#' https://community.rstudio.com/t/case-when-why-not/2685/2
#' 
#' There is a hole in the logic test with this: 
#' all_shoot_data %>% filter(!((ShootLength < 35 & !IsPruned) | 
#'                             (ShootLength >= 180 | (IsPruned & ShootDiameter >= 9)) | 
#'                             (ShootLength >=35 & (ShootLength < 180 |  ShootDiameter < 9))))
#'                             
#' has the same logical conditions as the 'Coarse' categorisation, but negated to find all those that DONT match,
#' returns 111 rows.
#' 
#' uncategorised shoots are all:
#'    <35 in length
#'    pruned
#'    < 9 in diameter
#'all_shoot_data %<>%
#'	mutate(ShootTypeCoarse = 
#'		   	if_else((ShootLength<35 & (!IsPruned | (IsPruned & ShootDiameter <7.5))), "short",
#'		   	  ifelse(((ShootLength>=35 & ShootLength<180) | (ShootLength<35 & IsPruned & ShootDiameter>=7.5 & ShootDiameter<9)), "medium",
#'				if_else((ShootLength>=180 | (IsPruned & ShootDiameter>=9)), "long","ERROR"))))


# Cross referencing WoodType from the all_arch_data data frame
all_shoot_data %<>% 
	left_join(select(all_arch_data, ShootUUID, WoodType), by = "ShootUUID")



#   Assigning ShootTypeCoarse based on ShootTypeRefined categories

#   Shoot types by refined category, incorporating length, diameter, and pruning status
#' Switching the logical columns to be just that, seems to have fixed the filtering issue here
all_shoot_data <- within(all_shoot_data, ShootTypeRefined <- 
	if_else((ShootLength<=1 & !IsPruned),"stub", 
		if_else((ShootLength>1 & ShootLength<=10 & !IsPruned),"very short", 
			if_else((ShootLength<=35 & ShootLength>5 & !IsPruned),"short", 
				if_else((ShootLength>35 & ShootLength<180 & !IsPruned),"medium",
					if_else((ShootDiameter<9 & IsPruned),"medium pruned",
						if_else((ShootLength>=180 & ShootLength<500 & !IsPruned),"long",
							if_else((ShootDiameter>=9 & ShootLength>=40 & IsPruned), "long pruned",
								if_else((ShootDiameter>=9 & ShootLength<40 & IsPruned),"long stubbed", 
									if_else(ShootLength>=500,"very long","ERROR"))))))))))						
						 

all_shoot_data %<>%
	mutate(ShootTypeCoarse = 
		   	if_else((ShootTypeRefined=="stub" | ShootTypeRefined=="very short" | ShootTypeRefined=="short"), "short",
		   			if_else((ShootTypeRefined=="medium"  | ShootTypeRefined=="medium pruned"), "medium",
		   					if_else((ShootTypeRefined=="long" | ShootTypeRefined=="long pruned" | ShootTypeRefined=="long stubbed"  | ShootTypeRefined== "very long"), "long", "ERROR"))))


# utilising coefficients from the shoot_leaf_area_import_and_plot power curve model (FitPwrVolume) to estimate leaf area
require(graphics)


all_shoot_data %<>%
mutate(ShootVolume = (pi * ((ShootDiameter/20)^2)*ShootLength/3))

all_shoot_data %<>%
mutate(ShootLeafArea = predict(FitPwrVolume, newdata = all_shoot_data))


		  

# for(vine_id in 1:9){
# 	# using numbers to reference column locations as most have spaces in the original
# 	temp_shoots <- read_csv(paste0("input/shoot_data/shoot_data_vine", vine_id ,".csv")) %>%
# 						rename(LeafLoss = 5, FruitPerShoot = 6, ShootLength = 7,
# 							   ShootType = 8, IsPruned = 9, HasRegrowth = 10,
# 							   IsStrung = 11, ShootDiameter = 12, Comments = 13) %>%
# 						mutate(ShootUUID = paste(vine_id, Shoot, sep = "-"),
# 							   ShootDiameter = as.numeric(ShootDiameter),
# 							   LeafLoss = as.numeric(LeafLoss),
# 							   VineUUID = vine_id) %>%
# 						select(VineUUID, ShootUUID, LeafLoss:Comments)
# 	
# 	
# 	if(length(all_shoot_data) <= 1)
# 		all_shoot_data <- temp_shoots
# 	else
# 		all_shoot_data %<>% bind_rows(temp_shoots)
# 	
# }

# rm(temp_shoots)
