# This file handles importing the shoot dataset

all_shoot_data <- NA

# shoot data is now in a single stacked sheet
# just need to rename columns & make UUIDs
all_shoot_data <- 
	read_csv(here("input/all_shoots_stacked.csv")) %>%
	mutate(ShootUUID = paste(Vine, Shoot, sep = "-S")) %>%
	rename(VineUUID = 1, LeafLoss = 6,
		   NumFruit = 7, ShootLength = 8,
		   ShootType = 9, IsPruned = 11,
		   HasRegrowth = 12, IsStrung = 13,
		   LengthIsEstimate = 14, ShootDiameter = 15,
		   Comments = 16) %>%
	mutate(IsPruned = gsub("y", TRUE, IsPruned),
		   HasRegrowth = gsub("y", TRUE, HasRegrowth),
		   IsStrung = gsub("y", TRUE, IsStrung),
		   LengthIsEstimate = gsub("y", TRUE, LengthIsEstimate)) %>%
	select(VineUUID, ShootUUID, LeafLoss, NumFruit,
		   ShootLength, ShootType, IsPruned, 
		   HasRegrowth, IsStrung, LengthIsEstimate, ShootDiameter, Comments)

#Shoot types by length only
all_shoot_data <- within(all_shoot_data, ShootTypeVeryCoarse <- 
					ifelse(ShootLength<35, "short",	
						   ifelse(ShootLength>=180, "long", "medium")))


#Shoot types by coarse category
all_shoot_data %<>%
	mutate(ShootTypeCoarse = ifelse((ShootLength<35 & is.na(IsPruned)), "short", 
		ifelse(ShootLength>=180 | (IsPruned=="TRUE" & ShootDiameter>=9), "long", 
				ifelse((ShootLength>=35 & (ShootLength<180 | ShootDiameter<9)), "medium", "ERROR"))))


#Shoot types by refined category, incorporating length, diameter, and pruning status
all_shoot_data <- within(all_shoot_data, ShootTypeRefined <- 
	ifelse((ShootLength<=1 & is.na(IsPruned)),"stub", 
		ifelse((ShootLength>1 & ShootLength<=10 & is.na(IsPruned)),"very short", 
			ifelse((ShootLength<=35 & ShootLength>5 & is.na(IsPruned)),"short", 
				ifelse((ShootLength>35 & ShootLength<=180 & is.na(IsPruned)),"medium",
					ifelse((ShootDiameter<9 & IsPruned=="TRUE"),"medium pruned", "ERROR"))))))
						ifelse((ShootLength>180 & ShootLength<500 & is.na(IsPruned)),"long",
							ifelse((ShootDiameter>=9 & ShootLength>=40 & IsPruned=="TRUE"), "long pruned",
								ifelse((ShootDiameter>=9 & ShootLength<40 & IsPruned=="TRUE"),"long stubbed", 
									ifelse(ShootLength>=500,"very long","ERROR"))))))))))						
						 
# for(vine_id in 1:9){
# 	# using numbers to reference column locations as most have spaces in the original
# 	temp_shoots <- read_csv(paste0("input/shoot_data/shoot_data_vine", vine_id ,".csv")) %>%
# 						rename(LeafLoss = 5, NumFruit = 6, ShootLength = 7,
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


