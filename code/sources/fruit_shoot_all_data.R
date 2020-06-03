relative_quadrants <- data.frame(quadrant = c(1:36), 
								 QuadrantFromLeader = rep(c(3:1, 1:3), 6),
								 QuadrantFromTrunk = rep(c(3:1, 1:3), each = 6))

source("code/sources/data_import/vine1_import.R")
source("code/sources/data_import/vine2_import.R")
source("code/sources/data_import/vine3_import.R")
source("code/sources/data_import/vine4_import.R")
source("code/sources/data_import/vine5_import.R")
source("code/sources/data_import/vine6_import.R")
source("code/sources/data_import/vine7_import.R")
source("code/sources/data_import/vine8_import.R")
source("code/sources/data_import/vine9_import.R")


source("code/sources/merge_vine_data.R")


quadrant_





all_shoot_data <- NA

for(vine_num in 1:9) {
	temp <- read_csv(paste0("./input/shoot_data/shoot_data_vine", vine_num ,".csv")) %>%
				rename(LeafLoss = 3, NumFruit = 4, ShootLength = 5,
					   ShootType = 6, IsPruned = 7, HasRegrowth = 8,
					   IsStrung = 9, ShootDiameter = 10, Notes = 11) %>%
				mutate(ShootUUID = paste(vine_num, Shoot, sep = "-"),
					   ShootDiameter = as.numeric(ShootDiameter),
					   LeafLoss = as.numeric(LeafLoss),
					   Vine = vine_num) %>%
				select(Vine, ShootUUID, LeafLoss:Notes)
	
	if(length(all_shoot_data) <= 1)
		all_shoot_data <- temp
	else 
		all_shoot_data %<>% bind_rows(temp)

}


all_shoot_data %<>%
	left_join(select(all_arch_data, to_shoot_id, origin_target_id, quadrant), by = c("ShootUUID" = "to_shoot_id")) %>%
	left_join()
	rename(OriginID = origin_target_id, Quadrant = quadrant)
	