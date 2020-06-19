all_shoot_data <- NA

for(vine_id in 1:9){
	# using numbers to reference column locations as most have spaces in the original
	temp_shoots <- read_csv(paste0("input/shoot_data/shoot_data_vine", vine_id ,".csv")) %>%
						rename(LeafLoss = 5, NumFruit = 6, ShootLength = 7,
							   ShootType = 8, IsPruned = 9, HasRegrowth = 10,
							   IsStrung = 11, ShootDiameter = 12, Comments = 13) %>%
						mutate(ShootUUID = paste(vine_id, Shoot, sep = "-"),
							   ShootDiameter = as.numeric(ShootDiameter),
							   LeafLoss = as.numeric(LeafLoss),
							   VineUUID = vine_id) %>%
						select(VineUUID, ShootUUID, LeafLoss:Comments)
	
	
	if(length(all_shoot_data) <= 1)
		all_shoot_data <- temp_shoots
	else
		all_shoot_data %<>% bind_rows(temp_shoots)
	
}

rm(temp_shoots)
