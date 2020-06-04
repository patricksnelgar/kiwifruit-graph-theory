
relative_qaudrants <- data.frame(quadrant = c(1:36), 
								 QuadrantFromLeader = rep(c(3:1, 1:3), 6),
								 QuadrantFromTrunk = rep(c(3:1, 1:3), each = 6))

flowering_dates <- read_csv("input/flowering_dates.csv") %>%
					mutate(FloweringColour = tolower(FloweringColour),
						   FloweringDate = dmy(FloweringDate))

all_fruit_data <- NULL

for(vine_id in 1:9){
	
	fruit_data <- read_csv(paste0("input/fruit_data/fruit_data_vine", vine_id, ".csv")) %>%
					mutate(ShootID = replace_na(ShootID, 1000))
	
	arch_data <- read_csv(paste0("input/architecture/kiwimac_data_vine", vine_id, ".csv")) %>%
					mutate(shoot_id = paste(vine_id, to_shoot_id, sep = "-")) %>%
					filter(!is.na(shoot_id))
	
	fruit_data %<>%
		mutate(ShootUUID = paste(Vine, ShootID, sep = "-"),
			   FloweringColour = tolower(FloweringColour)) %>%
		left_join(select(arch_data, shoot_id, quadrant), c("ShootUUID" = "shoot_id")) %>%
		left_join(relative_qaudrants, c("quadrant" = "quadrant")) %>%
		left_join(flowering_dates, c("FloweringColour" = "FloweringColour")) %>%
		rename(Quadrant = quadrant) %>%
		select(Vine:TrayFruit, FruitUUID, ShootUUID:QuadrantFromTrunk, 
			   FruitPos:FloweringColour, FloweringDate, SubTLeaf:Comments)
	
	if(is.null(all_fruit_data))
		all_fruit_data <- fruit_data
	else
		all_fruit_data %<>% 
		bind_rows(fruit_data)
	
}

write_csv(all_fruit_data, "output/fruit_data_stacked.csv", na = "")
