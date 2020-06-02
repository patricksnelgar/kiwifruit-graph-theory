all_fruit_data <- bind_rows(vine1_fruit_data, vine2_fruit_data, vine3_fruit_data,
							vine4_fruit_data, vine5_fruit_data, vine6_fruit_data, 
							vine7_fruit_data, vine8_fruit_data, vine9_fruit_data) %>%
	mutate(VineTreatmentOrder = factor(Vine, levels = c(1, 2, 3, 4, 6, 5, 9, 8, 7)),
		   VineTreatment = ifelse(Vine %in% c(1, 4, 9), "1 Conv", 
		   					   ifelse(Vine %in% c(2, 6, 8), "2 Stru",
		   					   	   ifelse(Vine %in% c(3, 5, 7), "3 Spur", NA))),
		   VineRow = ifelse(Vine %in% c(1:3), 1,
		   				 ifelse(Vine %in% c(4:6), 2,
		   				 	   ifelse(Vine %in% c(7:9), 3, NA)))) %>%
	mutate(dm_bins = cut(DryMatter, c(0,14:23,30), include.lowest = TRUE, labels = c("< 14", paste(14:22, "to", 15:23), "> 23")),
		   fw_bins = cut(FreshWeight, c(0, 74, 84, 95, 108, 118, 128, 138, 151, 180, 210, 240), 
		   			  include.lowest = TRUE, 
		   			  right = FALSE,
		   			  labels = c("< 74g", paste("count", c(42, 39, 36, 33, 30, 27, 25, 22, 18, 16)))),
		   hue_bins = cut(Hue1, seq(0, 120, 5), labels = paste(seq(0, 115, 5), "to", seq(5, 120, 5))),
		   ssc_bins = cut(SSC1, seq(5, 20, 1.5), include.lowest = TRUE, labels = paste(seq(5, 18.5, 1.5), "to", seq(6.5, 20, 1.5))),
		   firm_bins = cut(Firm1, seq(0, 11, 1), include.lowest = TRUE, labels = paste(0:10, "to", 1:11)))

all_fruit_data %<>%
	left_join(flowering_dates, by = c("FloweringColour" = "FloweringColour"))


all_arch_data <- bind_rows(mutate(vine1_links, Vine = 1),
						   mutate(vine2_links, Vine = 2),
						   mutate(vine3_links, Vine = 3),
						   mutate(vine4_links, Vine = 4),
						   mutate(vine5_links, Vine = 5),
						   mutate(vine6_links, Vine = 6),
						   mutate(vine7_links, Vine = 7),
						   mutate(vine8_links, Vine = 8),
						   mutate(vine9_links, Vine = 9)) %>%
	mutate(VineTreatmentOrder = factor(Vine, levels = c(1, 2, 3, 4, 6, 5, 9, 8, 7)),
		   VineTreatment = ifelse(Vine %in% c(1, 4, 9), "1 Conv", 
		   					   ifelse(Vine %in% c(2, 6, 8), "2 Stru",
		   					   	   ifelse(Vine %in% c(3, 5, 7), "3 Spur", NA))),
		   VineRow = ifelse(Vine %in% c(1:3), 1,
		   				 ifelse(Vine %in% c(4:6), 2,
		   				 	   ifelse(Vine %in% c(7:9), 3, NA))))


all_fruit_data %<>%
	left_join(relative_quadrants, by = c("quadrant" = "quadrant")) %>%
	select(Vine:quadrant, QuadrantFromLeader, QuadrantFromTrunk, FruitPos:firm_bins)

write_csv(all_fruit_data, "workspace/all_fruit_data.csv")
write_csv(all_arch_data, "workspace/all_arch_data.csv")
