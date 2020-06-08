all_fruit_data <- bind_rows(vine1_fruit_data, vine2_fruit_data, vine3_fruit_data,
							vine4_fruit_data, vine5_fruit_data, vine6_fruit_data, 
							vine7_fruit_data, vine8_fruit_data, vine9_fruit_data)

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
						   mutate(vine9_links, Vine = 9))


all_fruit_data %<>%
	left_join(select(quadrant_info, quadrant, QuadrantFromLeader:NorthSouth), by = c("quadrant" = "quadrant")) %>%
	select(Vine:quadrant, QuadrantFromLeader:NorthSouth, FruitPos:FloweringColour, FloweringDate, SubTLeaf:Comments)


