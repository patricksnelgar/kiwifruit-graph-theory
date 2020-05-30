

grid_quadrants <- data.frame(row = rep(6:1, each = 6), column = rep(1:6, 6), quadrant = 1:36)


summarised_fruit _data <- all_fruit_data %>%
	group_by(Vine, quadrant) %>%
	summarise(avg_dm = mean(DryMatter, na.rm = TRUE), avg_fw = mean(FreshWeight, na.rm = TRUE))

summarised_fruit_data %>%
	ggplot() +
		geom_point(aes(quadrant, avg_dm)) + 
		facet_wrap(~Vine)

summarised_fruit_data %>%
	left_join(grid_quadrants, by = c("quadrant" = "quadrant")) %>%
	ggplot() +
		geom_bin2d(aes(column, row, fill = avg_dm), stat = "identity")


summarised_fruit_data %>%
	left_join(grid_quadrants, by = c("quadrant" = "quadrant")) %>%
	ggplot() +
	geom_bin2d(aes(column, row, fill = avg_dm), stat = "identity") +
	facet_wrap(~Vine)
