


grid_quadrants <- data.frame(row = rep(1:6, each = 6), column = rep(1:6, 6), quadrant = 1:36)

summarised_fruit_data <- all_fruit_data %>%
	filter(!is.na(DryMatter) & !is.na(FreshWeight)) %>%
	group_by(Vine, quadrant) %>%
	summarise(avg_dm = mean(DryMatter, na.rm = TRUE), avg_fw = mean(FreshWeight, na.rm = TRUE)) %>%
	mutate(dm_bins = cut(avg_dm, c(0,14:23,30), include.lowest = TRUE, labels = c("< 14", paste(14:22, "to", 15:23), "> 23"))) %>%
	left_join(grid_quadrants, by = c("quadrant" = "quadrant")) %>%
	left_join(relative_quadrants, by = c("quadrant" = "quadrant"))

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
	geom_bin2d(aes(column, row, fill = dm_bins), stat = "identity") +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	facet_wrap(~Vine) + 
	ggtitle("Average Dry matter by quadrant, by vine") +
	theme_bw()


summarised_fruit_data %>%
	ggplot() +
		geom_point(aes(QuadrantFromLeader, avg_dm)) +
		theme_bw()

