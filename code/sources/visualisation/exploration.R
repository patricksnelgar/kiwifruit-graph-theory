
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
	ggplot() +
		geom_bin2d(aes(column, row, fill = dm_bins), stat = "identity") + 
		scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") + 
		geom_text(aes(column, row, label = quadrant), alpha = 0.5) +
		ggtitle("Average dry matter by quadrant - All vines") + 
		labs(x = NULL, y = NULL) +
		theme_patrick() +
		theme(axis.text.x = element_blank(), axis.text.y = element_blank())

ggsave("output/all_vines/Average dry matter by quadrant - all vines.jpg", width = 12, height = 8)

summarised_fruit_data %>%
	ggplot() +
	geom_bin2d(aes(column, row, fill = dm_bins), stat = "identity") +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	geom_text(aes(column, row, label = quadrant)) +
	facet_wrap(~Vine) + 
	ggtitle("Average dry matter by quadrant, by vine") +
	theme_patrick() + 
	theme(axis.text.x = element_blank(), axis.text.y = element_blank())

ggsave("output/all_vines/Average dry matter by quadrant by vines.jpg", width = 12, height = 8)

#### trying linear model for dry matter vs quadrant from leader ####
tmp_dm <- filter(summarised_fruit_data, Vine == 1)$avg_dm
tmp_quad <- filter(summarised_fruit_data, Vine == 1)$QuadrantFromLeader

dm_reg <- lm(tmp_dm ~ tmp_quad)
dm_reg_summary <- summary(dm_reg)

summarised_fruit_data %>%
	filter(Vine == 1) %>%
	ggplot() +
		geom_point(aes(QuadrantFromLeader, avg_dm)) +
		geom_line(aes(tmp_quad, predict(dm_reg, newdata = data.frame(tmp_quad = c(1:3)))), data = data.frame(tmp_quad = c(1:3)), colour = "blue") +
		labs(caption = 
			 	bquote(~R^2 ~ "=" ~ .(format(dm_reg_summary$r.squared, digits = 3)) ~ 
			 		   	"    Adjusted" ~ R^2 ~ "=" ~ .(format(dm_reg_summary$adj.r.squared, digits = 3)))) +
		ggtitle("Linear regression of dry matter vs relative quadrant from leader - Vine 1") +
		theme_patrick()

ggsave("output/vine 1/Dry matter vs relative quadrant from leader.jpg", width = 12, height = 8)

