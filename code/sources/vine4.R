#### data import ####
vine4_data <- read_csv("input/kiwimac_data_vine4.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine4_fruit_data <- read_csv("input/fruit_data_vine4.csv") %>%
	select(FruitID:Height, FrtWt:DM) %>%
	mutate(shoot_id = paste("4", ShootID, sep = "-"))

#### data gathering ####

vine4_sources <- vine4_data %>%
	distinct(from) %>%
	rename(label = from)

vine4_targets <- vine4_data %>%
	distinct(to) %>%
	rename(label = to)

vine4_nodes <- full_join(vine4_sources, vine4_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine4_data, to, length, to_shoot_id, x, y, quadrant, diameter, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine4_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine4_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier) %>%
	select(label:to_shoot_id, target_type, origin_target_id, x_pos, y_pos, diameter, to_origin_id) %>%
	filter(!is.na(label))

vine4_links <- vine4_data %>%
	select(from, to) %>%
	full_join(select(vine4_nodes, label, x_pos, y_pos), by = c("from" = "label")) %>%
	rename(xstart = x_pos, ystart = y_pos) %>%
	full_join(vine4_nodes, by = c("to" = "label")) %>%
	rename(xend = x_pos, yend = y_pos) 

vine4_fruit_data %<>%
	left_join(., select(vine4_links, to_shoot_id, xend, yend), by = c("shoot_id" = "to_shoot_id")) 

vine4_fruit_data %<>%
	mutate(taste_bin = cut(DM, c(0, 16.1, 17.4, 17.9, Inf), include.lowest = TRUE, labels = c("Under MTS", "M band", "T band", "Y band")))


#### DM heatmap plot ####
colours1 <- c(col2hex("tomato2"),
			  col2hex("dimgrey"),
			  col2hex("steelblue2"), 
			  col2hex("plum"),
			  col2hex("springgreen3"))
			  						
# Y, Shoots, Under MTS, M, T
colours2 <- c(col2hex("goldenrod1"),
			  col2hex("dimgrey"),
			  col2hex("aquamarine3"),
			  col2hex("blueviolet"),
			  col2hex("red"))

vine4_fruit_dm <- ggplot(filter(vine4_links, !is.na(to))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey")+
	geom_point(aes(x = yend, y = xend, colour = target_type), 
			   data = filter(vine4_links, !is.na(to) & target_type == "Shoot"),
			   shape = 19, 
			   size = 4) + 
	scale_colour_manual(name = "target_type", 
						labels = c("Shoot", "Origin", "Junction"),
						values = c("Shoot" = "dimgrey")) +
	geom_jitter(aes(yend, xend, fill = taste_bin), 
				data = filter(vine4_fruit_data, !is.na(taste_bin)), 
				alpha = 0.8, 
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_manual(name = "Zespri Taste bands",
					  values = c("steelblue2", "plum", "springgreen3", "tomato2"), 
					  labels = c("Under MTS", "M band", "T band", "Y band")) +
	guides(size = FALSE, colour = FALSE) +
	ggtitle("Kiwimac Vine4 - DM heatmap") +
	labs(x = NULL, y = NULL) +
	theme_bw() + 
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())


vine4_interactive_dm <- ggplotly(vine4_fruit_dm)

# Fruit data points
vine4_interactive_dm$x$data[[483]]$text <- vine4_fruit_data %>%
	filter(taste_bin == "Under MTS") %$%
	paste0("FruitID: ", FruitID, 
		   "\nFW: ", FrtWt, "g",
		   "\nDM: ", DM, "%")
# Fruit data points
vine4_interactive_dm$x$data[[484]]$text <- vine4_fruit_data %>%
	filter(taste_bin == "M band") %$%
	paste0("FruitID: ", FruitID, 
		   "\nFW: ", FrtWt, "g",
		   "\nDM: ", DM, "%")
# Fruit data points
vine4_interactive_dm$x$data[[485]]$text <- vine4_fruit_data %>%
	filter(taste_bin == "T band") %$%
	paste0("FruitID: ", FruitID, 
		   "\nFW: ", FrtWt, "g",
		   "\nDM: ", DM, "%")
# Fruit data points
vine4_interactive_dm$x$data[[486]]$text <- vine4_fruit_data %>%
	filter(taste_bin == "Y band") %$%
	paste0("FruitID: ", FruitID, 
		   "\nFW: ", FrtWt, "g",
		   "\nDM: ", DM, "%")
# shoot data
vine4_interactive_dm$x$data[[482]]$text <- paste("Shoot ID: ",
													filter(vine4_links, target_type == "Shoot")$to_shoot_id)

# edit the fucking names because plotly also doesnt honour lables
vine4_interactive_dm$x$data[[482]]$name <- "Shoots"
vine4_interactive_dm$x$data[[483]]$name <- "Under MTS"
vine4_interactive_dm$x$data[[484]]$name <- "M band"
vine4_interactive_dm$x$data[[485]]$name <- "T band"
vine4_interactive_dm$x$data[[486]]$name <- "Y band"


# playing around with buttons 
# it works!
vine4_interactive_dm %<>% 
	layout(updatemenus = list(
		list(
			type = 'buttons',
			y = 1,
			buttons = list(
				list(
					label = "Palette 1",
					method = "restyle",
					args = list("marker.color", colours1)
				),
				list(
					label = "Palette 2",
					method = "restyle",
					args = list("marker.color", colours2)
				)
			)
		)
	))

# Working dropdown to show / hide different markers
# list(
# 	type = "dropdown",
# 	y = 0.8,
# 	buttons = list(
# 		list(
# 			label = "DM",
# 			method = "restyle",
# 			args = list("visible", list(TRUE, TRUE, TRUE, TRUE, TRUE))
# 		),
# 		list(
# 			label = "FW",
# 			method = "restyle",
# 			args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE))
# 		)
# 	)
# )


# Fucking stupid, expects full path, cant do relative WHY???
saveWidget(vine4_interactive_dm, "kiwimac vine4 dry matter.html", selfcontained = TRUE)

#### Interactive fruit height ####
vine4_fruit_height <- ggplot(filter(vine4_links, !is.na(to))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey")+
	geom_point(aes(x = yend, y = xend, colour = target_type), 
			   data = filter(vine4_links, !is.na(to) & target_type == "Shoot"),
			   shape = 19, 
			   size = 4) + 
	scale_colour_manual(name = "target_type", 
						labels = c("Shoot", "Origin", "Junction"),
						values = c("Shoot" = "dimgrey")) +
	geom_jitter(aes(yend, xend, fill = factor(Height)), 
				data = filter(vine4_fruit_data, !is.na(Height)), 
				alpha = 0.8, 
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	# scale_fill_manual(name = "Zespri Taste bands",
	# 				  values = c("steelblue2", "plum", "springgreen3", "tomato2"), 
	# 				  labels = c("Under MTS", "M band", "T band", "Y band")) +
	guides(size = FALSE, colour = FALSE) +
	ggtitle("Kiwimac Vine4 - fruit height") +
	labs(x = NULL, y = NULL) +
	theme_bw() + 
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())


vine4_interactive_dm_height <- ggplotly(vine4_fruit_height)



#### Interactive flower timing ####
vine4_flower_timing <- ggplot(filter(vine4_links, !is.na(to))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey")+
	geom_point(aes(x = yend, y = xend, colour = target_type), 
			   data = filter(vine4_links, !is.na(to) & target_type == "Shoot"),
			   shape = 19, 
			   size = 4) + 
	scale_colour_manual(name = "target_type", 
						labels = c("Shoot", "Origin", "Junction"),
						values = c("Shoot" = "dimgrey")) +
	geom_jitter(aes(yend, xend, fill = Flowering), 
				data = filter(vine4_fruit_data, !is.na(Flowering) & Flowering != "."), 
				alpha = 0.8, 
				size = 4,
				width = 10,
				height = 90,
				shape = 21) +
	scale_fill_manual(name = "Flower Timing",
					  values = c("bl" = "black", 
					  		   "g" = "azure4", 
					  		   "p" = "darkorchid3", 
					  		   "pi" = "hotpink2", 
					  		   "r" = "orangered2", 
					  		   "w" = "ivory2", 
					  		   "y" = "gold2"),
					  labels = c("Black", "Grey", "Purple", "Pink", "Red", "White", "Yellow")) +
	guides(size = FALSE, colour = FALSE) +
	ggtitle("Kiwimac Vine4 - Flower Timing") +
	labs(x = NULL, y = NULL) +
	theme_bw() + 
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())


vine4_interactive_flower_timing <- ggplotly(vine4_flower_timing)

vine4_interactive_flower_timing$x$data[[482]]$name <- "Shoots"
vine4_interactive_flower_timing$x$data[[483]]$name <- "Black"
vine4_interactive_flower_timing$x$data[[484]]$name <- "Grey"
vine4_interactive_flower_timing$x$data[[485]]$name <- "Purple"
vine4_interactive_flower_timing$x$data[[486]]$name <- "Pink"
vine4_interactive_flower_timing$x$data[[487]]$name <- "Red"
vine4_interactive_flower_timing$x$data[[488]]$name <- "White"
vine4_interactive_flower_timing$x$data[[489]]$name <- "Yellow"

saveWidget(vine4_interactive_flower_timing, "kiwimac vine4 flower timing.html")


#### Histograms ####

vine4_Fw_hist <- vine4_fruit_data %>%
	filter(!is.na(FrtWt)) %>%
	ggplot() +
		geom_histogram(aes(FrtWt, fill = taste_bin), bins = 100) +
		ggtitle("Frequency of Fw coloured by taste band") + 
		labs(x = "Fresh weight (g)", y = "Frequency") +
		theme_bw()

ggsave("output/Vine 4/FW by tastebin.jpg", width = 12,height = 6)

vine4_DM_hist <- vine4_fruit_data %>%
	filter(!is.na(DM)) %>%
	ggplot() +
		geom_histogram(aes(DM, fill = taste_bin), bins = 100) +
		ggtitle("Frequency of DM coloured by taste band") + 
		labs(x = "Dry matter (%)", y = "Frequency") +
		theme_bw()

ggsave("output/Vine 4/DM by tastebin.jpg", width = 12,height = 6)


#### Scatter plots ####
vine4_fruit_data %>%
	filter(!is.na(DM) & !is.na(FrtWt) & !is.na(Height)) %>%
	ggplot() +
		geom_point(aes(DM, Height, colour = taste_bin), size = 2) +
		ggtitle("DM vs Height coloured by taste bin") + 
		labs(x = "Dry matter (%)", y = "distance from canopy wire (cm)") +
		theme_bw()

ggsave("output/Vine 4/DM vs Height coloured by taste band.jpg", width = 12, height = 6)

vine4_fruit_data %>%
	filter(!is.na(DM) & !is.na(FrtWt)) %>%
	ggplot() +
		geom_point(aes(DM, FrtWt, colour = taste_bin), size = 2) +
		ggtitle("DM vs FW coloured by taste bin") + 
		labs(x = "Dry matter (%)", y = "Fresh weight (g)") +
		theme_bw()

ggsave("output/Vine 4/DM vs FW coloured by taste band.jpg", width = 12, height = 6)

vine4_fruit_data %>%
	mutate(distance_from_leader = abs(yend)) %>%
	filter(!is.na(DM)) %>%
	ggplot() +
		geom_point(aes(distance_from_leader, DM, colour = taste_bin), size = 2) +
		ggtitle("Distance from leader (absolute) vs DM coloured by taste band") +
		labs(x = "Distance from leader (cm)", y = "Dry matter (%)") +
		theme_bw()

ggsave("output/Vine 4/Distance from leader vs DM.jpg", width = 12, height = 8)

### ggraph stuff ####
vine4_graph <- tbl_graph(vine4_nodes, vine4_data) %>%
	activate(nodes) %>%
	mutate(cost_to_origin = node_distance_from(origin_target_id, weights = length)) %>%
	mutate(target_label = ifelse(!is.na(to_shoot_id), to_shoot_id, to_origin_id))

ggraph(vine4_graph, layout = "manual", x = vine4_nodes$y_pos, y = vine4_nodes$x_pos) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 5) +
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 4") +
	geom_text(x = 0, y = 1750, label = "N", size = 14) +
	geom_text(x = 0, y = -1690, label = "S", size = 14) +
	theme_graph()


ggsave("output/graphs/kiwimac_vine4_layout.png", width = 20, height = 20)


ggraph(vine4_graph, 'tree') +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 6) +
	geom_node_text(aes(label = target_label, colour = target_type), repel = TRUE) +
	ggtitle("Kiwimac - Vine 4 architecture") +
	theme_graph() +
	theme(text = element_text(size = 14), title = element_text(size = 18))

ggsave("output/graphs/kiwimac_vine4.png", width = 35, height = 20)

