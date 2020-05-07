
vine4_data <- read_csv("input/kiwimac_data_vine4.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine4_fruit_data <- read_csv("input/fruit_data_vine4.csv") %>%
						select(FruitID:Height, FrtWt:DM) %>%
						mutate(shoot_id = paste("4", ShootID, sep = "-"))
						

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
					theme_bw() + 
					theme(plot.title = element_text(size = 22, hjust = 0.5))


vine4_interactive_fruit <- ggplotly(vine4_fruit_dm)

# Fruit data points
vine4_interactive_fruit$x$data[[483]]$text <- vine4_fruit_data %>%
												filter(taste_bin == "Under MTS") %$%
												paste0("FruitID: ", FruitID, 
													"\nFW: ", FrtWt, "g",
													"\nDM: ", DM, "%")
# Fruit data points
vine4_interactive_fruit$x$data[[484]]$text <- vine4_fruit_data %>%
												filter(taste_bin == "M band") %$%
												paste0("FruitID: ", FruitID, 
													  "\nFW: ", FrtWt, "g",
													  "\nDM: ", DM, "%")
# Fruit data points
vine4_interactive_fruit$x$data[[485]]$text <- vine4_fruit_data %>%
												filter(taste_bin == "T band") %$%
												paste0("FruitID: ", FruitID, 
													  "\nFW: ", FrtWt, "g",
													  "\nDM: ", DM, "%")
# Fruit data points
vine4_interactive_fruit$x$data[[486]]$text <- vine4_fruit_data %>%
												filter(taste_bin == "Y band") %$%
												paste0("FruitID: ", FruitID, 
													  "\nFW: ", FrtWt, "g",
													  "\nDM: ", DM, "%")
# shoot data
vine4_interactive_fruit$x$data[[482]]$text <- paste("Shoot ID: ",
													filter(vine4_links, target_type == "Shoot")$to_shoot_id)

# edit the fucking names because plotly also doesnt honour lables
vine4_interactive_fruit$x$data[[482]]$name <- "Shoots"
vine4_interactive_fruit$x$data[[483]]$name <- "Under MTS"
vine4_interactive_fruit$x$data[[484]]$name <- "M band"
vine4_interactive_fruit$x$data[[485]]$name <- "T band"
vine4_interactive_fruit$x$data[[486]]$name <- "Y band"

# Fucking stupid, excpects full path, cant do relative WHY???
saveWidget(vine4_interactive_fruit, "kiwimac_vine4_interactive.html", selfcontained = TRUE)

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
