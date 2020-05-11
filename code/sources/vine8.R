
vine8_data <- read_csv("input/kiwimac_data_vine8.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine8_sources <- vine8_data %>%
	distinct(from) %>%
	rename(label = from)

vine8_targets <- vine8_data %>%
	distinct(to) %>%
	rename(label = to)

vine8_nodes <- full_join(vine8_sources, vine8_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine8_data, to, length, to_shoot_id, x, y, quadrant, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine8_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine8_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier*-1) %>%
	select(label:to_shoot_id, target_type, origin_target_id, x_pos, y_pos, to_origin_id) %>%
	filter(!is.na(label))

vine8_graph <- tbl_graph(vine8_nodes, vine8_data) %>%
	activate(nodes) %>%
	mutate(cost_to_origin = node_distance_from(origin_target_id, weights = length)) %>%
	mutate(target_label = ifelse(!is.na(to_shoot_id), to_shoot_id, to_origin_id))


ggraph(vine8_graph, layout = "tree") + 
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 6) +
	geom_node_text(aes(label = target_label, colour = target_type), repel = TRUE, size = 6) +
	ggtitle("Kiwimac - Vine 8 architecture") + 
	theme_graph() +
	theme(text = element_text(size = 14), title = element_text(size = 18))

ggsave("output/graphs/kiwimac_vine8.png", width = 49, height = 20)


ggraph(vine8_graph, layout = "manual", x = vine8_nodes$x_pos, y = vine8_nodes$y_pos) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(fill = target_type), shape = 21, size = 5) + 
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 8") +
	#geom_text(x = 0, y = 1750, label = "N", size = 14) +
	#geom_text(x = 0, y = -1700, label = "S", size = 14) +
	geom_vline(xintercept = c(-1000, -500, 0, 500, 1000)) +
	geom_hline(yintercept = c(-2000, -1000, 0, 1000, 2000)) +
	geom_text(aes(x = x, y = y, label = label), data = quadrant_labels, size = 14) +
	theme_graph()


ggsave("output/graphs/kiwimac_vine8_layout.png", width = 20, height = 20)