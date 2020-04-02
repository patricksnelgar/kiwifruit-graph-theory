
vine4_data <- read_csv("input/kiwimac_data_vine4.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine4_sources <- vine4_data %>%
	distinct(from) %>%
	rename(label = from)

vine4_targets <- vine4_data %>%
	distinct(to) %>%
	rename(label = to)

vine4_nodes <- full_join(vine4_sources, vine4_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine4_data, to, length, to_shoot_id, x, y, quadrant, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine4_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine4_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier) %>%
	select(label:to_shoot_id, target_type, origin_target_id, x_pos, y_pos, to_origin_id) %>%
	filter(!is.na(label))

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
