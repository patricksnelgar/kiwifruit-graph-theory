
vine2_data <- read_csv("input/kiwimac_data_vine2.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine2_sources <- vine2_data %>%
	distinct(from) %>%
	rename(label = from)

vine2_targets <- vine2_data %>%
	distinct(to) %>%
	rename(label = to)

vine2_nodes <- full_join(vine2_sources, vine2_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine2_data, to, length, to_shoot_id, x, y, quadrant, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine2_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine2_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier) %>%
	select(label:to_shoot_id, target_type, origin_target_id, x_pos, y_pos, to_origin_id)

vine2_graph <- tbl_graph(vine2_nodes, vine2_data) %>%
	activate(nodes) %>%
	mutate(cost_to_origin = node_distance_from(origin_target_id, weights = length)) %>%
	mutate(target_label = ifelse(!is.na(to_shoot_id), to_shoot_id, to_origin_id))


ggraph(vine2_graph, 'tree') +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 6) +
	geom_node_text(aes(label = target_label, colour = target_type), repel = TRUE) +
	ggtitle("Kiwimac - Vine 2 architecture") +
	theme_graph() +
	theme(text = element_text(size = 14), title = element_text(size = 18))

ggsave("output/graphs/kiwimac_vine2.png", width = 35, height = 20)




ggraph(vine2_graph, layout = "manual", x, y) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 5) + 
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 2") +
	geom_text(x = 0, y = 1750, label = "N", size = 14) +
	geom_text(x = 0, y = -1700, label = "S", size = 14) +
	theme_graph()
	

ggsave("output/graphs/kiwimac_vine2_layout.png", width = 20, height = 20)



