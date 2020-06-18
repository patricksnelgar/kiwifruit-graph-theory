
vine1_graph <- tbl_graph(vine1_nodes, vine1_data) %>%
	activate(nodes) %>%
	mutate(cost_to_origin = node_distance_from(origin_target_id, weights = length)) %>%
	mutate(target_label = ifelse(!is.na(to_shoot_id), to_shoot_id, to_origin_id))


ggraph(vine1_graph, layout = "manual", x = vine1_nodes$y_pos, y = vine1_nodes$x_pos) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(fill = target_type), shape = 21, size = 5) + 
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 1") +
	geom_text(x = 0, y = 1750, label = "N", size = 14) +
	geom_text(x = 0, y = -1690, label = "S", size = 14) +
	theme_graph()


ggsave("output/vine 1/x-and-y_layout.png", width = 20, height = 20)


ggraph(vine1_graph, 'tree') +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 6) +
	geom_node_text(aes(label = target_label, colour = target_type), repel = TRUE) +
	ggtitle("Kiwimac - Vine 1 architecture") +
	theme_graph() +
	theme(text = element_text(size = 14), title = element_text(size = 18))

ggsave("output/vine 1/tree_architecture.png", width = 35, height = 20)


