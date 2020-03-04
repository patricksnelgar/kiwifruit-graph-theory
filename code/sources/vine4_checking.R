## code for highlighting a path
vine4_graph %>%
	activate(nodes) %>%
	morph(to_shortest_path, 275, 1, weights = length) %>%
	mutate(active_path = 1) %>%
	activate(edges) %>%
	mutate(active_path = 1) %>%
	unmorph() %>%
	mutate(active_path = ifelse(is.na(active_path), 1, 2)) %>%
	activate(nodes) %>%
	mutate(active_path = ifelse(is.na(active_path), 1, 2)) %>%
	ggraph(layout = "manual", x = vine4_nodes$y_pos, y = vine4_nodes$x_pos) +
	geom_edge_link(aes(alpha = active_path), colour = "brown") +
	geom_node_point(aes(colour = target_type, alpha = active_path), size = 5) + 
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 4") +
	geom_text(x = 0, y = 1750, label = "N", size = 14) +
	geom_text(x = 0, y = -1700, label = "S", size = 14) +
	theme_graph() + 
	guides(alpha = FALSE)

