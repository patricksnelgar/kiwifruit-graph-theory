ggraph(vine2_graph, 'manual', x = y_pos, y = x_pos) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 6) +
	#geom_node_text(aes(label = target_label, colour = target_type), repel = TRUE) +
	ggtitle("Kiwimac - Vine 2 architecture") +
	theme_graph() +
	theme(text = element_text(size = 14), title = element_text(size = 18)) +
	transition_reveal(label)
