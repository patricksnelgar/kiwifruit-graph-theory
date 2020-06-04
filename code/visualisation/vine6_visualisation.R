
#### DryMatter heatmap plot ####
vine6_basic_dm <- ggplot(filter(vine6_links, !is.na(to))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10))+
	geom_point(aes(x = yend, y = xend, colour = target_type), 
			   data = filter(vine6_links, !is.na(to) & target_type == "Shoot"),
			   shape = 19, 
			   size = 4) + 
	scale_colour_manual(name = "target_type", 
						labels = c("Shoot"),
						values = c("Shoot" = "dimgrey")) +
	geom_jitter(aes(yend, xend, fill = dm_bins), 
				data = filter(vine6_fruit_data, !is.na(dm_bins)), 
				alpha = 0.8, 
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(type = "div", palette = "RdYlBu", direction = "-1") +
	guides(size = FALSE) +
	ggtitle("Kiwimac vine6 - basic Dry Matter heatmap") +
	labs(x = NULL, y = NULL) +
	theme_bw() + 
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

ggsave("./output/vine 6/basic Dry Matter heatmap.jpg", width = 16, height = 12)


#### ggraph stuff ####
vine6_graph <- tbl_graph(vine6_nodes, vine6_data) %>%
	activate(nodes) %>%
	mutate(cost_to_origin = node_distance_from(origin_target_id, weights = length)) %>%
	mutate(target_label = ifelse(!is.na(to_shoot_id), to_shoot_id, to_origin_id))


ggraph(vine6_graph, layout = "manual", x = vine6_nodes$y_pos, y = vine6_nodes$x_pos) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(fill = target_type), shape = 21, size = 5) + 
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 6") +
	geom_text(x = 0, y = 1550, label = "N", size = 14) +
	geom_text(x = 0, y = -1550, label = "S", size = 14) +
	# geom_vline(xintercept = c(-1000, -500, 0, 500, 1000)) +
	# geom_hline(yintercept = c(-2000, -1000, 0, 1000, 2000)) +
	# geom_text(aes(x = x, y = y, label = label), data = quadrant_labels, size = 14) +
	 theme_graph()


ggsave("output/vine 6/x-and-y_layout.png", width = 20, height = 20)


ggraph(vine6_graph, layout = "tree") + 
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 6) +
	geom_node_text(aes(label = target_label, colour = target_type), repel = TRUE, size = 6) +
	ggtitle("Kiwimac - Vine 6 architecture") + 
	theme_graph() +
	theme(text = element_text(size = 14), title = element_text(size = 18))

ggsave("output/vine 6/tree_architecture.png", width = 49, height = 20)