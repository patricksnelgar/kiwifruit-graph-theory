# this file plots the network diagrams for each vine.

for(vine_id in 1:9) {
	
	temp_arch <- 
		all_arch_data %>%
		filter(VineUUID == vine_id)
	
	# need a list of all the possible node IDs
	vine_nodes <- 
		temp_arch %$%
		c(ParentNodeID, NodeID) %>%
		unique() %>%
		as.data.frame(.) %>%
		rename(label = 1) %>%
		arrange(label)
	
	# add architecture information for each node
	vine_nodes %<>%
		left_join(select(temp_arch, NodeID:ParentOriginID), by = c("label" = "NodeID")) %>%
		mutate(NodeType = factor(if_else(!is.na(ShootUUID), "Shoot", 
								 if_else(!is.na(OriginUUID), "Origin", "Junction")),
								 levels = c("Shoot", "Origin", "Junction")),
			   NodeLabel = if_else(!is.na(ShootUUID), ShootUUID, OriginUUID))


	# join previous node dataset and
	# edge dataset into a ggraph object then plot
	
	
	temp_arch %>%
		rename(from = ParentNodeID, to = NodeID) %>%	
		tbl_graph(vine_nodes, .) %>%
			ggraph('tree') +
				geom_edge_link(colour = "brown") +
				geom_node_point(aes(colour = NodeType), size = 6) +
				geom_node_text(aes(label = NodeLabel), repel = TRUE, size = 6) +
				ggtitle(paste("Vine", vine_id, "tree architecture")) +
				theme_graph()
		
	
	ggsave(here(paste0("output/Vine ", vine_id, "/vine", vine_id, "_tree_architecture.png")), width = 30, height = 20)
}
