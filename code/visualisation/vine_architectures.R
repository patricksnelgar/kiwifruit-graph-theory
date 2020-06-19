

for(vine_id in 1:9) {
	temp_arch <- 
		all_arch_data %>%
		filter(VineUUID == vine_id)
	

	vine_nodes <- 
		temp_arch %$%
		c(ParentNodeID, NodeID) %>%
		unique() %>%
		as.data.frame(.) %>%
		rename(label = 1) %>%
		arrange(label)
	
	vine_nodes %<>%
		left_join(select(temp_arch, NodeID:ParentOriginID), by = c("label" = "NodeID")) %>%
		mutate(NodeType = factor(ifelse(!is.na(ShootUUID), "Shoot", 
								 ifelse(!is.na(OriginUUID), "Origin", "Junction")),
								 levels = c("Shoot", "Origin", "Junction")),
			   NodeLabel = ifelse(!is.na(ShootUUID), ShootUUID, OriginUUID))
	
	temp_arch %>%
		rename(from = ParentNodeID, to = NodeID) %>%	
		tbl_graph(vine_nodes, .) %>%
			ggraph('tree') +
				geom_edge_link(colour = "brown") +
				geom_node_point(aes(colour = NodeType), size = 6) +
				geom_node_text(aes(label = NodeLabel), repel = TRUE, size = 6) +
				ggtitle(paste("Vine", vine_id, "tree architecture")) +
				theme_graph()
		
	
	ggsave(paste0("output/Vine ", vine_id, "/vine", vine_id, "_tree_architecture.jpg"), width = 30, height = 20)
}
