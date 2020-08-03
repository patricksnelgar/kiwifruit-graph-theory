
all_ggraph_data <- NA

for(vine_id in 1:9){
	temp_arch <- 
		read_csv(here(paste0("input/architecture/kiwimac_data_vine", vine_id, ".csv"))) %>%
		mutate(ShootUUID = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
			   CaneUUID = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
			   OriginUUID = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA)) %>%
		rename(VineUUID = 1, ParentNodeID = 2, NodeID = 3, Comments = 15)
	
	# Isolate all the unique node IDs
	temp_sources <- temp_arch %>%
		distinct(ParentNodeID) %>%
		rename(label = ParentNodeID)
	
	temp_targets <- temp_arch %>%
		distinct(NodeID) %>%
		rename(label = NodeID)
	
	temp_nodes <- full_join(temp_sources, temp_targets, by = "label") %>%
		arrange(label) %>%
		full_join(select(temp_arch, VineUUID, NodeID, ShootUUID, OriginUUID, Comments), 
				  by = c("label" = "NodeID")) %>%
		mutate(NodeType = ifelse(!is.na(ShootUUID), "Shoot", ifelse(!is.na(OriginUUID), "Origin","Junction"))) 
	
	# Sets the node ID for the origin point the segment belongs to
	# this is for tracing path lengths
	temp_nodes %<>% 
		group_by(ParentOriginID) %>%
		mutate(OriginNodeID = first(label)) %>%
		ungroup()
	
	# Vine ID not need but smart to keep in
	temp_links <- temp_arch %>%
		select(VineUUID, ParentNodeID, NodeID) %>%
		rename(from = ParentNodeID, to = NodeID)
	
}