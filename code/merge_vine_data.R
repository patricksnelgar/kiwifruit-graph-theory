all_fruit_data <- NA
all_shoot_data <- NA
all_arch_data <- NA

for(vine_id in 1:9){
	temp_arch <- read_csv(paste0("input/architecture/kiwimac_data_vine", vine_id, ".csv")) %>%
					mutate(ShootUUID = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
						   CaneUUID = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
						   OriginUUID = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
						   ParentOriginID = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA)) %>%
					rename(VineUUID = 1, ParentNodeID = 2, NodeID = 3, SegmentLength = 4, 
						   DistanceFromQuadrantX = 8, DistanceFromQuadrantY = 9,
						   Quadrant = 10, SegmentDiameter = 11, SegmentOrientation = 13, Comments = 15)
		
	
	
	# Build the linkage map for tree visualisation
	temp_sources <- temp_arch %>%
						distinct(ParentNodeID) %>%
						rename(label = ParentNodeID)
	
	temp_targets <- temp_arch %>%
						distinct(NodeID) %>%
						rename(label = NodeID)
	
	# TODO: reorganise the columns to have all the stuff about the node close to the NodeID column
	#		and have the 'meta' data () further to the right
	temp_nodes <- full_join(temp_sources, temp_targets, by = "label") %>%
					arrange(label) %>%
					full_join(select(temp_arch, VineUUID, NodeID:SegmentLength, ShootUUID:ParentOriginID, DistanceFromQuadrantX:Comments), 
							  by = c("label" = "NodeID")) %>%
					mutate(NodeType = ifelse(!is.na(ShootUUID), "Shoot", ifelse(!is.na(OriginUUID), "Origin","Junction"))) 
	
	# Sets the node ID for the origin point the segment belongs to.
	temp_nodes %<>% 
		group_by(ParentOriginID) %>%
		mutate(OriginNodeID = first(label)) %>%
		ungroup()
	
	temp_nodes %<>% 
		full_join(quadrant_info, by = c("Quadrant" = "quadrant")) %>%
		mutate(DistanceFromTrunk = (DistanceFromQuadrantX + x_offset) * x_multiplier,
			   DistanceFromLeader = (DistanceFromQuadrantY + y_offset) * y_multiplier) %>%
		filter(!is.na(label)) %>%
		select(VineUUID, label, ShootUUID:OriginUUID, SegmentLength, SegmentDiameter, SegmentOrientation, 
			   DistanceFromTrunk, DistanceFromLeader, Quadrant, QuadrantFromLeader:NorthSouth, 
			   ParentOriginID, NodeType, Comments) %>%
		rename(NodeID = label)
	
	temp_links <- temp_arch %>%
					select(ParentNodeID, NodeID) %>%
					left_join(select(temp_nodes, NodeID, DistanceFromTrunk, DistanceFromLeader), 
							  by = c("ParentNodeID" = "NodeID")) %>%
					rename(SegmentStartX = DistanceFromTrunk, SegmentStartY = DistanceFromLeader) %>%
					left_join(temp_nodes, by = "NodeID") %>%
					rename(SegmentEndX = DistanceFromTrunk, SegmentEndY = DistanceFromLeader)
					
}




all_fruit_data <- bind_rows(vine1_fruit_data, vine2_fruit_data, vine3_fruit_data,
							vine4_fruit_data, vine5_fruit_data, vine6_fruit_data, 
							vine7_fruit_data, vine8_fruit_data, vine9_fruit_data) %>%
					rename(FloweringColour = Flowering)

all_fruit_data %<>%
	left_join(flowering_dates, by = c("FloweringColour" = "FloweringColour"))


all_arch_data <- bind_rows(mutate(vine1_links, Vine = 1),
						   mutate(vine2_links, Vine = 2),
						   mutate(vine3_links, Vine = 3),
						   mutate(vine4_links, Vine = 4),
						   mutate(vine5_links, Vine = 5),
						   mutate(vine6_links, Vine = 6),
						   mutate(vine7_links, Vine = 7),
						   mutate(vine8_links, Vine = 8),
						   mutate(vine9_links, Vine = 9))


all_fruit_data %<>%
	left_join(select(quadrant_info, quadrant, QuadrantFromLeader:NorthSouth), by = c("quadrant" = "quadrant")) %>%
	select(Vine:quadrant, QuadrantFromLeader:NorthSouth, FruitPos:FloweringColour, FloweringDate, SubTLeaf:Comments)


