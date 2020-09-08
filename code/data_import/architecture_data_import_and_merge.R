# This file imports and combines all the vine architecture information.
# combines quadrant info and calculates x&y co-ords for beginning and end of each segment for ggplot mapping

all_arch_data <- NA

for(vine_id in 1:9){
	temp_arch <- read_csv(here(paste0("input/architecture/kiwimac_data_vine", vine_id, ".csv"))) %>%
					mutate(ShootUUID = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-S"), NA),
						   CaneUUID = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
						   OriginUUID = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-O"), NA),
						   ParentOriginID = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-O"), NA)) %>%
					rename(VineUUID = 1, ParentNodeID = 2, NodeID = 3, SegmentLength = 4, 
						   DistanceFromQuadrantX = 8, DistanceFromQuadrantY = 9,
						   Quadrant = 10, SegmentDiameter = 11, SegmentOrientation = 13, Comments = 15) %>%
					select(-to_shoot_id, -spurs, -cane_id, -to_origin_id, -base_origin_id)
	
	temp_arch %<>% 
		left_join(quadrant_info, by = "Quadrant") %>%
		mutate(DistanceFromTrunk = (DistanceFromQuadrantX + OffsetX) * MultiplierX,
			   DistanceFromLeader = (DistanceFromQuadrantY + OffsetY) * MultiplierY)
		
	
	# Map start & end segment coords for ggplots gem_segment
	# as ggplot is much faster for mapping the architecture than letting ggraph handle it.
	temp_arch %<>%
		select(ParentNodeID, NodeID) %>%
		left_join(select(temp_arch, NodeID, DistanceFromTrunk, DistanceFromLeader), 
			  by = c("ParentNodeID" = "NodeID")) %>%
		rename(SegmentStartX = DistanceFromTrunk, SegmentStartY = DistanceFromLeader) %>%
		left_join(select(temp_arch, -ParentNodeID), by = "NodeID") %>%
		rename(SegmentEndX = DistanceFromTrunk, SegmentEndY = DistanceFromLeader) %>%
		select(VineUUID, ParentNodeID, NodeID, ShootUUID:OriginUUID, ParentOriginID, SegmentStartX, SegmentStartY,
			   SegmentEndX, SegmentEndY, SegmentLength:SegmentOrientation,
			   Quadrant:Comments)
	
	
	# merge the current vine with full dataset.
	if(length(all_arch_data) <= 1)
		all_arch_data <- temp_arch
	else
		all_arch_data %<>% bind_rows(temp_arch)
					
}

rm(temp_arch)
