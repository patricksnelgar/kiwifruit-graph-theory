# This file imports and combines all the vine architecture information.
# combines quadrant info and calculates x&y co-ords for beginning and end of each segment for ggplot mapping

all_arch_data <- NA

for(vine_id in 1:9){
	temp_arch <- read_csv(here(paste0("input/architecture/kiwimac_data_vine", vine_id, ".csv"))) %>%
					mutate(ShootUUID = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-S"), NA),
						   CaneUUID = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
						   OriginUUID = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-O"), NA),
						   ParentOriginID = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-O"),NA), 
						   LeaderNS = if_else(row_number()<which(to_origin_id == "trunk"), "S", 
						   				   if_else(row_number()>which(to_origin_id == "trunk"), "N", NA_character_))) %>%
						   
					rename(VineUUID = 1, ParentNodeID = 2, NodeID = 3, SegmentLength = 4, 
						   DistanceFromQuadrantX = 8, DistanceFromQuadrantY = 9,
						   Quadrant = 10, SegmentDiameter = 11, SegmentOrientation = 13, Comments = 15) %>%
					select(-to_shoot_id, -spurs, -cane_id, -to_origin_id, -base_origin_id)
	
	temp_arch %<>% 
		left_join(quadrant_info, by = "Quadrant") %>%
		mutate(DistanceFromTrunk = (DistanceFromQuadrantX + OffsetX) * MultiplierX,
			   DistanceFromLeader = (DistanceFromQuadrantY + OffsetY) * MultiplierY)
	
#Assigning treatment column
	temp_arch %<>% 
	mutate(VineTreatmentNoNumber = ifelse(vine_id %in% c(1, 4, 9), "Conventional", 
								   ifelse(vine_id %in% c(2, 6, 8), "Strung",
								   	   ifelse(vine_id %in% c(3, 5, 7), "Spur", NA))))	
		
	# Pulling out wood type categories from the CaneUUID column
	
	temp_arch %<>%
		mutate(WoodType =  
			   	if_else(grepl("C", CaneUUID, fixed=TRUE), "Cane", 
					if_else(grepl("H", CaneUUID, fixed=TRUE), "Short cane",
						if_else(grepl("T", CaneUUID, fixed=TRUE), "Stub", 
							if_else(grepl("P", CaneUUID, fixed=TRUE), "Spur",
								if_else(grepl("A", CaneUUID, fixed=TRUE), "Adventitious", NA_character_))))))
		
	# Map start & end segment coords for ggplots gem_segment
	# as ggplot is much faster for mapping the architecture than letting ggraph handle it.
	temp_arch %<>%
		select(ParentNodeID, NodeID) %>%
		left_join(select(temp_arch, NodeID, DistanceFromTrunk, DistanceFromLeader), 
			  by = c("ParentNodeID" = "NodeID")) %>%
		rename(SegmentStartX = DistanceFromTrunk, SegmentStartY = DistanceFromLeader) %>%
		left_join(select(temp_arch, -ParentNodeID), by = "NodeID") %>%
		rename(SegmentEndX = DistanceFromTrunk, SegmentEndY = DistanceFromLeader) %>%
		select(VineUUID, VineTreatmentNoNumber,ParentNodeID, NodeID, ShootUUID:OriginUUID, ParentOriginID, WoodType, SegmentStartX, SegmentStartY,SegmentEndX, SegmentEndY, SegmentLength:SegmentOrientation,
			   Quadrant:SegmentOrientation, QuadrantFromLeader:EastWest, LeaderNS, Comments)
	
	
	# merge the current vine with full dataset.
	if(length(all_arch_data) <= 1)
		all_arch_data <- temp_arch
	else
		all_arch_data %<>% bind_rows(temp_arch)
					
}

rm(temp_arch)

