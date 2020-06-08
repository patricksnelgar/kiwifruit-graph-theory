#### data import ####
vine2_data <- read_csv("input/architecture/kiwimac_data_vine2.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine2_fruit_data <- read_csv("input/fruit_data/fruit_data_vine2.csv") %>%
	mutate(shoot_id = paste(Vine, ShootID, sep = "-"))

#### data gathering ####

vine2_sources <- vine2_data %>%
	distinct(from) %>%
	rename(label = from)

vine2_targets <- vine2_data %>%
	distinct(to) %>%
	rename(label = to)

vine2_nodes <- full_join(vine2_sources, vine2_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine2_data, to, length, to_shoot_id, x, y, quadrant, diameter, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine2_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine2_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier) %>%
	select(label:to_shoot_id, cane_id, target_type, origin_target_id, x_pos, y_pos, quadrant, diameter, to_origin_id) %>%
	filter(!is.na(label))

vine2_links <- vine2_data %>%
	select(from, to) %>%
	full_join(select(vine2_nodes, label, x_pos, y_pos), by = c("from" = "label")) %>%
	rename(xstart = x_pos, ystart = y_pos) %>%
	full_join(vine2_nodes, by = c("to" = "label")) %>%
	rename(xend = x_pos, yend = y_pos) 

vine2_fruit_data %<>%
	left_join(., select(vine2_links, to_shoot_id, cane_id, xend, yend, quadrant), by = c("shoot_id" = "to_shoot_id")) 

vine2_fruit_data %<>%
	rename(ShootUUID = shoot_id, CaneID = cane_id) %>%
	select(Vine:FruitUUID, ShootUUID, CaneID, xend:quadrant, FruitPos:Comments)
