#### data import ####
vine1_data <- read_csv("input/architecture/kiwimac_data_vine1.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine1_fruit_data <- read_csv("input/fruit_data/fruit_data_vine1.csv") %>%
	mutate(shoot_id = paste(Vine, ShootID, sep = "-"))

#### data gathering ####

vine1_sources <- vine1_data %>%
	distinct(from) %>%
	rename(label = from)

vine1_targets <- vine1_data %>%
	distinct(to) %>%
	rename(label = to)

vine1_nodes <- full_join(vine1_sources, vine1_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine1_data, to, length, to_shoot_id, x, y, quadrant, diameter, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine1_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine1_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier) %>%
	select(label:to_shoot_id, target_type, origin_target_id, x_pos, y_pos, quadrant, diameter, to_origin_id) %>%
	filter(!is.na(label))

vine1_links <- vine1_data %>%
	select(from, to) %>%
	full_join(select(vine1_nodes, label, x_pos, y_pos), by = c("from" = "label")) %>%
	rename(xstart = x_pos, ystart = y_pos) %>%
	full_join(vine1_nodes, by = c("to" = "label")) %>%
	rename(xend = x_pos, yend = y_pos) 

vine1_fruit_data %<>%
	left_join(., select(vine1_links, to_shoot_id, xend, yend, quadrant), by = c("shoot_id" = "to_shoot_id")) 

vine1_fruit_data %<>%
	rename(ShootUUID = shoot_id) %>%
	select(Vine:FruitUUID, ShootUUID, xend:quadrant, FruitPos:Comments)

