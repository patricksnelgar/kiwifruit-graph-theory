
all_fruit_data <- NA

# Script relies on the architechure data being present
if(length(all_arch_data) <= 1){
	stop("need arch data")
} else {
	
	for(vine_id in 1:9){
		temp_fruit_data <- 
			read_csv(here(paste0("input/fruit_data/fruit_data_vine", vine_id, ".csv"))) %>%
			mutate(ShootUUID = paste(vine_id, ShootID, sep = "-"))
		
		temp_arch <- all_arch_data %>%
			filter(VineUUID == vine_id)
		
		# Join with relevant architecture measures
		# Convert Fruit ID to numeric then apply same formatting as cane shoot IDs etc.
		# Lots of renaming... 
		temp_fruit_data %<>%
			left_join(select(temp_arch, ShootUUID, CaneUUID, ParentOriginID, Quadrant)) %>%
			mutate(FruitID = paste(vine_id, as.numeric(gsub("([1-9])([0-9]*)", "\\2", FruitID)), sep = "-")) %>%
			rename(VineUUID = Vine, TrayID = Tray, TrayPosition = TrayFruit,
				   FruitUUID = FruitID, FruitPositionUpShoot = FruitPos,
				   FloweringColour = Flowering, SubtendingLeafSize = `Sub-T Leaf`,
				   DistanceFromCanopyWire = Height, FreshWeight = FrtWt,
				   FreshWeightSlice = FWslice, DryWeightSlice = DWslice,
				   HueAngle1 = Hue1, HueAngle2 = Hue2, 
				   Firmness1 = Firm1, Firmness2 = Firm2,
				   SoluableSolidsContent = SSC1, DryMatter = DM, AverageHueAngle = avHue,
				   AverageFirmness = avFirm, FASTLabComments = Notes) %>%
			select(VineUUID, TrayID:FruitUUID, ShootUUID:Quadrant, FruitPositionUpShoot:DistanceFromCanopyWire,
				   FreshWeight, DryMatter, FreshWeightSlice, DryWeightSlice, HueAngle1, HueAngle2, AverageHueAngle,
				   Firmness1, Firmness2, AverageFirmness, SoluableSolidsContent, FASTLabComments, Comments) 
		
		if(length(all_fruit_data) <= 1)
			all_fruit_data <- temp_fruit_data
		else
			all_fruit_data %<>% bind_rows(temp_fruit_data)
	}
	
	rm(temp_fruit_data)
	rm(temp_arch)
	
	seed_counts <- read_csv(here("input/fruit_data/seed_counts.csv")) %>%
						rename(SampleID = 1,
							   TrayPosition = 2,
							   TrayID = 3,
							   FruitUUID = 4,
							   SubSampleSeedWeight = 5,
							   TotalSeedWeight = 6,
							   AverageSeedCount = 7,
							   SeedComments = 8) %>%
						filter(!is.na(FruitUUID)) %>%
						mutate(VineID = gsub("([1-9])(.*)", "\\1", FruitUUID),
							   FruitUUID = paste(VineID, as.numeric(gsub("([1-9])([0-9]*)", "\\2", FruitUUID)), sep = "-"))
	
	all_fruit_data %<>%
		left_join(flowering_dates) %>%
		left_join(select(seed_counts, FruitUUID:SeedComments), by = "FruitUUID") %>%
		mutate(DryWeight = DryMatter / (100 * FreshWeight)) %>%
		select(VineUUID:FloweringColour, FloweringDate, SubtendingLeafSize:DryMatter, DryWeight,
			   FreshWeightSlice:SoluableSolidsContent, 
			   SubSampleSeedWeight:SeedComments, FASTLabComments, Comments)
	
}