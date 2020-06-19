
# Dry matter exclusions
all_fruit_data %<>%
	mutate(DryMatter = ifelse(FruitUUID == "3-480" | 
							  FruitUUID == "9-188", NA, DryMatter))

all_fruit_data %>%
	filter(FruitUUID == "3-480" | FruitUUID == "9-188") %$%
	is.na(DryMatter)


# Complete fruit exclusions
# only for fruit that has data but is not trusted
all_fruit_data %<>%
	filter(!FruitUUID %in% c("4-476", "8-808"))

all_fruit_data %>%
	filter(FruitUUID %in% c("4-476", "8-808")) %$%
	length(DryMatter) == 0



# Various metrics from different fruit
all_fruit_data %<>%
	filter() %>%
	mutate(HueAngle1 = ifelse(FruitUUID == "6-1064", NA, HueAngle1),
		   HueAngle2 = ifelse(FruitUUID == "6-1064", NA, HueAngle2),
		   AverageHueAngle =ifelse(FruitUUID == "6-1064", NA, AverageHueAngle))

all_fruit_data %>%
	filter(FruitUUID == "6-1064") %$%
	all(is.na(HueAngle1), is.na(HueAngle2), is.na(AverageHueAngle))
