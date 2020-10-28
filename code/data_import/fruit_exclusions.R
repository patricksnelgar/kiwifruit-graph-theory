# Excludes some or all metrics for specific fruit

# Dry matter exclusions
all_fruit_data %<>%
	mutate(DryMatter = if_else(FruitUUID == "3-F480" | 
							  FruitUUID == "9-F188", NA, DryMatter))

all_fruit_data %>%
	filter(FruitUUID == "3-F480" | FruitUUID == "9-F188") %$%
	is.na(DryMatter)


# Complete fruit exclusions
# only for fruit that has data but is not trusted
all_fruit_data %<>%
	filter(!FruitUUID %in% c("4-F476", "8-F808"))

all_fruit_data %>%
	filter(FruitUUID %in% c("4-F476", "8-F808")) %$%
	length(DryMatter) == 0



# Various metrics from different fruit
all_fruit_data %<>%
	filter() %>%
	mutate(HueAngle1 = if_else(FruitUUID == "6-F1064", NA, HueAngle1),
		   HueAngle2 = if_else(FruitUUID == "6-F1064", NA, HueAngle2),
		   AverageHueAngle =if_else(FruitUUID == "6-F1064", NA, AverageHueAngle))

all_fruit_data %>%
	filter(FruitUUID == "6-F1064") %$%
	all(is.na(HueAngle1), is.na(HueAngle2), is.na(AverageHueAngle))
