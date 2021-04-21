library(here)

source(here("code/data_import/import_all_data.R"))

all_fruit_data %<>%
	left_join(select(all_arch_data, VineUUID, Trea))

vine1 <- 
	filter(all_fruit_data, VineUUID == 1)

v1_aov <- aov(FreshWeight ~ ShootTypeRefined * Quadrant, data = vine1)
summary(v1_aov)


v <- aov(FreshWeight ~ ShootTypeRefined + Quadrant + LeaderNS + VineTreatmentNoNumber, data = all_fruit_data)
summary(v)


# play around with bootstrapping samples sizes and comparing 
# between treatments?

