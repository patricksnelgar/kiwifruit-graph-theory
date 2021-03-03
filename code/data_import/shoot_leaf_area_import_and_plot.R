library(ggplot2)

# This file handles importing the shoot leaf area dataset from shoots sampled from npon-experimental vines at KiwiMac



# make sure there is no existing data
leaf_area_data <- NA

# Data import
leaf_area_data <- 
	read_csv(here("input/shoot_leaf_area/shoot_leaf_area_data.csv"))


# Calculate shoot volume (cm^2)from formula for a cone, changing diameter from mm to cm
leaf_area_data %<>%
mutate(ShootVolume = (pi * ((ShootDiameter/20)^2)*ShootLength/3))

	
# Fit Power curve model to LeafArea vs Length data
FitPwrLength <- nls(LeafArea ~ a*ShootLength^b, data = leaf_area_data, start = list(a=1, b=1))
FitPwrLength


# Fit Power curve model to LeafArea vs conical Volume data
FitPwrVolume <- nls(LeafArea ~ c*ShootVolume^d, data = leaf_area_data, start = list(c=1, d=1))
FitPwrVolume
coef(FitPwrVolume)


# plot leaf area vs shoot length

ggplot(leaf_area_data) + geom_point(aes(ShootLength, LeafArea)) + geom_line(aes(ShootLength, predict(FitPwrLength)), colour = "green")


# plot leaf area vs shoot volume
ggplot(leaf_area_data) + geom_point(aes(ShootVolume, LeafArea)) + geom_line(aes(ShootVolume, predict(FitPwrVolume)), colour = "green")

# Save plots to output folder
#ggsave("Leaf Area vs shoot length power curve.png", path = here("output"),  dpi = 500)
