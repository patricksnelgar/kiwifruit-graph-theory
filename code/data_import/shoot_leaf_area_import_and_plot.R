# This file handles importing the shoot leaf area dataset from shoots sampled from non-experimental vines at KiwiMac
# Author: Kris Kramer-Walter

library(ggplot2)
library(nlstools)

# make sure there is no existing data
leaf_area_data <- NA

# Data import
leaf_area_data <- 
	read_csv(here("input/shoot_leaf_area/shoot_leaf_area_data.csv"))



# Calculated traits

# Calculate shoot volume (cm^2)from formula for a cone, changing diameter from mm to cm
leaf_area_data %<>%
	mutate(ShootVolume = (pi * ((ShootDiameter/20)^2)*ShootLength/3))

# Calculate Internode lengths
leaf_area_data %<>%
	mutate(InternodeLength = ShootLength/LeafNumber)






# Model fitting
# S = SQRT(MSE), MSE = Residual sum of squares/DFerror


# Fit linear regression model to LeafNumber vs ShootLength  data, S = 19.55 
FitLMLeafNo <- lm(ShootLength ~ LeafNumber, data = leaf_area_data)
FitLMLeafNo

summary(FitLMLeafNo)

# Residual plotting

par(mfrow = c(2, 2)) 
plot(FitLMLeafNo)
par(mfrow = c(1, 1))

# Fit 1 parameter Power curve model to LeafNumber vs ShootLength  data, S = 21.6
FitPwr1LeafNo <- nls(ShootLength ~ LeafNumber^x, data = leaf_area_data, start = list(x=1))
FitPwr1LeafNo

summary(FitPwr1LeafNo)

# Residual plotting
RedFitPwr1LeafNo <-nlsResiduals(FitPwr1LeafNo)
plot(RedFitPwr1LeafNo, which = 0)
plot(RedFitPwr1LeafNo, which = 1)
plot(RedFitPwr1LeafNo, which = 2)
plot(RedFitPwr1LeafNo, which = 3)
plot(RedFitPwr1LeafNo, which = 4)
plot(RedFitPwr1LeafNo, which = 5)
plot(RedFitPwr1LeafNo, which = 6)


# Fit 2 parameter Power curve model to LeafNumber vs ShootLength  data, S =
FitPwr2LeafNo <- nls(ShootLength ~ e*LeafNumber^f, data = leaf_area_data, start = list(e=1, f=1))
FitPwr2LeafNo

summary(FitPwr2LeafNo)

# Residual plotting
RedFitPwr2LeafNo <-nlsResiduals(FitPwr2LeafNo)
plot(RedFitPwr2LeafNo, which = 0)
plot(RedFitPwr2LeafNo, which = 1)
plot(RedFitPwr2LeafNo, which = 2)
plot(RedFitPwr2LeafNo, which = 3)
plot(RedFitPwr2LeafNo, which = 4)
plot(RedFitPwr2LeafNo, which = 5)
plot(RedFitPwr2LeafNo, which = 6)


# Fit 2 parameter Power curve model to LeafNumber vs ShootLength  data
FitLin2LeafNo <- nls(ShootLength ~ e*LeafNumber + f*LeafNumber^2, data = leaf_area_data, start = list(e=1, f=1))
FitLin2LeafNo

summary(FitLin2LeafNo)

# Residual plotting
RedFitLin2LeafNo <-nlsResiduals(FitLin2LeafNo)
plot(RedFitLin2LeafNo, which = 0)
plot(RedFitLin2LeafNo, which = 1)
plot(RedFitLin2LeafNo, which = 2)
plot(RedFitLin2LeafNo, which = 3)
plot(RedFitLin2LeafNo, which = 4)
plot(RedFitLin2LeafNo, which = 5)
plot(RedFitLin2LeafNo, which = 6)





# Fit Power curve model to LeafArea vs Length data
FitPwrLength <- nls(LeafArea ~ a*ShootLength^b, data = leaf_area_data, start = list(a=1, b=1))
FitPwrLength

RedFitPwrLength <-nlsResiduals(FitPwrLength)
plot(RedFitPwrLength, which = 0)
plot(RedFitPwrLength, which = 1)
plot(RedFitPwrLength, which = 2)
plot(RedFitPwrLength, which = 3)
plot(RedFitPwrLength, which = 4)
plot(RedFitPwrLength, which = 5)
plot(RedFitPwrLength, which = 6)

# Fit Power curve model to LeafArea vs conical Volume data
FitPwrVolume <- nls(LeafArea ~ c*ShootVolume^d, data = leaf_area_data, start = list(c=1, d=1))
FitPwrVolume
coef(FitPwrVolume)



FitLin2LeafNo

# Fitted plots

# plot ShootLength vs LeafNumber

ggplot(leaf_area_data, aes(LeafNumber, ShootLength)) + 
	geom_point(aes(LeafNumber, ShootLength)) + 
	geom_line(aes(LeafNumber, predict(FitPwr1LeafNo)), colour = "blue") +
	geom_line(aes(LeafNumber, predict(FitPwr2LeafNo)), colour = "green") + 
	geom_line(aes(LeafNumber, predict(FitLin2LeafNo)), colour = "red")  
	


	summary(lm(LeafNumber~ShootLength, leaf_area_data))



# plot ShootLength vs InternodeLength by ShootType

ShootLengthVsInterNode <-
ggplot(leaf_area_data, aes(ShootLength, InternodeLength)) + 
	geom_point(aes(col= ShootType)) 
ggplotly(ShootLengthVsInterNode)


# plot ShootDiameter vs InternodeLength by ShootType

ShootDiameterVsInterNode <-
	ggplot(leaf_area_data, aes(ShootDiameter, InternodeLength)) + 
	geom_point(aes(col= ShootType)) 
ggplotly(ShootDiameterVsInterNode)


# plot ShootLength vs LeafArea by ShootType

ShootLengthVsLeafArea <-
	ggplot(leaf_area_data, aes(ShootLength, LeafArea)) + 
	geom_point(aes(col= ShootType)) 
ggplotly(ShootLengthVsLeafArea)


# plot ShootDiameter vs InternodeLength by ShootType

ShootDiameterVsLeafArea <-
	ggplot(leaf_area_data, aes(ShootDiameter, LeafArea)) + 
	geom_point(aes(col= ShootType)) 
ggplotly(ShootDiameterVsLeafArea)

	
# plot leaf area vs shoot length

LeafAreaVsLength <-
ggplot(leaf_area_data) + 
	geom_point(aes(ShootLength, LeafArea, col = ShootType)) + 
	geom_line(aes(ShootLength, predict(FitPwrLength)), colour = "green")
LeafAreaVsLength

# plot leaf area vs shoot volume
LeafAreaVsVolume <-
ggplot(leaf_area_data) + 
	geom_point(aes(ShootVolume, LeafArea, col=ShootType)) + 
	geom_line(aes(ShootVolume, predict(FitPwrVolume)), colour = "green")
LeafAreaVsVolume


# Save plots to output folder
#ggsave("Leaf Area vs shoot length power curve.png", path = here("output"),  dpi = 500)
