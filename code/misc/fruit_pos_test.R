# maximum 9 fruit per shoot
angle <- 2 * pi / 9

pointXOffset <- function (central_xpoint, radius, point_pos){
	return(central_xpoint + radius * cos(point_pos * angle))
}

pointYOffset <- function (central_ypoint, radius, point_pos){
	return(central_ypoint + radius * sin(point_pos * angle))
}


test_frame <- data.frame(x = c(rep(0, 9), rep(3, 3)), y = c(rep(0,9), rep(-3, 3)), fruit_pos = c(1:9, 1:3))


ggplot(test_frame) +
	geom_point(aes(x, y), data = data.frame(x = c(0,3), y = c(0,-3)), colour = "black") +
	geom_point(aes(x = pointXOffset(x, 1, fruit_pos), y = pointYOffset(y, 1, fruit_pos), colour = factor(fruit_pos)), size = 3)
