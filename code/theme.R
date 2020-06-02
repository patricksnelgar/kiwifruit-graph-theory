theme_patrick <- function() {
	theme_bw() %+replace%
		theme(plot.title = element_text(hjust = 0.5, 
										size = 20,
										margin = margin(b = 5))
		)
}
