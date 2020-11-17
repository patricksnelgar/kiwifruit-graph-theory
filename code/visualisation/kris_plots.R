# Data exploration and plotting

  
  column_labels <- c("Conventional", "Strung", "Spur")
  names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")
  
 
  vine_order <- data.frame(vine_label = paste("Vine", c(1:4, 6, 5, 9, 8, 7)), 
                           VineRow = c(rep(1:3, each = 3)),
                           VineTreatment = c("1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur"))
 
all_fruit_data %>%
  filter(!is.na(VineRow), !is.na(VineTreatment)) %>%
  
   ggplot(aes(x=FreshWeight, y=DryMatter)) +
    geom_point(aes(col=ShootTypeCoarse), size=3) +
    facet_grid(vars(VineRow), vars(VineTreatment))
    
    # ggtitle("Dry Matter by Vine & treatment")
  
  
  
  
  