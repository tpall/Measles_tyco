# @knitr fig

library(ggplot2)
library(gridExtra)

labels <- lapply(LETTERS[1:5], function(label) textGrob(label, x = unit(0,"npc"), 
                                                        hjust = 0, gp = gpar(fontsize = 8, fontface = 2)))
p <- qplot(data=cars,x=speed,y=dist)
t <- tableGrob(head(aggregate(dist~speed, data=cars, mean)))
fig <- arrangeGrob(arrangeGrob(arrangeGrob(labels[[1]], p, heights = c(1,12)), 
                                arrangeGrob(labels[[2]], p, heights = c(1,12)),
                                ncol = 2, widths = c(6.5,3.5)), 
                    arrangeGrob(arrangeGrob(labels[[3]], p, heights = c(1,12)),
                                arrangeGrob(labels[[4]], t, heights = c(1,12)),
                                arrangeGrob(labels[[5]], p, heights = c(1,12)),
                                ncol = 3, widths=c(1,1,1)), 
                    heights = c(1,1))
fig
