library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

#simple 96 well plate layout
gs <- lapply(1:96, function(ii)
  grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
grid.arrange(grobs=gs, ncol=12,
             top="top label", bottom="bottom\nlabel",
             left="left label", right="right label")
grid.rect(gp=gpar(fill=NA))

gs <- lapply(1:384, function(ii)
  grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
grid.arrange(grobs=gs, ncol=12,
             top=c(LETTERS[1:8]),
             left=c(1:12))
grid.rect(gp=gpar(fill=NA))
