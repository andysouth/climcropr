
#Nicoles method for plotting raster maps with ggplot

printMap <- function(emaps){
  plotMap <- emaps
  names(plotMap) <- "layer"
  x <- init(plotMap, "x")
  names(x) <- "x"
  y <- init(plotMap, "y")
  names(y) <- "y"
  plotMap <- stack(x, y, plotMap)
  # setDT is a data.table function
  plotMap <- setDT(data.frame(getValues(plotMap)))
  plotMap <- plotMap[!is.nan(layer) & !is.na(layer), ]

  plotMap[ , lc := cut(layer, c(-Inf, -75, -50, -40, -35, -30, -25, -20, -15, -10, -5, -2, 0, 2, 5, 10, 15, 20, 25, 30, 35, 40, 50, 75, Inf), labels = c(-100, -75, -50, -40, -35, -30, -25, -20,-15, -10, -5, -2, 2, 5, 10, 15, 20, 25, 30, 35, 40, 50, 75,100))]

  gpal <- colorRampPalette(brewer.pal(11, "BrBG"))

  g1 <- ggplot(plotMap, aes(x = x, y = y)) +
    geom_raster(aes(fill = lc)) +
    theme_bw() +
    coord_equal() +
    borders("world", size = 0.1, xlim = c(-180, 180)) +
    xlab("Longitude") +
    ylab("Latitude") +
    scale_fill_manual(values = gpal(24), drop = FALSE, guide = guide_legend(reverse = FALSE, nrow = 1, label.position = "bottom", byrow = TRUE), name = "") +
    theme(legend.direction = "horizontal", legend.position = "bottom", panel.grid = element_blank())
  return(g1)

}
