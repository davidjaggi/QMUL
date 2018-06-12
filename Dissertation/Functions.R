printer <- function(plot, path){
  path = path
  png(filename=path, height = 960, width = 1440, res = 300)
  plot
  dev.off()
}
