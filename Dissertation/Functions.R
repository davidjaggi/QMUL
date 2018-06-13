##### Printer function to save pictures as png #################################
# This file contains useful functions which help to write the dissertation
printer <- function(plot, name){
  # name is the filename
  # plot is the plot which will be saved
  # path is the path under which the figure will be saved
  path = paste0('Dissertation/Figures/',name,'.png')
  png(filename=path, height = 960, width = 1440, res = 300)
    plot(q)
  dev.off()
}

##### Sinker function to save output to txt ####################################
sinker <- function(output, name){
  # name is the name of the file
  # output is the stuff which should be saved
  # path is the path to which the file will be saved
  path = paste0('Dissertation/Text/',name,'.txt')
  sink(path)
  print(output)
  sink()
}
