##### Import the packages ######################################################
# install.packages('rugarch')
# install.packages('parallel')
# install.packages('snow')

library(rugarch)
library(parallel)
library(snow)

##### Make a cluster to perform the analysis ###################################
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
