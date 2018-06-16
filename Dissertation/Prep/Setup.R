##### Import the packages ######################################################
library(rugarch)
library(parallel)
library(snow)

##### Make a cluster to perform the analysis ###################################
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
