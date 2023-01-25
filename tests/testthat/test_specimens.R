# Get specimens by datasetid
get_specimens(datasetid=c(19832,41610))

# Get specimens by specimenID
get_specimens(c(7,8))

# Get specimens from sites object

my_sites <- get_sites(c(13296, 5663, 24))
my_specimens <- get_specimens(my_sites)