# Import and convert NAB data to R data_frames for use in ROnan
#
# Version 0.1 - Gary Mulder - 04/11/2016

library(tidyverse)

# We assume that the The Numenta Anomaly Benchmark (https://github.com/numenta/NAB)
# has been checked out into the same parent directory as ROnan
setwd("../NAB/")

