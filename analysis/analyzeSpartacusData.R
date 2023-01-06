library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(ncdf4)

# params ------------------------------------------------------------------
input_dir = here("data_raw/spartacus/")


# files -------------------------------------------------------------------
files = dir(input_dir, ".*oe.*\\.nc", full.names = T)

# data --------------------------------------------------------------------
f20002022 = files[1:length(files)-1]


ncs = map(f20002022, read_ncdf)


ncs2 = do.call("c", ncs)
me = st_apply(ncs2, c("x", "y"), mean)



# difference from 2023 ----------------------------------------------------
f2023 = files[length(files)] %>% read_ncdf()


diffs = as.numeric(f2023[[1]]) - me[[1]]


diffs_star = f2023
diffs_star[[1]] = diffs



# write out differenzen ---------------------------------------------------
write_stars(diffs_star, here("output/oe_diff_20002022_2023_tmax.geotiff"))





