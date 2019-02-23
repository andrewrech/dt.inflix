
options(mc.cores = 8)
data.table::setDTthreads(8)

test_data_dir <- function(){

  dir <- system.file(package = "dt.inflix") %>% file.path(., "extdata/testdata")

  return(dir)

}
