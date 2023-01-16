
# tear down tests

  lapply(
         c(
      "teardown_file_placeholder.txt"
          ), function(i){

        if (file.exists(i))
          file.remove(i)
    })

    list.dirs(path = ".") %>% stringr::str_extract("ag_[a-f0-9]{18}") %>% na.omit %>% lapply(., function(dir){
             unlink(dir, recursive = TRUE, force = TRUE)
            })
