#!/usr/bin/env sh

# install dt.inflix R package and dependencies
# https://github.com/andrewrech/dt.inflix

# check script tools

  if [ ! -x `which Rscript` ]; then
  echo "No suitable Rscript program found."
  exit 1
  fi

# install dependencies

  echo "Installing dependencies..."

  Rscript -e \
  'install.packages("devtools", repos = "http://cran.us.r-project.org"); devtools::install_github("hadley/devtools"); install.packages("testthat", repos = "http://cran.us.r-project.org")'

  Rscript -e \
  'install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")'

  Rscript -e \
  'devtools::install_github(c("tidyverse/magrittr", "andrewrech/dt.inflix"))'

# install dt.inflix

  echo "Installing dt.inflix..."

  Rscript -e \
  'devtools::install_github("andrewrech/dt.inflix")'

  echo "Testing dt.inflix..."
    Rscript -e \
  'testthat::test_package("dt.inflix")'