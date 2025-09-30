## for interactive development within the docker container
## cd ~/Projects/gcrmn_model_alt
## docker run --rm -it -v $PWD:/home/Project gcrmn_alt
## OR
## singularity exec -B .:/home/Project r-analysis2.sif make run_R 
## setwd("R")

# Load the targets package
library(targets)
library(tarchetypes)

# Set global options
tar_option_set(
  packages = c("tidyverse", "sf", "synthos",
    "glmmTMB", "emmeans", "DHARMa", "patchwork",
    "brms", "rstan", "bayesplot", "tidybayes",
    "caret", "xgboost", "tidymodels",
    "rnaturalearth", "rlang",
    "posterior", "gbm", "dbarts", "HDInterval"),  # Load required packages
  format = "rds"                    # Default storage format
)
## lapply(packages, library, character.only = TRUE)

source("helper_functions.R")    # Load the modelling of incomplete spatial script
source("process_spatial.R")
source("process_benthic_data.R")
source("fit_models.R")
source("aggregate_models.R")

list(
  helper_functions(),
  process_spatial(),
  process_benthic_data(),
  fit_models(),
  aggregate_models()
)
