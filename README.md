GCMRN model comparisons
=======================================================

This respository comprises the codebase for fitting a bespoke Bayesian
Hierarchical model to GCMRN data and comparing the resulting temporal
trends at a range of spatial scales (ecoregion, subregion and region)
to those produced by the xgboost models developed by Jeremy Wicquart.

The codebase is designed to run in a containerised environment (either
Docker or Apptainer/Singularity if being run on HPC architecture).
Also note, this repository does not include the necessary input data,
without which, the codebase will not run.

## Prerequisits for running the code

1. git
2. access to the data
3. at least 300Gb free space
4. at least 40Gb RAM
5. make (build tools)

## Installing the codebase

To install the codebase, clone the current repository to a suitable
location on a machine that has docker or apptainer/singularity

For example:

```
git clone https://github.com/open-AIMS/gcrmn_model_alt.git .
```

## Including data

The cloned repo will already have some of the necessary directory
structure in place.  To complete all the data requirements, ensure
that the directory tree initially looks like the following:

```
|-- data
|   |-- primary
|   |   |-- data_xgboost.csv
|   |   |-- data_benthic_prepared_murray.RData
|   |   |-- data_predictors_pred_murray.RData
|   |   |-- meow.RData
|   |   |-- ecoregion.lookup.RData
|   |   |-- GIS
|   |       |-- reef_grid.shx
|   |       |-- reef_grid.shp
|   |       |-- reef_grid.prj
|   |       |-- reef_grid.fix
|   |       |-- reef_grid.dbf
|   |       |-- reef_grid.cpg
|   |       |-- world_sf.RData
```

Note, `meow.RData` and `ecoregion.lookup.RData` are already in the
repository.

## Building the environment

Most of the dependencies can be inferred by examination of the
`Dockerfile`. In fact, the safest way of ensuring that the codes will
run is the build a docker image from the `Dockerfile` and run within a
container.

Nevertheless, if you are running on bare metal, then ensure that both
`R` and `python` are installed and that their respective packages
indicated in the Dockerfile are installed and available.

## Docker

A docker container can be built via the following:

```
make build_docker
```

## Running the codes


### Via bare metal

1. run the R codes.  This will run all the R based analyses using the
   `targets` package to ensure all steps are performed in the correct
   order.

```
make run_R
```

2. render the document. This will render the quarto document to html

```
make render_docs
```

### Via docker

1. run the R codes.  This will run all the R based analyses using the
   `targets` package to ensure all steps are performed in the correct
   order.

```
make R_container
```

2. render the document. This will render the quarto document to html

```
make docs_container
```

### Via apptainer/singularity


## Debugging the code

### Via docker
In order to edit the code (for the purpose of debugging or adding
additional features):

1. load the `R/_targets.R` script
2. start a new terminal (**in the project root folder**)
3. run

```
docker run --rm -it -v $PWD:/home/Project gcrmn_alt
```

4. set the working directory to the `R` directory

```
setwd("R")
```

5. load the necessary targets libraries

```
library(targets)
library(tarchetypes)
```

6. load the other necessary libraries

```
packages = c("tidyverse", "sf", "synthos",
  "glmmTMB", "emmeans", "DHARMa", "patchwork",
  "brms", "rstan", "bayesplot", "tidybayes",
  "caret", "xgboost", "tidymodels",
  "rnaturalearth", "rlang",
  "posterior", "gbm", "dbarts", "HDInterval"
)
lapply(packages, library, character.only = TRUE)
```

7. navigate the code as usual


### Via apptainer/singularity

Follow the steps for docker except replace step 3 with:

```
singularity exec -B .:/home/Project r-analysis2.sif R
```
