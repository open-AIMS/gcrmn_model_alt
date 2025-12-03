GCMRN model comparisons
==========================

![status](https://img.shields.io/badge/status-in%20dev.-blue)
![version](https://img.shields.io/badge/version-0.1.0-blue)

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

## Repository structure

```
|-- data
|   |-- primary
|   |   |-- meow.RData
|   |   |-- ecoregion.lookup.RData
|-- docs
|   |-- resources
|   |   |-- <various resouces for document preparation>
|   |-- australia.qmd
|   |-- brazil.qmd
|   |-- caribbean.qmd
|   |-- compare_models.qmd
|   |-- eas.qmd
|   |-- etp.qmd
|   |-- pacific.qmd
|   |-- persga.qmd
|   |-- ropme.qmd
|   |-- south_asia.qmd
|   |-- wio.qmd
|-- R
|   |-- _targets.R
|   |-- helper_function.R
|   |-- process_spatial.R
|   |-- process_benthic_data.R
|   |-- fit_models.R
|   |-- aggregate_models.R
|-- stan
|   |--gcrmn_model_43.stan
|-- .gitignore
|-- Dockerfile
|-- Makefile
|-- README.md
|-- analysis.slurm
|-- dashboard.sh
|-- docs.slurm
```

- the `docs` directory comprises of the quarto documents and resources
  required for compiling self-contained HTML results documents for
  each GCRMN regions as well as an overal statistical methods and
  comparison document (compare_models.qmd).

- the R scripts comprise a R targets pipeline in which the collated
  data (not supplied in this repo) are processed, Bayesian
  Hierarchical models are fit seperately for each ecoregion and
  benthic category and the posteriors are aggregated up from ecoregion
  level to subregion, then region and finally, whole globe scale.

- the stan model is provided in the stan directory

- the root of the repository also contains a Dockerfile to assist with
  reproducibility over time as well as a Makefile and slurm files to
  assist with running the analyses in various locations.

## Input data

The cloned repo will already have some of the necessary directory
structure in place.  However, to complete all the data requirements,
ensure that the directory tree initially looks like the following:

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
repository. All other data, must be separately obtained - they cannot
be shared in this repository due to licencing or data sharing
agreements.


## Building the environment

Most of the dependencies can be inferred by examination of the
`Dockerfile`. In fact, the safest way of ensuring that the codes will
run is the build a docker image from the `Dockerfile` and run within a
container.

Nevertheless, if you are running on bare metal, then ensure that both
`R` and `python` are installed and that their respective packages
indicated in the Dockerfile are installed and available.

## Docker

A docker image can be built via the following:

```
make build_docker
```

## Apptainer/Singularity

An Apptainer/Singularity image (for HPC) can be built from a Docker
image via the following:

```
make build_singularity
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

### Via Apptainer/Singularity

1. submit a job to slurm that runs the R codes.  This will run all the
   R based analyses using the `targets` package to ensure all steps
   are performed in the correct order.

```
make slurm_R
```

2. submit a job to slurm that renders the document. This will render
   the quarto document to html

```
make slurm_docs
```

## Outputs

When running the R analysis pipeline outputs will be stored in the
following locations (according to artifact type):

- `data/`
  - `mod_*`: stan models
  - `posteriors_*`: extracted posteriors of the stan models
  - `cellmeans_*: summarised posteriors

- `output/figures/`
  - ...: modelled time series representations

- `output/tables/`
  - ...: tabular versions of the outputs


## Debugging the code

### Via docker

In order to run the code interactively (for the purpose of debugging
or adding additional features):

1. start a new terminal (**in the project root folder**)
2. run

```
docker run --rm -it -v $PWD:/home/Project gcrmn_alt
```

This will mount the current working directory to `/home/Project`
within the container and the container is set to automatically work
from this location.  Hence, all codes and outputs will be exchanged
via this mount point.

3. set the working directory to the `R` directory

```
setwd("R")
```

4. load the `R/_targets.R` script in an editor (ideally on the host)
5. establish a connection between the host and the R REPL (comint)
6. load the necessary targets libraries

```
library(targets)
library(tarchetypes)
```

7. load the other necessary libraries

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

8. navigate the code as usual


### Via apptainer/singularity

Follow the steps for docker except replace step 2 with:

```
singularity exec -B .:/home/Project r-analysis2.sif R
```

