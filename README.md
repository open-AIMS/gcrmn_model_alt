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
```
