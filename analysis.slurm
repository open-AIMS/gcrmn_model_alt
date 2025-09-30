#!/bin/bash
#SBATCH --job-name=gcrmn
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=45
#SBATCH --partition=mlogan
#SBATCH --mem=200GB
#SBATCH --output=analysis_%j.log
#SBATCH --mail-user=m.logan@aims.gov.au
#SBATCH --mail-type=ALL

module load singularity
singularity exec -B .:/home/Project r-analysis2.sif make run_R
