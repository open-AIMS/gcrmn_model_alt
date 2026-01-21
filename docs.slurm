#!/bin/bash
#SBATCH --job-name=gcrmn
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=3
#SBATCH --partition=mlogan
#SBATCH --mem=1GB
#SBATCH --output=docs_%j.log
#SBATCH --mail-user=m.logan@aims.gov.au
#SBATCH --mail-type=ALL

module load singularity
# singularity exec -B .:/home/Project r-analysis2.sif make render_docs
singularity exec -B .:/home/Project gcrmn_alt2.sif make render_docs
