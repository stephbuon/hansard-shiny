#!/bin/bash
#SBATCH -J tokenize_hansard
#SBATCH -o tokenize_hansard.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-1-m,medium-mem-1-l,high-mem-1,high-mem-2
#SBATCH --mem=200G
#SBATCH --exclusive
#SBATCH --ntasks-per-node=35

module purge
module load r

hostname
Rscript tokenize_hansard.R --cores ${SLURM_NTASKS}
