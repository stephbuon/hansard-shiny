#!/bin/bash
#SBATCH -J tokenize_hansard_w_year_decade
#SBATCH -o tokenize_hansard_w_year_decade.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-1-m,medium-mem-1-l,high-mem-1,high-mem-2
#SBATCH --mem=200G
#SBATCH --exclusive
#SBATCH --ntasks-per-node=35

module purge
module load r

hostname
Rscript tokenize_hansard_w_year_decade.R --cores ${SLURM_NTASKS}
