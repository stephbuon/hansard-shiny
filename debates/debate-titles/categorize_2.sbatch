#!/bin/bash
#SBATCH -J categorize_2
#SBATCH -o categorize_2.out
#SBATCH -p htc,standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=150G

module purge
module load r

Rscript categorize_2.R
