#!/bin/bash
#SBATCH -J speech_lengths
#SBATCH -o speech_lengths.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=150G

module purge
module load r

Rscript speech_lengths.R
