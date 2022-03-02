#!/bin/bash
#SBATCH -J nation_concerns_data
#SBATCH -o nation_concerns_data.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=80G

module purge
module load r
Rscript count_nation_pairs_in_debate_titles.R
