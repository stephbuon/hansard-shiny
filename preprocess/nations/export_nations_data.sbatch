#!/bin/bash
#SBATCH -J export_nations_data
#SBATCH -o export_nations_data.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=80G

module purge
module load r
Rscript export_clean_nations_data.R
Rscript count_nations_in_debate_titles.R
Rscript count_nations_debate_text.R
