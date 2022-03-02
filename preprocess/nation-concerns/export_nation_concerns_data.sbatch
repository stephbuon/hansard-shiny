#!/bin/bash
#SBATCH -J nation_concerns_data
#SBATCH -o nation_concerns_data.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=25G

module purge 
module load python/3

python nation_concerns_count.py hansard_c19_improved_speaker_names_app_data.csv nations.csv concerns.csv

module purge
module load r
Rscript join_nations_and_geography.R
Rscript export_treemap_data.R

