#!/bin/bash
#SBATCH -J nation_concern_count
#SBATCH -o nation_concern_count-4.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=25G

module purge 
module load python/3

python nation_concern_count.py hansard_justnine_decades1840_1849.csv nations.csv concerns.csv
