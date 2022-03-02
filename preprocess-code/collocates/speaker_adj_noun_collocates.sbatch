#!/bin/bash
#SBATCH -J speaker_adj_noun_collocates
#SBATCH -o speaker_adj_noun_collocates.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-1-m,medium-mem-1-l,high-mem-1,high-mem-2
#SBATCH --mem=60G
#SBATCH --exclusive

module purge
module load python/3

hostname
python speaker_adj_noun_collocates.py
