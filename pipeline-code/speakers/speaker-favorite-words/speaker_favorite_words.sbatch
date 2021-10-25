#!/bin/bash
#SBATCH -J speaker_favorite_words
#SBATCH -o speaker_favorite_words.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=40G

module purge
module load r

Rscript speaker_favorite_words.R