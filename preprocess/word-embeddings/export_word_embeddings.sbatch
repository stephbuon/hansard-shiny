#!/bin/bash
#SBATCH -J export_word_embeddings
#SBATCH -o export_word_embeddings.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=60G

module purge
module load r

Rscript export_word_embeddings.R
