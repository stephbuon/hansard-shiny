#!/bin/bash
#SBATCH -J export_hansard_corpus
#SBATCH -o export_hansard_corpus.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=150G

module purge
module load r

Rscript export_hansard_corpus.R