#!/bin/bash
#SBATCH -J export_hansard_viewer_data
#SBATCH -o export_hansard_viewer_data.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=6G

preprocess_code_dir="/users/sbuongiorno/hansard-shiny/preprocess"

scripts="hansard-corpus/export_hansard_corpus.R intro/intro.R"

module purge
module load r
module load python/3

for script in $scripts; 
do
    sbatch ${preprocess_code_dir}/${script}
done


