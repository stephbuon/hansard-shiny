#!/bin/bash
#SBATCH -J export_hansard_viewer_data
#SBATCH -o export_hansard_viewer_data.out
#SBATCH -p standard-mem-s,standard-mem-m,standard-mem-l,medium-mem-1-s,medium-mem-2,medium-mem-1-m,high-mem-1,high-mem-2
#SBATCH --mem=6G

PREPROCESS_CODE_DIR = "$HOME/preprocess/"

code = "export_hansard_corpus.sbatch "

module purge
module load r
module load python/3

for c in $code; 
do
    sbatch $PREPROCESS_CODE_DIR +  ${c}
done
