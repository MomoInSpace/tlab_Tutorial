#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=1
#SBATCH --time=5:00:00
#SBATCH --mem=8196


# Begin of section with executable commands
set -e
ls -l
bash $scratch/kaggle-cloud-organization_2/run_seg1.sh
bash $scratch/kaggle-cloud-organization_2/run_seg2.sh
bash $scratch/kaggle-cloud-organization_2/run_cls.sh

