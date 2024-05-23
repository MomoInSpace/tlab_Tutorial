#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=1
#SBATCH --time=1:00:00
#SBATCH --mem=4096
#SBATCH --output=jobs/tlab/output-%j
#SBATCH --error=jobs/tlab/error-%j

module purge
module load git nvhpc python3


# Begin of section with executable commands
export tlab_Tutorial=/home/m/m300912/tlab_Tutorial
export file_path_09=$tlab_Tutorial/src/09_profiling
export executable_path=$tlab_Tutorial/executable_matcalc/
export output_path=$executable_path/output/

set -e
ls -l
# Compile Programm:
cd $executable_path
source $file_path_09/compile_matcalc.sh

# Run Programm:
cd $output_path
$executable_path/profile_matrices.x

#python ../src/python_scripts/plot_time.py


