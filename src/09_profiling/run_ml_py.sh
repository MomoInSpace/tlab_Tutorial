#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=1
#SBATCH --time=5:00:00
#SBATCH --mem=8196
#SBATCH --output=jobs/tlab/output-%j
#SBATCH --error=jobs/tlab/error-%j



# Begin of section with executable commands
export ${file_path_09:='/home/m/m300912/tlab_coding/tlab_Tutorial/src/09_profiling/'}
export ${executable_path:='/home/m/m300912/tlab_coding/tlab_Tutorial/executable_matcalc/'}
export ${output_path:=$executable_path/'output/'}

set -e
ls -l
# Compile Programm:
cd $executable_path
source $file_path_09/compile_matcalc.sh

# Run Programm:
cd $output_path
$executable_path/profile_matrices.x

#python ../src/python_scripts/plot_time.py


