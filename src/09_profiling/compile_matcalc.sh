#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --nodes=1
#SBATCH --cpus-per-task=128
#SBATCH --gpus=1                   # Specify number of GPUs needed for the job
#SBATCH --time=1:00:00
#SBATCH --mem=1024
#SBATCH --output=output_omp/output-%j
#SBATCH --error=output_omp/error-%j

module purge
module load git python3 fftw gcc
module load openmpi/4.1.5-nvhpc-25.5 nvhpc/25.5-gcc-13.3.0
#module load arm-forge arm-forge/22.1-gcc-11.2.0

export tlab_Tutorial=/home/m/m300912/fortran_projs/tlab_Tutorial
export file_path_09=$tlab_Tutorial/src/09_profiling
export executable_path=$file_path_09/build/
export output_path=$file_path_09/output_omp/


# Compile Programm:
set -e
ls -l
mkdir -p $executable_path
cd $executable_path
nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=gpu -target=gpu -gpu=lineinfo -mp -cpp -o profile_matrices.x

#nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=multicore -target=multicore -gpu=lineinfo,cc80 -cpp -o profile_matrices.x #-ta=nvidia:cc80

# Run Programm:
mkdir -p $output_path
cd $output_path
$executable_path/profile_matrices.x

#python ../src/python_scripts/plot_time.py

###       SBATCH --ntasks-per-node=1
