#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --nodes=1
#SBATCH --time=1:00:00
#SBATCH --mem=1024
#SBATCH --output=output/output-%j
#SBATCH --error=output/error-%j

module purge
module load nvhpc/22.5-gcc-11.2.0 fftw gcc openmpi
#module load arm-forge arm-forge/22.1-gcc-11.2.0

export tlab_Tutorial=/home/m/m300912/fortran_projs/tlab_Tutorial
export file_path_09=$tlab_Tutorial/src/09_profiling
export executable_path=$file_path_09/build/
export output_path=$file_path_09/output_cpu/


# Compile Programm:
set -e
ls -l
mkdir -p $executable_path
cd $executable_path
#nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -o profile_matrices.x #-ta=nvidia:cc80
nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=multicore -target=multicore -gpu=lineinfo,cc80 -cpp -o profile_matrices.x #-ta=nvidia:cc80

# Run Programm:
mkdir -p $output_path
cd $output_path
mpirun -n 128 $executable_path/profile_matrices.x

#python ../src/python_scripts/plot_time.py

###       SBATCH --ntasks-per-node=1
