#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --ntasks=16
#SBATCH --mem=10g
#SBATCH --time=12:00:00
#SBATCH --output=ProfileCPU_Comm.out
#SBATCH --constraint=a100_80

#--exclusive

# Start of script ========================================
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/19_grid_comm2"
cd $gpu_profile
mkdir build -p
cd build

mpifort $gpu_profile/tlab_constants.f90 $gpu_profile/tlab_arrays.f90 $gpu_profile/grid_handler.f90 $gpu_profile/grid_communicator.f90 $gpu_profile/grid_debug.f90  $gpu_profile/program_gridhtest.f90 -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -o comm_test.run

# Test
#time mpirun -np 4 $gpu_profile/build/comm_test.o 10 4 10
#
#time mpirun -np 8 $gpu_profile/build/comm_test.o 10 4 10
#
#time mpirun -np 12 $gpu_profile/build/comm_test.o 10 4 10
#
#time mpirun -np 16 $gpu_profile/build/comm_test.o 10 4 10

##### SBATCH --mem=24576
##### SBATCH --ntasks-per-node=2
##### SBATCH --exclusive
##### SBATCH --nodes=4
##### SBATCH --ntasks-per-node=4
