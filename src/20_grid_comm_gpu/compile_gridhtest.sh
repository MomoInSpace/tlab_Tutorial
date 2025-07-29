#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --mem=10g
#SBATCH --ntasks=16
#SBATCH --time=12:00:00
#SBATCH --output=ProfileGPU_Comm.out
#SBATCH --constraint=a100_80

#--exclusive

# Start of script ========================================
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/20_grid_comm_gpu"
cd $gpu_profile
mkdir build -p
cd build

mpifort $gpu_profile/tlab_constants.cuf $gpu_profile/tlab_arrays.cuf $gpu_profile/grid_handler.cuf $gpu_profile/grid_communicator.cuf $gpu_profile/grid_debug.cuf  $gpu_profile/program_gridhtest.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -mp -o comm_test.run

# Test
#mpirun -np 4 $gpu_profile/build/comm_test.o 10 4 10
#
#mpirun -np 8 $gpu_profile/build/comm_test.o 10 4 10
#
#mpirun -np 12 $gpu_profile/build/comm_test.o 10 4 10
#
#mpirun -np 16 $gpu_profile/build/comm_test.o 10 4 10

##### SBATCH --mem=24576
##### SBATCH --ntasks-per-node=2
##### SBATCH --exclusive
##### SBATCH --nodes=4
##### SBATCH --ntasks-per-node=4
