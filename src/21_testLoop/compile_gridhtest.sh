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
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/21_testLoop"
cd $gpu_profile
mkdir build -p
cd build

mpifort $gpu_profile/program_accTestLoop.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o testLook.run

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
