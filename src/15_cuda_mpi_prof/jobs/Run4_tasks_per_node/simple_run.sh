#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --nodes=5
#SBATCH --ntasks-per-node=4
#SBATCH --mem=0
#SBATCH --time=12:00:00
#SBATCH --output=Run_10_one_5_%A_%a.out
#SBATCH --constraint=a100_80
#SBATCH --exclusive

#--exclusive

# Start of script ========================================
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/15_cuda_mpi_prof/"

# Test
mpirun -np 5 $gpu_profile/build/test_ring_times.o 2000 2000   256 "only256"
mpirun -np 10 $gpu_profile/build/test_ring_times.o 2000 2000   256 "only256"
mpirun -np 15 $gpu_profile/build/test_ring_times.o 2000 2000   256 "only256"
mpirun -np 20 $gpu_profile/build/test_ring_times.o 2000 2000  256 "only256"
#mpirun -np 5 $gpu_profile/build/test_ring_times.o 3000 3000 5 "5only5"

#mpirun -np 10 $gpu_profile/build/test_ring_times.o 3000 3000 5 "10only5"

#mpirun -np 15 $gpu_profile/build/test_ring_times.o 3000 3000 5 "15only5"

#mpirun -np 20 $gpu_profile/build/test_ring_times.o 3000 3000 5 "20only5"

#mpirun -np 20 $gpu_profile/build/test_ring_times.o 3000 100 20 "20full"

#mpirun -np 20 $gpu_profile/build/test_ring_times.o 30000 1000 20 "20full"

##### SBATCH --mem=24576
##### SBATCH --nodes=5
##### SBATCH --ntasks-per-node=2
