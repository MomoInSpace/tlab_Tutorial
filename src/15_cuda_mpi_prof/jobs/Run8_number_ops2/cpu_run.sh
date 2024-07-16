#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=128
#SBATCH --mem=0
#SBATCH --time=12:00:00
#SBATCH --output=CPU_Run_%A_%a.out
#SBATCH --constraint=a100_80
#SBATCH --exclusive
#
#
# old SBATCH --ntasks-per-node=128
# old SBATCH --gpus-per-task=1
# old SBATCH --ntasks=16

# Start of script ========================================
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/15_cuda_mpi_prof/"

let tot_size=51200
let overclock=8
# 1 GPU per Task:
for ((i=64; i<=64*$overclock; i=i+64)) do
    let num=$tot_size/$i 
    echo Tasks: $i, Number of Ops: $num
    mpirun -np $i $gpu_profile/build/test_ring_times_cpu_only.o 750 750 $num "cpu_conf_cpt4"
done

# Test
#mpirun -np 2   $gpu_profile/build/test_ring_times.o 20032 20032 128 "sameTot"
#mpirun -np 4   $gpu_profile/build/test_ring_times.o 10016 10016 128 "sameTot"
#mpirun -np 8   $gpu_profile/build/test_ring_times.o  5008  5008 128 "sameTot"
#mpirun -np 16  $gpu_profile/build/test_ring_times.o  2504  2504 128 "sameTot"
#mpirun -np 32  $gpu_profile/build/test_ring_times.o  1202  1202 128 "sameTot"
#mpirun -np 64  $gpu_profile/build/test_ring_times.o   626   626 128 "sameTot"
#mpirun -np 128 $gpu_profile/build/test_ring_times.o   313   313 128 "sameTot"

#mpirun -np 5 $gpu_profile/build/test_ring_times.o 3000 3000 5 "5only5"

#mpirun -np 10 $gpu_profile/build/test_ring_times.o 3000 3000 5 "10only5"

#mpirun -np 15 $gpu_profile/build/test_ring_times.o 3000 3000 5 "15only5"

#mpirun -np 20 $gpu_profile/build/test_ring_times.o 3000 3000 5 "20only5"

#mpirun -np 20 $gpu_profile/build/test_ring_times.o 3000 100 20 "20full"

#mpirun -np 20 $gpu_profile/build/test_ring_times.o 30000 1000 20 "20full"

##### SBATCH --mem=24576
##### SBATCH --nodes=5
##### SBATCH --ntasks-per-node=2
