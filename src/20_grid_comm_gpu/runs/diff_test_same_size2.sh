#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --ntasks=16
#SBATCH --gpus-per-task=1
#SBATCH --time=1:00:00
#SBATCH --output=diff_test_same_size2.out
#SBATCH --constraint=a100_80
#SBATCH --mem=2g
#SBATCH --exclusive

#--exclusive

# Start of script ========================================
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/20_grid_comm_gpu"
#cd $gpu_profile
#mkdir build -p
#cd build

# Test
echo "# N4 CPU Run"
mpirun -n 4 $gpu_profile/build/diff_test_cpu.run 16 8 32
echo "# N4 GPU Run"
mpirun -n 4 $gpu_profile/build/diff_test_gpu.run 16 8 32

echo "# N8 CPU Run"
mpirun -n 8 $gpu_profile/build/diff_test_cpu.run 8 8 32
echo "# N8 GPU Run"
mpirun -n 8 $gpu_profile/build/diff_test_gpu.run 8 8 32

#echo "N12 CPU Run"
#mpirun -n 12 diff_test_cpu.run 16 4 16
#echo "N12 GPU Run"
#mpirun -n 12 diff_test_gpu.run 16 4 16

echo "# N16 CPU Run"
mpirun -n 16 $gpu_profile/build/diff_test_cpu.run 4 2 8
echo "# N16 GPU Run"
mpirun -n 16 $gpu_profile/build/diff_test_gpu.run 4 2 8


##### SBATCH --mem=24576
##### SBATCH --ntasks-per-node=2
##### SBATCH --exclusive
##### SBATCH --ntasks-per-node=4
