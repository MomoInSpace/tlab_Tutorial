#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#SBATCH --ntasks=16
#SBATCH --gpus-per-task=1
#SBATCH --time=1:00:00
#SBATCH --output=runs/CalcDiff_4Node_2.out
#SBATCH --constraint=a100_80
#SBATCH --mem=2g
#SBATCH --exclusive

#--exclusive

# Start of script ========================================
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/20_grid_comm_gpu"
cd $gpu_profile
mkdir build -p
cd build

#mpifort $gpu_profile/tlab_constants.cuf $gpu_profile/tlab_arrays.cuf $gpu_profile/grid_handler.cuf $gpu_profile/grid_handler_gpu.cuf $gpu_profile/grid_handler_cpu.cuf $gpu_profile/grid_communicator.cuf $gpu_profile/grid_communicator_gpu.cuf $gpu_profile/grid_communicator_cpu.cuf $gpu_profile/grid_debug.cuf  $gpu_profile/program_gridhtest.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o comm_test_gpu.run

#mpifort $gpu_profile/tlab_constants.cuf $gpu_profile/tlab_arrays.cuf $gpu_profile/grid_handler.cuf $gpu_profile/grid_handler_gpu.cuf $gpu_profile/grid_handler_cpu.cuf $gpu_profile/grid_communicator.cuf $gpu_profile/grid_communicator_gpu.cuf $gpu_profile/grid_communicator_cpu.cuf $gpu_profile/grid_debug.cuf  $gpu_profile/program_gridhtest.f90 -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o comm_test_cpu.run

# Snabla CPU
mpifort $gpu_profile/tlab_constants.cuf $gpu_profile/tlab_arrays.cuf $gpu_profile/grid_handler.cuf $gpu_profile/grid_handler_gpu.cuf $gpu_profile/grid_handler_cpu.cuf $gpu_profile/grid_communicator.cuf $gpu_profile/grid_communicator_gpu.cuf $gpu_profile/grid_communicator_cpu.cuf $gpu_profile/grid_debug.cuf  $gpu_profile/grid_ops.cuf $gpu_profile/program_test_diff_cpu.cuf  -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o comm_snabla_cpu.run

# Snabla GPU
mpifort $gpu_profile/tlab_constants.cuf $gpu_profile/tlab_arrays.cuf $gpu_profile/grid_handler.cuf $gpu_profile/grid_handler_gpu.cuf $gpu_profile/grid_handler_cpu.cuf $gpu_profile/grid_communicator.cuf $gpu_profile/grid_communicator_gpu.cuf $gpu_profile/grid_communicator_cpu.cuf $gpu_profile/grid_debug.cuf  $gpu_profile/grid_ops.cuf $gpu_profile/program_simple_nabla_gpu.cuf  -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o comm_snabla_gpu.run

# Test
echo "N4 CPU Run"
mpirun -n 4 comm_snabla_cpu.run 16 4 16
echo "N4 GPU Run"
mpirun -n 4 comm_snabla_gpu.run 16 4 16

echo "N8 CPU Run"
mpirun -n 8 comm_snabla_cpu.run 16 4 16
echo "N8 GPU Run"
mpirun -n 8 comm_snabla_gpu.run 16 4 16

echo "N12 CPU Run"
mpirun -n 12 comm_snabla_cpu.run 16 4 16
echo "N12 GPU Run"
mpirun -n 12 comm_snabla_gpu.run 16 4 16

echo "N16 CPU Run"
mpirun -n 16 comm_snabla_cpu.run 16 4 16
echo "N16 GPU Run"
mpirun -n 16 comm_snabla_gpu.run 16 4 16

##### SBATCH --mem=24576
##### SBATCH --ntasks-per-node=2
##### SBATCH --exclusive
##### SBATCH --ntasks-per-node=4
