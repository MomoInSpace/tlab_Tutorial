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

#Compile Library
mpifort -c $gpu_profile/tlab_constants.cuf
mpifort -c $gpu_profile/tlab_arrays.cuf  tlab_constants.o
mpifort -c $gpu_profile/grid_handler.cuf tlab_arrays.o
mpifort -c $gpu_profile/grid_handler_cpu.cuf grid_handler.o
mpifort -c $gpu_profile/grid_handler_gpu.cuf grid_handler.o -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline
mpifort -c $gpu_profile/grid_communicator.cuf grid_handler.o
mpifort -c $gpu_profile/grid_communicator_cpu.cuf grid_handler_cpu.o
mpifort -c $gpu_profile/grid_communicator_gpu.cuf grid_handler_gpu.o -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline
mpifort -c $gpu_profile/grid_debug.cuf grid_handler_gpu.o grid_handler_cpu.o
mpifort -c $gpu_profile/grid_ops.cuf
#mpifort -c tlab_constants.o  tlab_arrays.o  grid_handler.o  grid_handler_cpu.o  grid_communicator.o  grid_communicator_cpu.o  grid_debug.o   grid_ops.o -o commLib.o


#Gridh Test CPU
#mpifort  tlab_constants.o  tlab_arrays.o  grid_handler.o  $gpu_profile/grid_handler_gpu.cuf  grid_handler_cpu.o  grid_communicator.o  $gpu_profile/grid_communicator_gpu.cuf  grid_communicator_cpu.o  grid_debug.o   grid_ops.o  $gpu_profile/program_gridhtest.f90 -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o gridh_test_cpu.run

#Gridh Test GPU
#mpifort  tlab_constants.o  tlab_arrays.o  grid_handler.o  $gpu_profile/grid_handler_gpu.cuf  grid_handler_cpu.o  grid_communicator.o  $gpu_profile/grid_communicator_gpu.cuf  grid_communicator_cpu.o  grid_debug.o   grid_ops.o  $gpu_profile/program_gridhtest.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o gridh_test_gpu.run

# Snabla CPU
mpifort  tlab_constants.o  tlab_arrays.o  grid_handler.o  $gpu_profile/grid_handler_gpu.cuf  grid_handler_cpu.o  grid_communicator.o  $gpu_profile/grid_communicator_gpu.cuf  grid_communicator_cpu.o  grid_debug.o   grid_ops.o  $gpu_profile/program_test_diff_cpu.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o diff_test_cpu.run

# Snabla GPU
mpifort  tlab_constants.o  tlab_arrays.o  grid_handler.o  $gpu_profile/grid_handler_gpu.cuf  grid_handler_cpu.o  grid_communicator.o  $gpu_profile/grid_communicator_gpu.cuf  grid_communicator_cpu.o  grid_debug.o   grid_ops.o  $gpu_profile/program_test_diff_gpu.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o diff_test_gpu.run
#mpifort $gpu_profile/tlab_constants.cuf $gpu_profile/tlab_arrays.cuf $gpu_profile/grid_handler.cuf $gpu_profile/grid_handler_gpu.cuf $gpu_profile/grid_handler_cpu.cuf $gpu_profile/grid_communicator.cuf $gpu_profile/grid_communicator_gpu.cuf $gpu_profile/grid_communicator_cpu.cuf $gpu_profile/grid_debug.cuf  $gpu_profile/grid_ops.cuf $gpu_profile/program_simple_nabla_gpu.cuf  -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -Minfo=accel,inline -o comm_snabla_gpu.run

# Test
#echo "N4 CPU Run"
#mpirun -n 4 diff_test_cpu.run 16 8 32
#echo "N4 GPU Run"
#mpirun -n 4 diff_test_gpu.run 16 8 32
#
#echo "N8 CPU Run"
#mpirun -n 8 diff_test_cpu.run 8 8 32
#echo "N8 GPU Run"
#mpirun -n 8 diff_test_gpu.run 8 8 32

#echo "N12 CPU Run"
#mpirun -n 12 diff_test_cpu.run 16 4 16
#echo "N12 GPU Run"
#mpirun -n 12 diff_test_gpu.run 16 4 16

#echo "N16 CPU Run"
#mpirun -n 16 diff_test_cpu.run 4 2 8
#echo "N16 GPU Run"
#mpirun -n 16 diff_test_gpu.run 4 2 8

##### SBATCH --mem=24576
##### SBATCH --ntasks-per-node=2
##### SBATCH --exclusive
##### SBATCH --ntasks-per-node=4
