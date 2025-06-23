#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --account=bb1153
#STATCH --ntasks=20
#SBATCH --mem=0
#SBATCH --time=12:00:00
#SBATCH --output=Run_10_one_5_%A_%a.out
#SBATCH --constraint=a100_80
#SBATCH --exclusive

#--exclusive

# Start of script ========================================
export gpu_profile="$HOME/fortran_projs/tlab_Tutorial/src/20_grid_comm_gpu"
cd $gpu_profile
mkdir build -p
cd build

#mpifort $gpu_profile/tlab_constants.f90 $gpu_profile/tlab_arrays.f90 $gpu_profile/export_values.f90 $gpu_profile/timer.f90 $gpu_profile/ring_timer_module.f90 -acc=gpu -target=gpu -Minfo=accel,inline -gpu=lineinfo,cc80 -cpp -o test_ring_times.o

#mpifort $gpu_profile/tlab_constants.f90 $gpu_profile/tlab_arrays.f90 $gpu_profile/export_values.f90 $gpu_profile/timer.f90 $gpu_profile/ring_timer_module.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -o test_ring_times_gpu_only.o

#mpifort $gpu_profile/tlab_constants.f90 $gpu_profile/tlab_arrays.f90 $gpu_profile/export_values.f90 $gpu_profile/timer.f90 $gpu_profile/ring_timer_module.cuf -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -o test_ring_times_cpu_only.o

#mpifort $gpu_profile/tlab_constants.f90 $gpu_profile/tlab_arrays.f90 $gpu_profile/tlab_arrays_gpu.f90 $gpu_profile/grid_handler.f90 $gpu_profile/grid_communicator.f90 $gpu_profile/grid_debug.f90  $gpu_profile/program_gridhtest.f90 -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -o comm_test.o

#mpifort $gpu_profile/bandwidth.f90 -acc=gpu -target=gpu -gpu=lineinfo,cc80 -cpp -g -O0 -o bandwidth.o

#mpifort ./ring-irecv_30.f90  ./ring_std.o

mpifort  $gpu_profile/tlab_constants.f90 $gpu_profile/tlab_arrays.cuf $gpu_profile/grid_handler.f90 $gpu_profile/grid_communicator.f90 $gpu_profile/grid_debug.f90  $gpu_profile/program_gridhtest.cuf -acc=gpu -target=gpu -Minfo=accel,inline -gpu=lineinfo,cc80 -cpp -o comm_test.o


# Test
#mpirun -np 5 $gpu_profile/build/test_ring_times.o 3000 250 5 "5only5"

#mpirun -np 10 $gpu_profile/build/test_ring_times.o 3000 250 5 "10only5"

#mpirun -np 15 $gpu_profile/build/test_ring_times.o 3000 250 5 "15only5"

#mpirun -np 20 $gpu_profile/build/test_ring_times.o 3000 250 5 "20only5"

##### SBATCH --mem=24576
##### SBATCH --nodes=5
##### SBATCH --ntasks-per-node=2
