export file_path_09=$PWD/'../src/09_profiling'
export executable_path=$PWD/'executable/'
export output_path=$PWD/'output/'


mkdir -p $executable_path

cd $executable_path

nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=verystrict -Minfo=accel,inline  -o profile_matrices.x #-ta=nvidia:cc80 
#nvfortran ./tlab_constants.f90 ./tlab_arrays.f90 ./matrix_output.f90 ./export_values.f90 ./matrix_multiply_module.f90 ./array_calc.f90 -acc=verystrict -ta=nvidia:cc80 -Minfo=accel,inline  -o profile_matrices.x

cd ..

mkdir -p $output_path

#cd $output_path

#../$executable_path/profile_matrices.x

#python ../src/python_scripts/plot_time.py

