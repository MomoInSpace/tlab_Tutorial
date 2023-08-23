export ${file_path_09:='/home/m/m300912/tlab_coding/tlab_Tutorial/src/09_profiling/'}
export ${executable_path:='/home/m/m300912/tlab_coding/tlab_Tutorial/executable_matcalc/'}
export ${output_path:=$executable_path/'output/'}


mkdir -p $executable_path

cd $executable_path

nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=verystrict,gpu -target=gpu -Minfo=accel,inline -gpu=ccnative,lineinfo  -o profile_matrices.x #-ta=nvidia:cc80 
#nvfortran ./tlab_constants.f90 ./tlab_arrays.f90 ./matrix_output.f90 ./export_values.f90 ./matrix_multiply_module.f90 ./array_calc.f90 -acc=verystrict -ta=nvidia:cc80 -Minfo=accel,inline  -o profile_matrices.x

cd ..

mkdir -p $output_path
