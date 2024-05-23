#file_path_09=${file_path_09:='/home/m/m300912/tlab_coding/tlab_Tutorial/src/09_profiling/'}
#executable_path=${executable_path:='/home/m/m300912/tlab_coding/tlab_Tutorial/executable_matcalc/'}
#output_path=${output_path:=$executable_path/'output/'}

export tlab_Tutorial=/home/m/m300912/tlab_Tutorial
export file_path_09=$tlab_Tutorial/src/09_profiling
export executable_path=$tlab_Tutorial/executable_matcalc/
export output_path=$executable_path/output/


mkdir -p $executable_path

cd $executable_path

#Old version with -ta
nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -Minfo=accel,inline -acc=gpu,verystrict -gpu=ccnative -Mfree -o profile_matrices.x #-ta=nvidia:cc80 

#Version which doesn't find gpu and doesn't compile
#nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=verystrict,gpu -target=gpu -Minfo=accel,inline -gpu=ccnative,lineinfo  -o profile_matrices.x #-ta=nvidia:cc80 

# Version which compiles without gpu
#nvfortran $file_path_09/tlab_constants.f90 $file_path_09/tlab_arrays.f90 $file_path_09/matrix_output.f90 $file_path_09/export_values.f90 $file_path_09/matrix_multiply_module.f90 $file_path_09/array_calc.f90 -acc=verystrict -Minfo=accel,inline  -o profile_matrices.x #-ta=nvidia:cc80 

#cd ..

# mkdir -p $output_path
