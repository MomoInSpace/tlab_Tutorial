



#module load python3 pytorch #git
#source activate ml_cloud
#alias conda="~/miniconda3/bin/conda"

#Modules for gpu-fortran
module load git nvhpc python3


export scratch=/scratch/m/m300912
export TORCH_HOME=$scratch
export path09=/home/m/m300912/tlab_Tutorial/src/09_profiling/

# Symlinks:
#ln -sfn $scratch ~/scratch
#ln -sfn ~/. $scratch/home

alias gpu='salloc -p gpu -A bb1153 -n 4 -t 480 --mem 8196'
alias gpu2='salloc -p gpu -A bb1153 --gpus=4 -t 480 --exclusive -N1'
alias gpu_small='salloc -p gpu -A bb1153 -n 4 -t 30 --mem 1024'
alias cpu='salloc -p interactive -A bb1153 -n 1 -t 720 --mem 1024'
alias kaggle='/home/m/m300912/.local/bin/kaggle'


#alias runml = 'python3 run.py train with configs/model_b_07.yaml -f'


