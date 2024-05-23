# Standard
mpifort ./ring-irecv_30.f90 -acc=gpu -target=gpu -Minfo=accel,inline -gpu=lineinfo,cc80 -cpp -o ./ring_std.o

# Ring Acc
mpifort ./gpu_ring_acc.f08  -acc=gpu -target=gpu -Minfo=accel,inline -gpu=lineinfo,cc80 -cpp -o ./ring_acc.o

# Ring Managed
mpifort ./gpu_ring_managed.cuf  -acc=gpu -target=gpu -Minfo=accel,inline -gpu=lineinfo,cc80 -cpp -o ./ring_man.o

# Ring Managed
mpifort ./gpu_ring_device.cuf  -acc=gpu -target=gpu -Minfo=accel,inline -gpu=lineinfo,cc80 -cpp -o ./ring_dev.o


echo "Standard:"
time mpirun -n 10 ./ring_std.o

echo "Ring Acc"
time mpirun -n 10 ./ring_acc.o

echo "Ring Managegd"
time mpirun -n 10 ./ring_man.o

echo "Ring Device"
time mpirun -n 10 ./ring_dev.o
