import numpy as np
import matplotlib.pyplot as plt
# import pandas as pd

times = np.genfromtxt("./output_omp/time_omp_node_gpuImpls.csv", delimiter=",")[:,1:-1]#,invalid_raise=False)#, usemask=True)

time_factor = 10**3
cpu_factor = 1

plt.semilogy(times[0]**2,time_factor*times[1]/cpu_factor,label="CPU Slow")
#plt.semilogy(times[0]**2,time_factor*times[2]/cpu_factor,label="cpu dot")
plt.semilogy(times[0]**2,time_factor*times[3]/cpu_factor,label="CPU Intrinsic")
#plt.semilogy(times[0]**2,time_factor*times[5],label="gpu acc for")
#plt.semilogy(times[0]**2,time_factor*times[6],label="gpu dotmix")
plt.semilogy(times[0]**2,time_factor*times[7],label="CPU OpenMP")
plt.semilogy(times[0]**2,time_factor*times[4],label="GPU Kernels")
#plt.semilogy(times[0]**2,time_factor*times[8],label="gpu no copy")
#plt.semilogy(times[0]**2,time_factor*times[9],label="gpu no data")

plt.xlabel ('Matrix Size [n*m]')
plt.ylabel ('Time [mSec]')


plt.suptitle("Profiles of Matrix Multiplication Implementations")
plt.title("8 Multiplications")

plt.legend()
plt.savefig("output_omp/time_open_mp_node_gpuImpls.pdf")
plt.show()
