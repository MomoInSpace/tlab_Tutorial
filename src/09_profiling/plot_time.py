import numpy as np
import matplotlib.pyplot as plt
# import pandas as pd

times = np.genfromtxt("./output2/time_8n.csv", delimiter=",")[:,1:-1]#,invalid_raise=False)#, usemask=True)

time_factor = 10**3
cpu_factor = 32

plt.semilogy(times[0]**2,time_factor*times[1]/cpu_factor,label="cpu slow*")
plt.semilogy(times[0]**2,time_factor*times[2]/cpu_factor,label="cpu dot*")
plt.semilogy(times[0]**2,time_factor*times[3]/cpu_factor,label="cpu intrinsic*")
plt.semilogy(times[0]**2,time_factor*times[4],label="gpu kernels")
plt.semilogy(times[0]**2,time_factor*times[5],label="gpu acc for")
plt.semilogy(times[0]**2,time_factor*times[6],label="gpu dotmix")

plt.xlabel ('Matrix Size [n*m]')
plt.ylabel ('Time [mSec]')


plt.suptitle("Profiles of Matrix Multiplication Implementations")
plt.title("8 Multiplication, Core Adjusted (32)")

plt.legend()
plt.savefig("output2/time_8n_adj32.pdf")
plt.show()