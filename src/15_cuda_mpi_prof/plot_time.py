import numpy as np
import matplotlib.pyplot as plt
# import pandas as pd 

times = np.genfromtxt("./time.csv", delimiter=",")[:,:-1]#,invalid_raise=False)#, usemask=True)

# print(times)          # Test if it reads values

# for time in times:    # Fast Plot
#     plt.plot(time)

# Clean Plot:
#plt.semilogy(times[0],times[1],label="cpu slow")
plt.plot(times[0],times[1],label="gpu device")
#plt.semilogy(times[0],times[3],label="gpu acc")
plt.plot(times[0],times[2],label="cpu fast")
#plt.semilogy(times[0],times[5],label="cpu fast")

# plt.plot(times[0],times[7],label="gpu intrinsic")


plt.legend()
plt.savefig("times.png")
#plt.show()
