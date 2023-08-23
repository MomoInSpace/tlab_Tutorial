import numpy as np
import matplotlib.pyplot as plt
# import pandas as pd 

times = np.genfromtxt("./time.csv", delimiter=",")[:,:-1]#,invalid_raise=False)#, usemask=True)

# print(times)          # Test if it reads values

# for time in times:    # Fast Plot
#     plt.plot(time)

# Clean Plot:
plt.plot(times[0],times[1],label="cpu slow")
plt.plot(times[0],times[2],label="cpu dot")
plt.plot(times[0],times[3],label="cpu intrinsic")
plt.plot(times[0],times[4],label="cpu foracc")

plt.legend()
plt.savefig("times.png")
plt.show()