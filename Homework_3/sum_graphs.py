import numpy as np
import matplotlib.pyplot as plt

sums = [275, 300, 325, 350, 375]

null_sum = []
sync_sum = []

lines = []
with open('data/null_sum.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	null_sum.append(float_list)

with open('data/sync_sum.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	sync_sum.append(float_list)

null_avg = np.average(null_sum, axis=0)
sync_avg = np.average(sync_sum, axis=0)

fig, ax = plt.subplots()
ax.set_xlabel('Initial Sum')
ax.set_ylabel('Average ns/iteration')
ax.grid()
plt.plot(sums, null_avg, color='blue', label='null')
plt.plot(sums, sync_avg, color='red', label='sync')
plt.legend()
plt.show()

