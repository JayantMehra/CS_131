import numpy as np
import matplotlib.pyplot as plt

sizes = range(2, 9)

null_size = []
sync_size = []

lines = []
with open('data/null_size.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	null_size.append(float_list)

with open('data/sync_size.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	sync_size.append(float_list)

null_avg = np.average(null_size, axis=0)
sync_avg = np.average(sync_size, axis=0)

fig, ax = plt.subplots()
ax.set_xlabel('Size of Array')
ax.set_ylabel('Average ns/iteration')
ax.grid()
plt.plot(sizes, null_avg, color='blue', label='null')
plt.plot(sizes, sync_avg, color='red', label='sync')
plt.legend()
plt.show()

