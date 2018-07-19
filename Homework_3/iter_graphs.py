import numpy as np
import matplotlib.pyplot as plt

tc = [100, 1000, 10000, 100000, 1000000]

null_threads = []
sync_threads = []
get_threads = []
unsync_threads = []
better_threads = []

lines = []
with open('data/null_iter.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	null_threads.append(float_list)

with open('data/sync_iter.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	sync_threads.append(float_list)

with open('data/better_iter.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	better_threads.append(float_list)

with open('data/get_iter.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	get_threads.append(float_list)

with open('data/unsync_iter.txt', 'r') as my_file:
	lines = my_file.read().splitlines()

for line in lines:
	str_list = line.split(',')
	float_list = [float(x) for x in str_list]
	unsync_threads.append(float_list)

null_threads = np.matrix(null_threads)
sync_threads = np.matrix(sync_threads)
unsync_threads = np.matrix(unsync_threads)
get_threads = np.matrix(get_threads)
better_threads = np.matrix(better_threads)

null_avg = np.squeeze(np.asarray(np.average(null_threads, axis=0)))
sync_avg = np.squeeze(np.asarray(np.average(sync_threads, axis=0)))
unsync_avg = np.squeeze(np.asarray(np.average(unsync_threads, axis=0)))
get_avg = np.squeeze(np.asarray(np.average(get_threads, axis=0)))
better_avg = np.squeeze(np.asarray(np.average(better_threads, axis=0)))

fig, ax = plt.subplots()
ax.set_xscale('log', basex=10)
ax.set_yscale('log', basey=10)
ax.set_xlabel('Number of Iterations')
ax.set_ylabel('Average ns/iteration')
ax.grid()
plt.plot(tc, null_avg, color='blue', label='null')
plt.plot(tc, better_avg, color='red', label='sync')
plt.plot(tc, unsync_avg, color='green', label='unsync')
plt.plot([100, 1000, 10000], get_avg, color='black', label='getnset')
plt.plot(tc, sync_avg, color='purple', label='bettersafe')
plt.legend()
plt.show()