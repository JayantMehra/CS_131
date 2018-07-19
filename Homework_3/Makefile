default:
	javac UnsafeMemory.java
dist:
	tar -zcvf files.tar.gz *.java Makefile
jar: 
	jar cf jmmplus.jar *.java
clean:
	rm -f *.class files.tar.gz
tests:
	java UnsafeMemory Null 1  10000 20 10 10 10 10 10 >> data/null_thread.txt 2>&1
	java UnsafeMemory Null 2  10000 20 10 10 10 10 10 >> data/null_thread.txt 2>&1
	java UnsafeMemory Null 4  10000 20 10 10 10 10 10 >> data/null_thread.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10 10 10 10 >> data/null_thread.txt 2>&1
	java UnsafeMemory Null 16 10000 20 10 10 10 10 10 >> data/null_thread.txt 2>&1
	java UnsafeMemory Null 32 10000 20 10 10 10 10 10 >> data/null_thread.txt 2>&1
	java UnsafeMemory Null 8  100     20 10 10 10 10 10 >> data/null_iter.txt 2>&1
	java UnsafeMemory Null 8  1000    20 10 10 10 10 10 >> data/null_iter.txt 2>&1
	java UnsafeMemory Null 8  10000   20 10 10 10 10 10 >> data/null_iter.txt 2>&1
	java UnsafeMemory Null 8  100000  20 10 10 10 10 10 >> data/null_iter.txt 2>&1
	java UnsafeMemory Null 8  1000000 20 10 10 10 10 10 >> data/null_iter.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10             >> data/null_size.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10 10          >> data/null_size.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10 10 10       >> data/null_size.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10 10 10 10    >> data/null_size.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10 10 10 10 10 >> data/null_size.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10 10 10 10 10 10 >> data/null_size.txt 2>&1
	java UnsafeMemory Null 8  10000 20 10 10 10 10 10 10 10 10 >>data/null_size.txt 2>&1
	java UnsafeMemory Null 8  10000 20 15 10 10 10 10 >> data/null_sum.txt 2>&1
	java UnsafeMemory Null 8  10000 20 15 15 10 10 10 >> data/null_sum.txt 2>&1
	java UnsafeMemory Null 8  10000 20 15 15 15 10 10 >> data/null_sum.txt 2>&1
	java UnsafeMemory Null 8  10000 20 15 15 15 15 10 >> data/null_sum.txt 2>&1
	java UnsafeMemory Null 8  10000 20 15 15 15 15 15 >> data/null_sum.txt 2>&1	
	java UnsafeMemory Synchronized 1  10000 20 10 10 10 10 10 >> data/sync_thread.txt 2>&1
	java UnsafeMemory Synchronized 2  10000 20 10 10 10 10 10 >> data/sync_thread.txt 2>&1
	java UnsafeMemory Synchronized 4  10000 20 10 10 10 10 10 >> data/sync_thread.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10 10 10 10 >> data/sync_thread.txt 2>&1
	java UnsafeMemory Synchronized 16 10000 20 10 10 10 10 10 >> data/sync_thread.txt 2>&1
	java UnsafeMemory Synchronized 32 10000 20 10 10 10 10 10 >> data/sync_thread.txt 2>&1
	java UnsafeMemory Synchronized 8  100     20 10 10 10 10 10 >> data/sync_iter.txt 2>&1
	java UnsafeMemory Synchronized 8  1000    20 10 10 10 10 10 >> data/sync_iter.txt 2>&1
	java UnsafeMemory Synchronized 8  10000   20 10 10 10 10 10 >> data/sync_iter.txt 2>&1
	java UnsafeMemory Synchronized 8  100000  20 10 10 10 10 10 >> data/sync_iter.txt 2>&1
	java UnsafeMemory Synchronized 8  1000000 20 10 10 10 10 10 >> data/sync_iter.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10             >> data/sync_size.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10 10          >> data/sync_size.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10 10 10       >> data/sync_size.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10 10 10 10    >> data/sync_size.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10 10 10 10 10 >> data/sync_size.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10 10 10 10 10 10 >> data/sync_size.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 10 10 10 10 10 10 10 10 >>data/sync_size.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 15 10 10 10 10 >> data/sync_sum.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 15 15 10 10 10 >> data/sync_sum.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 15 15 15 10 10 >> data/sync_sum.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 15 15 15 15 10 >> data/sync_sum.txt 2>&1
	java UnsafeMemory Synchronized 8  10000 20 15 15 15 15 15 >> data/sync_sum.txt 2>&1	
	java UnsafeMemory BetterSafe 1  10000 20 10 10 10 10 10 >> data/better_thread.txt 2>&1
	java UnsafeMemory BetterSafe 2  10000 20 10 10 10 10 10 >> data/better_thread.txt 2>&1
	java UnsafeMemory BetterSafe 4  10000 20 10 10 10 10 10 >> data/better_thread.txt 2>&1
	java UnsafeMemory BetterSafe 8  10000 20 10 10 10 10 10 >> data/better_thread.txt 2>&1
	java UnsafeMemory BetterSafe 16 10000 20 10 10 10 10 10 >> data/better_thread.txt 2>&1
	java UnsafeMemory BetterSafe 32 10000 20 10 10 10 10 10 >> data/better_thread.txt 2>&1
	java UnsafeMemory BetterSafe 8  100     20 10 10 10 10 10 >> data/better_iter.txt 2>&1
	java UnsafeMemory BetterSafe 8  1000    20 10 10 10 10 10 >> data/better_iter.txt 2>&1
	java UnsafeMemory BetterSafe 8  10000   20 10 10 10 10 10 >> data/better_iter.txt 2>&1
	java UnsafeMemory BetterSafe 8  100000  20 10 10 10 10 10 >> data/better_iter.txt 2>&1
	java UnsafeMemory BetterSafe 8  1000000 20 10 10 10 10 10 >> data/better_iter.txt 2>&1
