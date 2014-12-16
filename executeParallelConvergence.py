import multiprocessing
from subprocess import call
import sys
from itertools import product

NRPROC=1 # changed to run 1 proc / node
instId = sys.argv[1]
startIndex = (int(instId)-1) * NRPROC

## Script calling function
def callScript(n):
	n = str(n)
	print "Starting with n", n
	call("make data/convergence.n" + n + ".rds", shell=True)
	print "Done"

## Define the parameter set
## For n=3, ..., 15
nk = range(3, 16)
myTasks = nk[startIndex:(startIndex+NRPROC)]

## Start processing in parallel
pool = multiprocessing.Pool(processes=NRPROC)
print "Making computational tests for instance ID", instId
r = pool.map_async(callScript, myTasks)
r.wait() # Wait on the results

