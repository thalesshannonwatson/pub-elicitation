import multiprocessing
from subprocess import call
import sys
from itertools import product

NRPROC=1 # run 1 proc / node
instId = sys.argv[1]
startIndex = (int(instId)-1) * NRPROC

## Script calling function
def callScript(params):
	n = str(params[0])
	inst = str(params[1])
	print "Starting with n", n, "inst", inst
	call("make data/convergence-entropies.rds N=" + n + " INST=" + inst, shell=True)
	print "Done"

## Define the parameter set
n = range(3, 13) # n=3, ..., 12
inst = range(1, 101) # 100 instances
allTasks = list(product(n, inst))
myTasks = allTasks[startIndex:(startIndex+NRPROC)]

## Start processing in parallel
pool = multiprocessing.Pool(processes=NRPROC)
print "Making computational tests for instance ID", instId
r = pool.map_async(callScript, myTasks)
r.wait() # Wait on the results
