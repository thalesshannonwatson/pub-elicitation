import multiprocessing
from subprocess import call
import sys
from itertools import product

NRPROC=10 # run 10 procs / node (12 cores per node, to be sure we don't get biases in the running time computations)
instId = sys.argv[1]
startIndex = (int(instId)-1) * NRPROC

## Script calling function
def callScript(params):
	n = str(params[0])
	myseed = str(params[1])
	print "Starting with n", n, "seed", myseed
	call("make data/timing-test.rds N=" + n + " SEED=" + myseed, shell=True)
	print "Done"

## Define the parameter set
## For n=3, ..., 10
n = range(3, 11)
seeds = range(1, 21) # 20 seeds
allTasks = list(product(n, seeds))
myTasks = allTasks[startIndex:(startIndex+NRPROC)]

## Start processing in parallel
pool = multiprocessing.Pool(processes=NRPROC)
print "Making computational tests for instance ID", instId
r = pool.map_async(callScript, myTasks)
r.wait() # Wait on the results
