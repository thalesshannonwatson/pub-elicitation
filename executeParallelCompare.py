import multiprocessing
from subprocess import call
import sys
from itertools import product

NRPROC=1 # run 1 / node to speed up the best plane search
instId = sys.argv[1]
startIndex = (int(instId)-1) * NRPROC

## Script calling function
def callScript(params):
	n = str(params[0])
	seed = str(params[1])
	method = params[2]
	print "Starting with seed", seed, "n", n, "method", method
	call("make data/compare-" + method + ".rds N=" + n + " SEED=" + seed, shell=True)
	print "Done"

## Define the parameter set
## seeds in [1, ..., 20]
## n in [3, 6, 9]
## tests in ['volume', 'entropy', 'random']
seeds = range(1, 21) # 20 seeds
n = [3, 6, 9]
tests = ['volume', 'entropy', 'random']
allTasks = list(product(n, seeds, tests)) # length: 180
myTasks = allTasks[startIndex:(startIndex+NRPROC)]

## Start processing in parallel
pool = multiprocessing.Pool(processes=NRPROC)
print "Making computational tests for instance ID", instId
r = pool.map_async(callScript, myTasks)
r.wait() # Wait on the results

