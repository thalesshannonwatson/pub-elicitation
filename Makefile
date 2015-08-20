SEED=1911
N=5
K=2

all: ../graphics/distfig.pdf ../graphics/thrombo-voluprob.pdf ../graphics/thrombo-restricted.pdf ../graphics/uc-boxplot.pdf ../graphics/rnd-plot.pdf ../graphics/convergence.pdf ../graphics/example.pdf

.PHONY: all unittest

## Below here the final tests ##

lib/elicitation.so:
	R CMD SHLIB lib/elicitation.c

data/timing-test.rds: run/timing-test.R lib/elicitation.so lib/code.R lib/rnd-problem.R
	R --vanilla --file=$< --args $@ ${N} ${SEED}

data/convergence-planes.n%.rds: run/convergence-planes.R lib/code.R lib/rnd-beta.R
	R --vanilla --file=$< --args $@ $*

data/convergence-entropies.rds: run/convergence-entropies.R lib/code.R lib/elicitation.so
	R --vanilla --file=$< --args $@ ${N} ${INST}

data/compare-%.rds: run/compare-%.R lib/compare-common.R lib/elicitation.so
	R --vanilla --file=$< --args $@ ${N} ${SEED}

schedule-timing:
	qsub -d . schedule-timing.pbs

schedule-convergence-planes:
	qsub -d . schedule-convergence-planes.pbs

schedule-convergence-entropies:
	qsub -d . schedule-convergence-entropies.pbs

schedule-compare:
	qsub -d . schedule-compare.pbs

data/thrombo-voluprob.rds: run/thrombo-opts.R data/thrombo-problem.rds

data/thrombo-restricted.rds: run/thrombo-opts.R data/thrombo-problem.rds

## Here end the final tests ##

data/%.rds: run/%.R lib/elicitation.so
	R --vanilla --file=$< --args $@ ${SEED}

data/convergence.n%.rds: run/convergence.R lib/code.R lib/rnd-problem.R
	R --vanilla --file=$< --args $@ $* ${SEED}

data/thrombo-voluprob.seed.%.rds: run/thrombo-voluprob.R lib/elicitation.so
	R --vanilla --file=$< --args $@ $*

data/random-restricted.rds: run/random-restricted.R lib/elicitation.so
	R --vanilla --file=$< --args $@ ${SEED} ${N} ${K}

data/thrombo-equiprob.rds: run/thrombo-opts.R data/thrombo-problem.rds

../graphics/%.pdf: figures/%.R
	R --vanilla --file=$< --args $@ || rm $@

../graphics/heatmap.pdf: figures/heatmap.R lib/simplex.heat.R
	R --vanilla --file=$< --args $@ ${SEED} || rm $@

../graphics/thrombo-equiprob.pdf: data/thrombo-equiprob.seed.1.rds lib/plots.R
../graphics/thrombo-voluprob.pdf: data/thrombo-voluprob.seed.1.rds data/thrombo-voluprob.seed.2.rds lib/plots.R
../graphics/thrombo-restricted.pdf: data/thrombo-restricted.rds lib/plots.R

unittest:
	R --vanilla --file=unittest/unittests.R

schedule-convergence:
	qsub -d . schedule-convergence.pbs

schedule-thrombo: data/thrombo-problem.rds
	qsub -d . schedule.pbs

schedule-thrombo-rests: data/thrombo-problem.rds
	qsub -d . schedule-restr-single.pbs

schedule-thrombo-equi: data/thrombo-problem.rds
	qsub -d . schedule-equi.pbs

schedule-rand:
	qsub -d . schedule-rand.pbs

