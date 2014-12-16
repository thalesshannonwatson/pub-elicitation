An assessment of the convergence rate of the simulation based on a long run (1M planes), where we partition the long chain into chunks of 1k, 2k, ..., 50k planes. For each chunk, we find the minimum entropy. This gives us between 1,000 entropies for 1k planes to 20 entropies for 50k planes, which gives some idea of the rate of convergence and the uncertainty around it.

The entropies were calculated on a cluster, in chunks of 10k planes each, after the series of 1M planes was pre-generated. This was done for problems of dimension 3 - 12, where we kept the measurement distributions stable between dimensions (so the problem of dimension 4 was a simple extension of the problem of dimension 3, etc.).

The scripts here do the following:

 - boxplot.R provides a raw boxplot of the samples, plus lines indicating 1.01 and 1.02 times the minimum entropy found in 1M iterations.

 - relative.R is an exploratory analysis looking at the mean entropy after N iterations as compared to the mean entropy after 50k iterations. Various transformations of N are tried to find a good linear approximation of the convergence curves. Using 1/x^(1/5) appears most promising. There is also an attempt to look at how the slope relates to the problem dimension. A linear relationship would appear to fit nicely.

 - regress.R formulates the above as a regression on both n and N, and plots predictions from the regression model over the boxplot seen earlier. Then there is an attempt to predict the number of iterations required to reach a certain target accuracy. (Is now ../figures/convergence.R)
