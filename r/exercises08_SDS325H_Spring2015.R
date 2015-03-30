library(mosaic)

## Problem 1

treat=c(203, 198, 145)
control = c(156, 221, 162)
mytab = t(cbind(treat,control))
chisq(mytab)
chisq.test(mytab)

mytest = do(10000)*{
	treat_rand = rmultinom(1,size=546,prob=c(359,419,307))
	control_rand = rmultinom(1,size=539,prob=c(359,419,307))
	sim.tab = t(cbind(treat_rand, control_rand))
	chisq(sim.tab)
}

hist(mytest$X.squared, 100, prob=TRUE)
curve(dchisq(x,2), add=TRUE, col='red')

sum(mytest$X.squared > chisq(mytab))/10000 


## Problem 2

n = 100

# Find critical values
lbound = qbinom(0.025, size=n, prob=0.5)
ubound = qbinom(0.975, size=n, prob=0.5)

sum(dbinom(40:60, size=100, prob=0.5))
sum(dbinom(41:59, size=100, prob=0.5))

# So we reject the null whenever X is outside (40,60)
# A quick sanity check by Monte Carlo
nmc = 1000
mysim = do(nmc)*{
	x_sim = rbinom(1, size=n, prob = 0.5)
	{x_sim < lbound || x_sim > ubound}
}
sum(mysim)/nmc


# Now the power calculation
w_grid = seq(0, 1, by=0.01)
power_grid = rep(0, length(w_grid))  # pre-allocate a vector to store the result

for(i in seq_along(w_grid)) {
	w = w_grid[i]
	mysim = do(nmc)*{
		x_sim = rbinom(1, size=n, prob = w)
		{x_sim < lbound || x_sim > ubound}
	}
	power_grid[i] = sum(mysim)/nmc
}

plot(w_grid, power_grid, type='b')
