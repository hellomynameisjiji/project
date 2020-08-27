set.seed(2017311195)

y <- seq(-7,10,.02)
dens <- 0.5*dnorm(y,1,2) + 0.5*dnorm(y,2,2)
plot (y, dens, ylim=c(0,1.1*max(dens)),
  type="l", xlab="y", ylab="", xaxs="i",
  yaxs="i", yaxt="n", bty="n", cex=2)



# Ch 2


#' title: "Bayesian data analysis demo 2.1"

#' author: "Aki Vehtari, Markus Paasiniemi"

#' date: "`r format(Sys.Date())`"

#' ---




#' ## Probability of a girl birth given placenta previa (BDA3 p. 37).

#' 

#' 437 girls and 543 boys have been observed. Calculate and plot the

#' posterior distribution of the proportion of girls $\theta$, using

#' uniform prior on $\theta$.

#'




#' ggplot2 is used for plotting

#' to install new packages, type e.g. install.packages('ggplot2')

#+ setup, message=FALSE, error=FALSE, warning=FALSE

library(ggplot2)




#' Posterior is Beta(348,544)

# seq creates evenly spaced values

df1 <- data.frame(theta = seq(0.375, 0.525, 0.001)) 

a <- 438

b <- 544

# dbeta computes the posterior density

df1$p <- dbeta(df1$theta, a, b)




#' compute also 95% central interval

# seq creates evenly spaced values from 2.5% quantile

# to 97.5% quantile (i.e., 95% central interval)

# qbeta computes the value for a given quantile given parameters a and b

df2 <- data.frame(theta = seq(qbeta(0.025, a, b), qbeta(0.975, a, b), length.out = 100))

# compute the posterior density

df2$p <- dbeta(df2$theta, a, b)




#' Plot posterior (Beta(438,544))

ggplot(mapping = aes(theta, p)) +

  geom_line(data = df1) +

  # Add a layer of colorized 95% posterior interval

  geom_area(data = df2, aes(fill='1')) +

  # Add the proportion of girl babies in general population

  geom_vline(xintercept = 0.485, linetype='dotted') +

  # Decorate the plot a little

  labs(title='Uniform prior -> Posterior is Beta(438,544)', y = '') +

  scale_y_continuous(expand = c(0, 0.1), breaks = NULL) +

  scale_fill_manual(values = 'darkblue', labels = '95% posterior interval') +

  theme(legend.position = 'bottom', legend.title = element_blank())



#' ---

#' title: "Bayesian data analysis demo 2.2"

#' author: "Aki Vehtari, Markus Paasiniemi"

#' date: "`r format(Sys.Date())`"

#' ---




#' ## Probability of a girl birth given placenta previa (BDA3 p. 37)

#'

#' Illustrate the effect of prior and compare posterior distributions

#' with different parameter values for the beta prior distribution.

#' 




#' ggplot2 is used for plotting, tidyr for manipulating data frames

#+ setup, message=FALSE, error=FALSE, warning=FALSE

library(ggplot2)

library(tidyr)




#' Observed data: 437 girls and 543 boys

a <- 437

b <- 543




#' Evaluate densities at evenly spaced points between 0.375 and 0.525

df1 <- data.frame(theta = seq(0.375, 0.525, 0.001))




#' Posterior with Beta(1,1), ie. uniform prior

df1$pu <- dbeta(df1$theta, a+1, b+1)




#' 3 different choices for priors

#'

#' - Beta(0.485\*2,(1-0.485)\*2)

#' - Beta(0.485\*20,(1-0.485)\*20)

#' - Beta(0.485\*200,(1-0.485)\*200)

n <- c(2, 20, 200) # prior counts

apr <- 0.485 # prior ratio of success




# helperf returns for given number of prior observations, prior ratio

# of successes, number of observed successes and failures and a data

# frame with values of theta, a new data frame with prior and posterior

# values evaluated at points theta.

helperf <- function(n, apr, a, b, df)

  cbind(df, pr = dbeta(df$theta, n*apr, n*(1-apr)), po = dbeta(df$theta, n*apr + a, n*(1-apr) + b), n = n)

# lapply function over prior counts n and gather results into key-value pairs.

df2 <- lapply(n, helperf, apr, a, b, df1) %>% do.call(rbind, args = .) %>%

  gather(grp, p, -c(theta, n), factor_key = T)

# add correct labels for plotting

df2$title <- factor(paste0('alpha/(alpha+beta)=0.485, alpha+beta=',df2$n))

levels(df2$grp) <- c('Posterior with unif prior', 'Prior', 'Posterior')




#' Plot distributions

ggplot(data = df2) +

  geom_line(aes(theta, p, color = grp)) +

  geom_vline(xintercept = 0.485, linetype = 'dotted') +

  facet_wrap(~title, ncol = 1) +

  labs(x = '', y = '') +

  scale_y_continuous(breaks = NULL) +

  theme(legend.position = 'bottom', legend.title = element_blank())

#' ---

#' title: "Bayesian data analysis demo 2.3"

#' author: "Aki Vehtari, Markus Paasiniemi"

#' date: "`r format(Sys.Date())`"

#' ---




#' ## Probability of a girl birth given placenta previa (BDA3 p. 37).

#' 

#' Simulate samples from Beta(438,544), draw a histogram with

#' quantiles, and do the same for a transformed variable.

#' 




#' ggplot2 is used for plotting, tidyr for manipulating data frames

#+ setup, message=FALSE, error=FALSE, warning=FALSE

library(ggplot2)

library(tidyr)




#' Sample from posterior Beta(438,544).

#' Obtain all draws at once and store them in vector 'theta'

a <- 438

b <- 544

theta <- rbeta(10000, a, b)

#' Compute odds ratio for all draws

phi <- (1 - theta) / theta




#' Compute 2.5% and 97.5% quantile approximation using samples

quantiles <- c(0.025, 0.975)

thetaq <- quantile(theta, quantiles)

phiq <- quantile(phi, quantiles)




#' Histogram plots with 30 bins for theta and phi

# merge the data into one data frame for plotting

df1 <- data.frame(phi,theta) %>% gather()

# merge quantiles into one data frame for plotting

df2 <- data.frame(phi = phiq, theta = thetaq) %>% gather()

ggplot() +

  geom_histogram(data = df1, aes(value), bins = 30) +

  geom_vline(data = df2, aes(xintercept = value), linetype = 'dotted') +

  facet_wrap(~key, ncol = 1, scales = 'free_x')  +

  labs(x = '', y = '') +

  scale_y_continuous(breaks = NULL)
#' ---

#' title: "Bayesian data analysis demo 2.4"

#' author: "Aki Vehtari, Markus Paasiniemi"

#' date: "`r format(Sys.Date())`"

#' ---




#' ## Probability of a girl birth given placenta previa (BDA3 p. 37).

#' 

#' Calculate the posterior distribution on a discrete grid of points by

#' multiplying the likelihood and a non-conjugate prior at each point,

#' and normalizing over the points. Simulate samples from the resulting

#' non-standard posterior distribution using inverse cdf using the

#' discrete grid.

#' 




#' ggplot2 and gridExtra are used for plotting, tidyr for manipulating data frames

#+ setup, message=FALSE, error=FALSE, warning=FALSE

library(ggplot2)

library(gridExtra)

library(tidyr)




#' ### Evaluating posterior with non-conjugate prior in grid

#' 

#' Posterior with observations (437,543) and uniform prior (Beta(1,1))

a <- 437

b <- 543

#' Evaluate densities at evenly spaced points between 0.1 and 1

df1 <- data.frame(theta = seq(0.1, 1, 0.001))

df1$con <- dbeta(df1$theta, a, b)




#' Compute the density of non-conjugate prior in discrete points, i.e. in a grid

#' this non-conjugate prior is the same as in figure 2.4 in the book

pp <- rep(1, nrow(df1))

pi <- sapply(c(0.385, 0.485, 0.585), function(pi) which(df1$theta == pi))

pm <- 11

pp[pi[1]:pi[2]] <- seq(1, pm, length.out = length(pi[1]:pi[2]))

pp[pi[3]:pi[2]] <- seq(1, pm, length.out = length(pi[3]:pi[2]))




#' normalize the prior

df1$nc_p <- pp / sum(pp)




#' compute the un-normalized non-conjugate posterior in a grid

po <- dbeta(df1$theta, a, b) * pp




#' normalize the posterior

df1$nc_po <- po / sum(po)







#' Plot posterior with uniform prior, non-conjugate

#' prior and the corresponding non-conjugate posterior

# gather the data frame into key-value pairs

# and change variable names for plotting

df2 <- gather(df1, grp, p, -theta, factor_key = T) #%>%

levels(df2$grp) <- c('Posterior with uniform prior',

                     'Non-conjugate prior', 'Non-conjugate posterior')

ggplot(data = df2) +

  geom_line(aes(theta, p)) +

  facet_wrap(~grp, ncol = 1, scales = 'free_y') +

  coord_cartesian(xlim = c(0.35,0.6)) +

  scale_y_continuous(breaks=NULL) +

  labs(x = '', y = '')




#' ### Inverse cdf sampling

#' 

#' compute the cumulative density in a grid

df1$cs_po <- cumsum(df1$nc_po)




#' Sample from uniform distribution U(0,1)

# set.seed(seed) is used to set seed for the randon number generator

set.seed(2601)

# runif(k) returns k uniform random numbers from interval [0,1]

r <- runif(10000)




#' Inverse-cdf sampling

# function to find the value smallest value theta at which the cumulative

# sum of the posterior densities is greater than r.

invcdf <- function(r, df) df$theta[sum(df$cs_po < r) + 1]

# sapply function for each sample r. The returned values s are now

# random draws from the distribution.

s <- sapply(r, invcdf, df1)




#' Create three plots: p1 is the posterior, p2 is the cdf of the posterior

#' and p3 is the histogram of posterior samples (drawn using inv-cdf)

p1 <- ggplot(data = df1) +

  geom_line(aes(theta, nc_po)) +

  coord_cartesian(xlim = c(0.35, 0.6)) +

  labs(title = 'Non-conjugate posterior', x = '', y = '') +

  scale_y_continuous(breaks = NULL)

p2 <- ggplot(data = df1) +

  geom_line(aes(theta, cs_po)) +

  coord_cartesian(xlim = c(0.35, 0.6)) +

  labs(title = 'Posterior-cdf', x = '', y = '') +

  scale_y_continuous(breaks = NULL)

p3 <- ggplot() +

  geom_histogram(aes(s), binwidth = 0.003) +

  coord_cartesian(xlim = c(0.35, 0.6)) +

  labs(title = 'Histogram of posterior samples', x = '', y = '') +

  scale_y_continuous(breaks = NULL)

# combine the plots

grid.arrange(p1, p2, p3)
