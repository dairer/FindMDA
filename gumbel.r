library(extRemes)
library(ggplot2)
set.seed(5)

compareGEV.GUMBEL(rexp(100000, rate = 3), 'Expenential Distribution') # exponential is in MDA of Gumble
compareGEV.GUMBEL(rweibull(100000, shape=1, scale = 1), 'Weibull Distribution') # Weibull is in MDA of Gumble
compareGEV.GUMBEL(rcauchy(100000,  location = 1, scale = 1), 'Cauchy Distribution') # Cauchy is not in MDA of Gumble
compareGEV.GUMBEL(runif(100000,  min = 0, max = 1), 'Uniform Distribution') # Uniform is not in MDA of Gumble


compareGEV.GUMBEL <- function(mydata, orig.dist = 'Original Distribution'){
  # take 20000 samples of size 100 from mydata and store the max of each sample
  my_maxima = replicate(20000, max(sample(mydata, size = 100)))
  
  # fit GEV
  f.gev = fevd(my_maxima, type = 'GEV', method = "MLE")
  gev.mu = as.numeric(f.gev$results$par[1])
  gev.sigma = as.numeric(f.gev$results$par[2])
  gev.xi = as.numeric(f.gev$results$par[3])
  
  # fit Gumbel
  f.gumbel = fevd(my_maxima, type = 'Gumbel', method = "MLE")
  gum.mu = as.numeric(f.gumbel$results$par[1])
  gum.sigma = as.numeric(f.gumbel$results$par[2])
  gum.xi = 0
  
  # calculate Quantiles for each fit
  n = length(my_maxima)
  probabilities = (1:n)/(n+1)
  
  qq.data <- data.frame(sort(qgev(probabilities, gev.mu,  gev.sigma, gev.xi)),
                        sort(qgev(probabilities, gum.mu,  gum.sigma, gum.xi)),.name_repair = 'minimal')
  
  names(qq.data) <- c('qq.GEV','qq.GUMBEL')
  
  # plot against each other, if MDE = GUmbel, should lie on straight diagonal
  ggplot()+
    geom_point(data =qq.data, aes(qq.GEV, qq.GUMBEL), alpha = 0.4)+
    geom_abline(intercept=0, slope=1) +
    ggtitle(paste0('QQ plot of Gumbel and GEV fitted to maxima of ', orig.dist))+
    theme_minimal()
}
