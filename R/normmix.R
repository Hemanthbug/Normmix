#' Mixture of Two Normal Distributions
#'
#' @param x Numeric vector of values.
#' @param mean1 Mean of the first normal distribution.
#' @param sd1 Standard deviation of the first normal distribution.
#' @param mean2 Mean of the second normal distribution.
#' @param sd2 Standard deviation of the second normal distribution.
#' @param mixprob Mixture probability (weight for the first distribution, between 0 and 1).
#'
#' @return A numeric vector of cumulative probabilities.
#' @export
#'
#' @examples
#' pnormmix(1, 0, 1, 5, 1.5, 0.6)
pnormmix = function(
    x,mean1,sd1,mean2, sd2,mixprob)
{mixprob*pnorm(mean = mean1,sd = sd1)+
    (1-mixprob)*pnorm(mean=mean2,sd = sd2)}


#' Density Function for a Mixture of Two Normal Distributions
#'
#' @param x Numeric vector of values where the density is evaluated.
#' @param mean1 Mean of the first normal distribution.
#' @param sd1 Standard deviation of the first normal distribution.
#' @param mean2 Mean of the second normal distribution.
#' @param sd2 Standard deviation of the second normal distribution.
#' @param mixprob Mixture probability (weight for the first distribution, between 0 and 1).
#'
#' @return
#' @export
#'
#' @examples
#' dnormmix(1, 0, 1, 5, 1.5, 0.6)
dnormmix = function(
    x,mean1,sd1,mean2, sd2,mixprob)
{mixprob*dnorm(mean = mean1,sd = sd1)+
    (1-mixprob)*dnorm(mean=mean2,sd = sd2)}


#' Quantile Function for a Mixture of Two Normal Distributions
#'
#' @param p Numeric vector of probabilities (must be between 0 and 1).
#' @param mean1 Mean of the first normal distribution.
#' @param sd1 Standard deviation of the first normal distribution.
#' @param mean2 Mean of the second normal distribution.
#' @param sd2 Standard deviation of the second normal distribution.
#' @param mixprob Mixture probability (weight for the first distribution, between 0 and 1).
#' @param tol Tolerance for numerical computation (default is `1e-7`).
#' @param maxiter Maximum number of iterations for numerical methods (default is `100`).
#'
#' @return
#' @export
#'
#' @examples
#' qnormmix(0.5, 0, 1, 5, 1.5, 0.6)
#'
qnormmix = function(p,mean1,sd1,mean2,sd2,mixprob,tol =1e-7,maxiter = 100)
{
  if(any(p<0|p>1)){
    stop("p must be 0 and 1")
  }
}


#' Generate Random Samples from a Mixture of Two Normal Distributions
#'
#' @param n Number of random samples to generate.
#' @param mean1 Mean of the first normal distribution.
#' @param sd1 Standard deviation of the first normal distribution.
#' @param mean2 Mean of the second normal distribution.
#' @param sd2 Standard deviation of the second normal distribution.
#' @param mixprob Mixture probability (weight for the first distribution, between 0 and 1).
#'
#' @return
#' @export
#'
#' @examples
#' rnormmix(1000, 0, 1, 5, 1.5, 0.6)
rnormmix= function(n, mean1, sd1, mean2, sd2, mixprob){
  hist(r, breaks = 50, main = "Mixture Histogram")}
