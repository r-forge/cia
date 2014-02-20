#################################################################
## Compute the sensitivity of CIP for given weights
sensitivitylinearCIP = function(weights, normdata, covmethod = c('standard', 'robust')){
  if (missing(weights)){
    stop ('weights is missing')
  }
  weights = as.vector(weights)
  n = length(weights)
  if (missing(normdata)){
    stop ('normdata is missing')
  }
  normdata = as.matrix(normdata)
  tmp = dim(normdata)
  if (tmp[2] != n){
    stop ('dimension mismatch')
  }
  if (covmethod[1] == 'robust'){
    require(robustbase)
    Sigma = covMcd(normdata)$cov
  }
  else {
    Sigma = stats::cov(normdata, use = 'complete.obs')
  }
  
  # component weights
  sensitivity = as.vector((Sigma %*% weights) / sqrt(as.numeric(t(weights) %*% Sigma %*% weights)))
  component   = weights * sensitivity
  
  out = list(sensitivity = sensitivity, component = component, Sigma = Sigma)
  return(out)
}