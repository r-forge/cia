#################################################################
## Process control parameters
processControl = function(control = list()){
  if (!is.list(control) || length(control) == 0){
    control = list(method = NULL, 
                   aggregation = NULL,
                   normalization = NULL, 
                   weights = NULL,
                   missingindicator = NULL,
                   techclass = NULL)
  }
  nam = names(control)
  if (!("method" %in% nam) || is.null(control$method)){
    control$method = c('mixed-eight-indicators','eight-indicators' , 'four-indicators', 'six-indicators' )
  }
  if (!("aggregation" %in% nam) || is.null(control$aggregation)){
    control$aggregation = c('geometric' , 'linear' )
  }
  if (!("normalization" %in% nam) || is.null(control$normalization)){
    control$normalization = c('min-max', 'z-score', 'robust-z-score')
  }
  if (!("weights" %in% nam) || is.null(control$weights)){
    control$weights = c( 'method'  ) # alternative is also setting the weights
  }
  if (!("missingindicator" %in% nam) || is.null(control$missingindicator)){
    #control$missingindicator = c('last','none','reweight', 'linear')
    control$missingindicator = c('last', 'linear')    
  }
  if (!("techclass" %in% nam) || is.null(control$techclass)){
    control$techclass = c('OECD', 'Lall')
  }
  if (!("cleaning" %in% nam) || is.null(control$cleaning)){
    control$cleaning = c('none', 'hampel_twosided' , 'hampel_onesided')
  }  
  if( control$aggregation[1]=='geometric' &  control$normalization[1]!='min-max' ){
    control$normalization[1]='min-max'
    # problem with http://planetmath.org/NegativeNumber.html
  }
  return(control)
}

#################################################################
## Complete missing data
completeData = function(data, control = list()){
  ctr = processControl(control)
  if (ctr$missingindicator[1] == 'reweight' |  ctr$missingindicator[1] == 'none' ){
    return(data)
  }
  country  = sort(unique(as.character(data$country)))
  ncountry = length(country)
  n = ncol(data)
  # iterate over all country
  for (i in 1 : ncountry){
    seldata = data[data$country == country[i],]
    #seldata = droplevels(seldata)
    # determine if NaN are present in the data
    pos = order(as.character(seldata$year))
    # ordered indicators wrt year
    indicators = as.matrix(seldata[pos,3:n])
    indicators = completeIndicators(indicators, ctr)
    # kb
    seldata[pos,3:n] = indicators
    data[data$country == country[i],] = seldata
  }
  return(data)
}

completeIndicators = function(indicators, ctr) {
  indicators = as.matrix(indicators)
  # if reweight, keep dataset
  if (ctr$missingindicator[1] == 'reweight' |  ctr$missingindicator[1] == 'none' ){
    return(indicators)
  }
  # if no NaN detected, keep dataset
  if (!any(is.na(indicators) | is.nan(indicators))){
    return(indicators)
  }
  n = nrow(indicators)
  # NaN observed but no history
  if (n == 1){
    #cat ('NA observed for a single year; cannot complete missing data\n')
    return(indicators)
  }
  # NaN observed and history available; fill missing value depending on the method
  if (ctr$missingindicator[1] == 'last'){
    #for( i in 1:ncol(indicators)){
    #  indicators[,i] = interpNA(indicators[,i],method="before")
    #}
    counter = rep(0,length(indicators[1,]))
    maxstep = 20;
    for (t in 2 : n){
      pos = is.na(indicators[t,]) | is.nan(indicators[t,]) 
      if (any(pos)){
        counter[pos] = counter[pos]+1;   
        indicators[t,(pos & counter<=maxstep) ] = indicators[t-1,(pos & counter<=maxstep)]
        counter[!pos] = 0
        counter[ counter[pos]>maxstep] = 0; 
      }
    }
  }
  else{
    for( i in 1:ncol(indicators)){
      if( length(na.omit(indicators[,i]))>2 ){
        indicators[,i] = interpNA(indicators[,i],method="linear")        
      }
    }
    # fill in remaining missing ones 
    counter = rep(0,length(indicators[1,]))
    maxstep = 20;
    for (t in 2 : n){
      pos = is.na(indicators[t,]) | is.nan(indicators[t,]) 
      if (any(pos)){
        counter[pos] = counter[pos]+1;   
        indicators[t,(pos & counter<=maxstep) ] = indicators[t-1,(pos & counter<=maxstep)]
        counter[!pos] = 0
        counter[ counter[pos]>maxstep] = 0; 
      }
    }    
    #for (t in 2 : (n-1)){
    #  pos = is.na(indicators[t,]) || is.nan(indicators[t,])
    #  if (any(pos)){
    #    indicators[t,pos] = (indicators[t-1,pos] + indicators[t+1,pos])/2
    #  }
    #}
  }
  # replace remaining NA or NaN by unconditional average
  #M = matrix(data = apply(indicators, 2, mean, na.rm = TRUE), nrow = nrow(indicators), 
  #           ncol = ncol(indicators), byrow = TRUE)
  #pos = is.na(indicators) | is.nan(indicators)
  #indicators[pos] = M[pos]
  return(indicators)  
}

#################################################################
## Clean local outlying data data
cleanData = function(data, control = list()){
  ctr = processControl(control)
  if (ctr$cleaning[1] == 'none'){
    return(data)
  }
  country  = sort(unique(as.character(data$country)))
  ncountry = length(country)
  n = ncol(data)
  # iterate over all country
  for (i in 1 : ncountry){
    seldata = data[data$country == country[i],]
    #seldata = droplevels(seldata)
    # determine if NaN are present in the data
    pos = order(as.character(seldata$year))
    # ordered indicators wrt year
    indicators = as.matrix(seldata[pos,3:n])
    indicators = cleanIndicators(indicators, ctr)
    # kb
    seldata[pos,3:n] = indicators
    data[data$country == country[i],] = seldata    
  }
  return(data)
}

hampel_twosided = function (x, k=2, t0 = 3){
  # package pracma
  n <- length(x)
  y <- x
  L <- 1.4826
  for (i in (k + 1):(n - k)) {
    localseries = na.omit( x[(i - k):(i + k)] )
    if(length(localseries)>=3){
      x0 <- median(localseries)
      sigma <- L * median(abs(localseries - x0))      
      if(sigma!=0){
        if ( x[i]  > (x0+t0 * sigma)   ) {
          y[i] <- (x0+t0 * sigma)
        }     
        if ( x[i]  < (x0-t0 * sigma)   ) {
          y[i] <- (x0-t0 * sigma)
        }
      }
    }
  }
  return(y)
}
hampel_onesided = function (x, k=4, t0 = 3){
  # package pracma
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  for (i in (k + 1):(n )) {
    localseries = na.omit( x[(i - k):(i )] )
    if(length(localseries)>=3){
      x0 <- median(localseries)
      sigma <- L * median(abs(localseries - x0))      
      if(sigma!=0){
        if ( x[i]  > (x0+t0 * sigma)   ) {
          y[i] <- (x0+t0 * sigma)
        }     
        if ( x[i]  < (x0-t0 * sigma)   ) {
          y[i] <- (x0-t0 * sigma)
        }      
      }
    }
  }  
  return(y)
}

cleanIndicators = function(indicators, ctr) {
  indicators = as.matrix(indicators)
  # if reweight, keep dataset
  if (ctr$missingindicator[1] == 'none'){
    return(indicators)
  }
  # if no NaN detected, keep dataset
  if (!any(is.na(indicators) | is.nan(indicators))){
    return(indicators)
  }
  n = nrow(indicators)
  # NaN observed but no history
  if (n == 1){
    #cat ('NA observed for a single year; cannot complete missing data\n')
    return(indicators)
  }
  # NaN observed and history available; fill missing value depending on the method
  if (ctr$missingindicator[1] == 'hampel_twosided'){
    for( i in 1:ncol(indicators)){
      indicators[,i] = hampel_twosided(indicators[,i])
    }
  }
  else{
    for( i in 1:ncol(indicators)){
      indicators[,i] = hampel_onesided(indicators[,i])
    }
  }
  return(indicators)  
}