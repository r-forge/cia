#################################################################
## Compute the CIP for a given dataset and control argument
computeCIP = function(data, year = NULL, control = list()){
  # inputs checking
  if (missing(data))
    stop('data is missing')
  if (!is.data.frame(data))
    stop('data must be a dataframe')
  hdr = colnames(data)
  if (is.null(hdr))
    stop('data must have colnames')
  
  # process control
  ctr = processControl(control)
  
  # complete the data
  data = completeData(data, ctr)

  # clean the data
  data = cleanData(data, ctr)
  
  # take a subset of data if year provided
  if (!is.null(year)){
    pos = !is.na(match(as.character(data$year), as.character(year)))
    if (!any(pos)){
      stop('no year corresponding to year in data')
    }
    data = data[pos,]
    #data = droplevels(data)
  }
  
  # retrive country names and years
  country  = sort(unique(as.character(data$country)))
  ncountry = length(country)
  year     = sort(unique(as.character(data$year)))
  nyear    = length(year)
  
  # initialization
  index = ranking = matrix(data = NA, nrow = ncountry, ncol = nyear)
  dimnames(index) = dimnames(ranking) = list(country = country, year = year)
  yearlist = list()
  
  # compute CIP for each year
  for (i in 1 : nyear){
    tmp = computeYearlyCIP(data, year[i], ctr)
    pos = match(rownames(tmp$normindicators), country) # position of country in storage matrix
    index[pos,i]   = tmp$index
    ranking[pos,i] = tmp$ranking
    yearlist[[i]]  = list(year = year[i], index = tmp$index, ranking = tmp$ranking, 
                          indicators = tmp$indicators, normindicators = tmp$normindicators, weights = tmp$weights)
  }
  
  # change in ranking if more than two years
  changeinranking = NULL
  if (nyear > 1){
    tmp = apply(ranking[, seq(from = nyear, by = -1, to = 1)], 1, diff)
    changeinranking = matrix(data = tmp, nrow = ncountry, ncol = nyear - 1, byrow = FALSE)
    dimnames(changeinranking) = list(country = country, year = year[2:nyear])
  }
  
  out = list(index = index, ranking = ranking, changeinranking = changeinranking, yearlist = yearlist)
  return(out)
}
#computeCIP = cmpfun(computeCIP)

#################################################################
## Compute the CIP for a given year
computeYearlyCIP = function(data, year, ctr){
  # subset of data for the selected years
  seldata = data[as.character(data$year) == as.character(year),]
  #seldata = droplevels(seldata)
  
  # determine country for the year
  country  = as.character(seldata$country)
  ncountry = length(country)
  if (length(unique(country)) != ncountry){
    stop('several time same country for same year')
  }
  
  # select indicators
  indicators = seldata[,3:ncol(seldata)]
  rownames(indicators) = country
  
  # normalization of indicators
  normindicators = normalizeIndicators(indicators, ctr)
  
 
  
  if (ctr$method[1] == 'mixed-eight-indicators'){
    MXq = (normindicators[,"MHXsh"] +   normindicators[,"MXsh"])/2
    if (ctr$techclass[1] == 'OECD'){
      # CIP.8 = (MVApc + MXpc + [MHVAsh + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
       INnt = (normindicators[,"MHVAsh"] +   normindicators[,"MVAsh"])/2
    }
    else if (ctr$techclass[1] == 'Lall'){
      # CIP.8 = (MVApc + MXpc + [MHVAsh_Lall + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
      INnt = (normindicators[,"MHVAsh"] +   normindicators[,"MHVAsh_Lall"])/2
    }
    normindicators = cbind(normindicators,INnt,MXq)
    colnames(normindicators) =  c("MVApc", "MXpc", "MHVAsh", "MVAsh", "MHXsh", "MXsh", "ImWMVA", "ImWMT", "MHVAsh_Lall",
                                  "INnt" , "MXq" )
  }
  #if (any(is.na(normindicators)) || any(is.nan(normindicators))){
  #  if (ctr$missingindicator[1] != 'reweight'){
  #    #cat('some indicators are NaN; set control$missingindicator to reweight\n')
  #    ctr$missingindicator = 'reweight'
  #  }
  #}
  
  # weights for indicators
  weights = computeWeights(normindicators, ctr)
  
  # aggregation of indicators
  pos = colSums(weights)>0
  # avoid that NA of indicator not in definition propagates
  
  if (ctr$aggregation[1] == 'linear'){
    if(ctr$missingindicator[1] == 'reweight'){
      index = apply(normindicators[,pos] * weights[,pos], 1, sum, na.rm = TRUE)
    }else{
      index = apply(normindicators[,pos] * weights[,pos], 1, sum, na.rm = FALSE)
    } 
  }
  else if (ctr$aggregation[1] == 'geometric'){
    if(ctr$missingindicator[1] == 'reweight'){
      index = apply(normindicators[,pos]^weights[,pos], 1, prod, na.rm = TRUE)
    }else{
      index = apply(normindicators[,pos]^weights[,pos], 1, prod, na.rm = FALSE)
    }        
  }
  else{
    stop('control$method not well defined')
  }
  ranking = rank(-index, na.last = "keep")
  #index = round(index,4)
  out = list(index = index, ranking = ranking, indicators = indicators , 
             normindicators = normindicators, weights = weights)
  return(out)
}


#################################################################
## Normalization of the indicator variables
normalizeIndicators = function(indicators, ctr){
  indicators = as.matrix(indicators)
  tmp = dim(indicators)
  m = tmp[1]
  n = tmp[2]
  
  if (ctr$normalization[1] == "min-max"){
    # DA we could maybe speedup with sweep instead
    min = matrix(data = apply(indicators, 2, min, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    max = matrix(data = apply(indicators, 2, max, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    out = (indicators - min) / (max - min)
  }
  else if (ctr$normalization[1] == "z-score"){
    mu  = matrix(data = apply(indicators, 2, mean, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    sig = matrix(data = apply(indicators, 2, sd, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    out = (indicators - mu) / sig
    out[out>3] = 3;    out[out<(-3)] = -3;
  }
  else if (ctr$normalization[1] == "robust-z-score"){
    mu  = matrix(data = apply(indicators, 2, median, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    sig = matrix(data = apply(indicators, 2, mad, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    out = (indicators - mu) / sig;
    out[out>3] = 3;    out[out<(-3)] = -3;    
  } 
  else {
    stop('control$normalization not well defined')
  }
  # set NaN where infinite value are observed
  pos = is.infinite(indicators)
  out[pos] = NaN
  return(out)
}

#################################################################
## Compute a matrix of weights of the same dimension than indicators
computeWeights = function(indicators, ctr){
  indicators = as.matrix(indicators)
  tmp = dim(indicators)
  m = tmp[1]
  n = tmp[2]
  weights = matrix(data = 0, nrow = m, ncol = n)
  dimnames(weights) = dimnames(indicators)
  
  if ( (ctr$weights[1]=="method")|(ctr$weights[1]=="random")  ) {
    dnames = colnames(indicators)
    vnames = c("MVApc", "MXpc", "MHVAsh", "MVAsh", "MHXsh", "MXsh", "ImWMVA", "ImWMT", "MHVAsh_Lall",
               "INnt" , "MXq" )
    if (ctr$method[1] == 'four-indicators'){
      if (ctr$techclass[1] == 'OECD'){
        # CIP.4 = ( MVApc + MXpc + MHVAsh + MHXsh) / 4
        pos = match(vnames[c(1,2,3,5)], dnames)
      }
      else if (ctr$techclass[1] == 'Lall'){
        # CIP.4 = ( MVApc + MXpc + MHVAsh_Lall + MHXsh) / 4
        pos = match(vnames[c(1,2,9,5)], dnames)
      }
      else{
        stop('control$techclass not well defined')
      }
      if (any(is.na(pos))){
        stop('wrong variable names in data for control$method == four-indicators')
      }
      if(ctr$weights[1]=="method"){ 
        o = rep(0.25,4)
       }else{  o = runif(4) ; weights[,pos] = o/sum(o) } 
       weights[,pos] = matrix(data = o, nrow = m, ncol = 4, byrow = TRUE)
    }
    else if (ctr$method[1] == 'six-indicators') {
      if (ctr$techclass[1] == 'OECD'){
        # CIP.6 = ( MVApc + MXpc + [MHVAsh + MVAsh]/2 + [MHXsh + MXsh]/2) / 4
        pos = match(vnames[1:6], dnames)
      }
      else if (ctr$techclass[1] == 'Lall'){
        # CIP.6 = ( MVApc + MXpc + [MHVAsh_Lall + MVAsh]/2 + [MHXsh + MXsh]/2) / 4
        pos = match(vnames[c(1,2,9,4,5,6)], dnames)
      }
      else{
        stop('control$techclass not well defined')
      }
      if (any(is.na(pos))){
        stop('wrong variable names in data for control$method == six-indicators')
      }
      if(ctr$weights[1]=="method"){ 
        o = c(0.25, 0.25, 0.125, 0.125, 0.125, 0.125)
      }else{  o = runif(6) ; weights[,pos] = o/sum(o) } 
      weights[,pos] = matrix(data = o, nrow = m, ncol = 6, byrow = TRUE) 
    } 
    else if (ctr$method[1] == 'eight-indicators') {
      if (ctr$techclass[1] == 'OECD'){
        # CIP.8 = (MVApc + MXpc + [MHVAsh + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
        pos = match(vnames[1:8], dnames)
      }
      else if (ctr$techclass[1] == 'Lall'){
        # CIP.8 = (MVApc + MXpc + [MHVAsh_Lall + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
        pos = match(vnames[c(1,2,9,4,5,6,7,8)], dnames)
      }
      else{
        stop('control$techclass not well defined')
      }
      if (any(is.na(pos))){
        stop('wrong variable names in data for control$method == eight-indicators')
      }
      if(ctr$weights[1]=="method"){ 
        o = c(1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6)
      }else{  o = runif(8) ; weights[,pos] = o/sum(o) } 
      weights[,pos] = matrix(data = o, nrow = m, ncol = 8, byrow = TRUE)
    }
    else if(ctr$method[1] == 'mixed-eight-indicators') {
      if (ctr$techclass[1] == 'OECD'){
        # CIP.8 = (MVApc + MXpc + [MHVAsh + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
        #c("MVApc", "MXpc", "MHVAsh", "MVAsh", "MHXsh", "MXsh", "ImWMVA", "ImWMT", "MHVAsh_Lall",
        #  "INnt" , "MXq" )
        pos = match(vnames[c(1:2,10,11,7:8)], dnames)
      }
      else if (ctr$techclass[1] == 'Lall'){
        # CIP.8 = (MVApc + MXpc + [MHVAsh_Lall + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
        pos = match(vnames[c(1:2,10,11,7:8)], dnames)
      }
      else{
        stop('control$techclass not well defined')
      }
      if (any(is.na(pos))){
        stop('wrong variable names in data for control$method == mixed eight-indicators')
      }
      if(ctr$weights[1]=="method"){ 
        o = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
      }else{  o = runif(6) ; weights[,pos] = o/sum(o) } 
      weights[,pos] = matrix(data = o, nrow = m, ncol = 6, byrow = TRUE)      
    }
    else {
      stop('control$method not well defined')
    }
  }
  else {
    if (length(ctr$weights) != n) {
      stop('dimension of weights must match dimension of indicators')
    }
    weights = matrix(data = ctr$weights, nrow = m, ncol = n, byrow = TRUE)
  }
  
  # reweighting scheme
  if (ctr$missingindicator[1] == 'reweight') {
    idx = is.nan(indicators) | is.nan(indicators)
    weights[idx] = 0
    # reweight
    weights = sweep(x = weights, MARGIN = 1, STATS = apply(weights, 1, sum), FUN = '/')
  }
  return(weights)
}