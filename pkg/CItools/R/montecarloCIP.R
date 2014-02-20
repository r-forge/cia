#################################################################
## Monte Carlo analysis of CIP

montecarloCIP = function(data, year, nboot = 250, control = list()) {
  ctr = processControl(control)
  if (missing(data))
    stop('data is missing')
  if (missing(year))
    stop('year is missing')
  
  cip = computeCIP(data, year = year)
  country  = rownames(cip$index)
  ncountry = length(country)
  
  indexMC   = matrix(data = NA, nrow = ncountry, ncol = nboot)
  rankingMC = matrix(data = NA, nrow = ncountry, ncol = nboot)
  for (m in 1 : nboot) {
    dft = processControl()
    rndctr = ctr
    # randomize method
    if (ctr$method[1] == 'random'){
      rndctr$method = dft$method[sample(1:length(dft$method), 1)]
    }
    # randomize aggregation
    if (ctr$aggregation[1] == 'random'){
      rndctr$aggregation = dft$aggregation[sample(1:length(dft$aggregation), 1)]
    }
    # randomize normalization
    if (ctr$normalization[1] == 'random'){
      rndctr$normalization = dft$normalization[sample(1:length(dft$normalization), 1)]
    }
    # randomize missing data technique
    if (ctr$missingindicator[1] == 'random'){
      rndctr$missingindicator = dft$missingindicator[sample(1:length(dft$missingindicator), 1)]
    }
    # randomize technology classification
    if (ctr$techclass[1] == 'random'){
      rndctr$techclass = dft$techclass[sample(1:length(dft$techclass), 1)]
    }
    # randomize cleaning
    if (ctr$cleaning[1] == 'random'){
      rndctr$cleaning = dft$cleaning[sample(1:length(dft$techclass), 1)]
    }    
    # randomize weights
    if ( ctr$weights[1] == 'random' ){
      rndctr$weights = rep(0, 9 ) 
      dnames = colnames(data[,-c(1:2)])
      vnames = c("MVApc", "MXpc", "MHVAsh", "MVAsh", "MHXsh", "MXsh", "ImWMVA", "ImWMT", "MHVAsh_Lall",
                 "INnt" , "MXq" )
      if (rndctr$method[1] == 'mixed-eight-indicators') {
         dnames = c(dnames,"INnt" , "MXq") 
      }
      if (rndctr$method[1] == 'four-indicators'){
        if (rndctr$techclass[1] == 'OECD'){
          # CIP.4 = ( MVApc + MXpc + MHVAsh + MHXsh) / 4
          pos = match(vnames[c(1,2,3,5)], dnames)
        }
        else if (rndctr$techclass[1] == 'Lall'){
          # CIP.4 = ( MVApc + MXpc + MHVAsh_Lall + MHXsh) / 4
          pos = match(vnames[c(1,2,9,5)], dnames)
        }
        else{
          stop('control$techclass not well defined')
        }
        if (any(is.na(pos))){
          stop('wrong variable names in data for control$method == four-indicators')
        }
        o = runif(4) ; rndctr$weights[pos] = o/sum(o) 
      }
      else if (rndctr$method[1] == 'six-indicators') {
        if (rndctr$techclass[1] == 'OECD'){
          # CIP.6 = ( MVApc + MXpc + [MHVAsh + MVAsh]/2 + [MHXsh + MXsh]/2) / 4
          pos = match(vnames[1:6], dnames)
        }
        else if (rndctr$techclass[1] == 'Lall'){
          # CIP.6 = ( MVApc + MXpc + [MHVAsh_Lall + MVAsh]/2 + [MHXsh + MXsh]/2) / 4
          pos = match(vnames[c(1,2,9,4,5,6)], dnames)
        }
        else{
          stop('control$techclass not well defined')
        }
        if (any(is.na(pos))){
          stop('wrong variable names in data for control$method == six-indicators')
        }
        o = runif(6) ; rndctr$weights[pos] = o/sum(o)
      } 
      else if (rndctr$method[1] == 'eight-indicators') {
        if (rndctr$techclass[1] == 'OECD'){
          # CIP.8 = (MVApc + MXpc + [MHVAsh + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
          pos = match(vnames[1:8], dnames)
        }
        else if (rndctr$techclass[1] == 'Lall'){
          # CIP.8 = (MVApc + MXpc + [MHVAsh_Lall + MVAsh]/2 + [MHXsh + MXsh] + ImWMVA + ImWMT) / 6
          pos = match(vnames[c(1,2,9,4,5,6,7,8)], dnames)
        }
        else{
          stop('control$techclass not well defined')
        }
        if (any(is.na(pos))){
          stop('wrong variable names in data for control$method == eight-indicators')
        }
        o = runif(8) ; rndctr$weights[pos] = o/sum(o)
      }
      else if (rndctr$method[1] == 'mixed-eight-indicators') {
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
        o = runif(6) ; rndctr$weights[pos] = o/sum(o)
      }      
      else {
        stop('control$method not well defined')
      }      
    }
    # complete the dataset if randomization on missing data technique
    tmp = computeCIP(data, year, rndctr)
    indexMC[,m]   = tmp$index
    rankingMC[,m] = tmp$ranking
  }
  
  dimnames(indexMC) = dimnames(rankingMC) = list(country = country, 1:nboot)
  list(cip = cip, indexMC = indexMC, rankingMC = rankingMC)
}
#montecarloCIP = cmpfun(montecarloCIP)

