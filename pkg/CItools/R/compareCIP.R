#################################################################
## Compare CIP between K countries
## Criterion 1: MeanIndexDifference: see formula 2 attached paper
## Criterion 2: MeanAbsRankDifference : see formula 3 attached paper

compareCIP = function(cip, bench = NULL){
  if (!is.list(cip))
    stop ('cip must be a list')
  if (!any(match(names(cip), 'yearlist')))
    stop ('yearlist must be present in cip')
  if (!is.list(cip$yearlist))
    stop ('yearlist must be a list')
  
  nlist    = length(cip$yearlist)
  country  = rownames(cip$ranking)
  ncountry = length(country)
  
  # aggregated measure over years
  IndexDifference = array(data = NA, c(ncountry, ncountry, nlist))
  dimnames(IndexDifference) = list(country = country, country = country, year = 1:nlist)
  
  # D_AB measure over time (difference in values of the composite indicator)
  for (t in 1 : nlist){
    cipt = cip$yearlist[[t]]
    tmpcip   = cip$yearlist[[t]]$index
    #
    #countryt = rownames(cipt$normindicators)
    #ncountryt = length(countryt)
    countryt = names(tmpcip)
    ncountryt = length(countryt)    
    for (i in 1 : (ncountryt-1)) {
      countryti = countryt[i]
      posit = which(countryt == countryti)
      posi  = which(country == countryti)
      for (j in ((i+1) : ncountryt)) {
        countrytj = countryt[j]
        posjt = which(countryt == countrytj)
        posj  = which(country == countrytj)
        if (length(posit == 1) && length(posjt == 1)){
          #tmpi = cipt$normindicators[posit,] * cipt$weights[posit,]
          #tmpj = cipt$normindicators[posjt,] * cipt$weights[posjt,] 
          tmpi = tmpcip[posit] 
          tmpj = tmpcip[posjt] 
          # store in big matrix
          # IndexDifference[posi,posj,t] = sum(tmpi - tmpj, na.rm = TRUE)
          IndexDifference[posi,posj,t] = tmpi - tmpj
        }
      }
    }
  }
  AvIndexDifference = round(apply(IndexDifference, c(1,2), mean, na.rm = TRUE),4)
  
  # RS measure (if bench available)
  AbsRankDifference = AvAbsRankDifference = AvRankCorrelation = NULL
  if (!is.null(bench)){
    if (!is.list(bench))
      stop ('bench must be a list')
    if (!any(match(names(bench), 'yearlist')))
      stop ('yearlist must be present in bench')
    if (!is.list(bench$yearlist))
      stop ('yearlist must be a list')
    AbsRankDifference = rep(NaN, nlist)
    RankCorrelation = rep(NaN, nlist)
    tmpbench = bench$yearlist[[1]]$ranking # single year benchmark
    for (t in 1 : nlist){
      tmpcip   = cip$yearlist[[t]]$ranking
      nams = intersect(names(tmpcip), names(tmpbench))
      idxcip   = !is.na(match(names(tmpcip), nams))
      idxbench = !is.na(match(names(tmpbench), nams))
      if (sum(idxcip) > 1 && sum(idxbench) > 1){
        AbsRankDifference[t] = mean( abs(tmpcip[idxcip] - tmpbench[idxbench]), na.rm = TRUE)
        RankCorrelation[t] = cor( tmpcip[idxcip] , tmpbench[idxbench] , use="complete.obs")        
      }
    }
    AvAbsRankDifference = mean(AbsRankDifference, na.rm = TRUE)    
    AvRankCorrelation   = mean(RankCorrelation, na.rm = TRUE)
  }
  out = list(IndexDifference     = IndexDifference, 
             AvIndexDifference   = AvIndexDifference,
             AbsRankDifference   = AbsRankDifference,
             AvAbsRankDifference = AvAbsRankDifference,
             AvRankCorrelation   = AvRankCorrelation)
  return(out)
} 
#compareCIP = cmpfun(compareCIP)