#################################################################
## Compare index between K countries
# Args:
#   computed index of compind class
#   benchmark is also computed index of a particul time period of class compind
#
# Returns:
#   A list of Index Difference, AvIndexdifference, AbsRankdifference, 
#   AvAbsRanKDifference and AvRankCorrelation 

compare <- function(input, ...) UseMethod("compare")

compare.compind <- function(input, bench= NULL){
  
  nlist <- input$time.length
  entity <- rownames(input$ranking)
  time.period <- colnames(input$ranking)
  nentity <- length(entity)
  
  # aggregated measure over years
  IndexDifference <- array(data = NA, c(nentity, nentity, nlist))
  dimnames(IndexDifference) <- list(entity = entity, entity = entity, year = time.period)
  
  # D_AB measure over time (difference in values of the composite indicator)
  for (time.interval in 1 : nlist){
    tmp.index   <- input$index[,time.interval]
    entityt <- names(tmp.index)
    nentityt <- length(entityt)    
    for (i in 1 : (nentityt-1)) {
      entityti <- entityt[i]
      posit <- which(entityt == entityti)
      posi  <- which(entity == entityti)
      for (j in ((i+1) : nentityt)) {
        entitytj <- entityt[j]
        posjt <- which(entityt == entitytj)
        posj  <- which(entity == entitytj)
        if (length(posit == 1) && length(posjt == 1)){
          tmpi <- tmp.index[posit] 
          tmpj <- tmp.index[posjt] 
          IndexDifference[posi,posj,time.interval] = tmpi - tmpj
        }
      }
    }
  }
  AvIndexDifference <- round(apply(IndexDifference, c(1,2), mean, na.rm = TRUE),4)
  
  # RS measure (if bench available)
  AbsRankDifference = AvAbsRankDifference = AvRankCorrelation = NULL
  if (!is.null(bench)){      
    AbsRankDifference <- rep(NaN, nlist)
    RankCorrelation <- rep(NaN, nlist)
    tmpbench <- bench$ranking[,1] # single year benchmark
    for (time.interval in 1 : nlist){
      tmp.index <- input$ranking[, time.interval]
      nams <- intersect(names(tmp.index), names(tmpbench))
      idxindex <- !is.na(match(names(tmp.index), nams))
      idxbench <- !is.na(match(names(tmpbench), nams))
      if (sum(idxindex) > 1 && sum(idxbench) > 1){
        AbsRankDifference[time.interval] <- mean( abs(tmp.index[idxindex] - tmpbench[idxbench]), na.rm = TRUE)
        RankCorrelation[time.interval] <- cor( tmp.index[idxindex] , tmpbench[idxbench] , use="complete.obs")        
      }
    }
    AvAbsRankDifference <- mean(AbsRankDifference, na.rm = TRUE)    
    AvRankCorrelation   <- mean(RankCorrelation, na.rm = TRUE)
  }
  out = list(IndexDifference     = IndexDifference, 
             AvIndexDifference   = AvIndexDifference,
             AbsRankDifference   = AbsRankDifference,
             AvAbsRankDifference = AvAbsRankDifference,
             AvRankCorrelation   = AvRankCorrelation)
  
  out$call <- match.call()
  out$bench <- bench
  out$time.period <- time.period
  class(out) <- "compare"
  return(out)
} 
