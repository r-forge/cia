
#################################################################
## Monte Carlo analysis of CIP

montecarlo <- function(input, ...) UseMethod("montecarlo")
montecarlo.compind <- function(input, nboot = 250, random=NULL) {
  if(is.null(random))
    stop('random can not be null, should be the paramenters to vary in MonteCarlo')
  possible.randoms <- c("aggregation", "normalization", "missingindicator", "cleaning")
  final.random <- random[random %in% possible.randoms]
  if((length(final.random) == 0) && (typeof(final.random) == "character"))
    stop('random can only be only among aggregation, normalization, missingindicator and cleaning')
  
  ctr <- input$ctr
  if (missing(input))
    stop('Calculated index of class compind is missing')
  
  entity  = rownames(input$index)
  nentity = length(entity)
  
  indexMC   = matrix(data = NA, nrow = nentity, ncol = nboot)
  rankingMC = matrix(data = NA, nrow = nentity, ncol = nboot)
  for (m in 1 : nboot) {
    dft = list(aggregation = c('geometric' , 'linear'),
              normalization = c('min-max', 'none', 'z-score', 'robust-z-score'), 
              missingindicator = c('last','none','reweight', 'linear'), 
              cleaning = c('none', 'hampel_twosided' , 'hampel_onesided')      
              )
    rndctr = ctr
   
    # randomize aggregation
    if ('aggregation'%in%final.random){
      rndctr$aggregation = dft$aggregation[sample(1:length(dft$aggregation), 1)]
    }
    # randomize normalization
    if ('normalization'%in%final.random){
      rndctr$normalization = dft$normalization[sample(1:length(dft$normalization), 1)]
    }
    # randomize missing data technique
    if ('missingindicator'%in%final.random){
      rndctr$missingindicator = dft$missingindicator[sample(1:length(dft$missingindicator), 1)]
    }
    
    # randomize cleaning
    if ('cleaning'%in%final.random){
      rndctr$cleaning = dft$cleaning[sample(1:length(dft$cleaning), 1)]
    }    
   
    # complete the dataset if randomization on missing data technique
    call <- input$call
    call$ctr <- rndctr
    tmp = eval(call)
    indexMC[,m]   = tmp$index
    rankingMC[,m] = tmp$ranking
  }
  
  dimnames(indexMC) = dimnames(rankingMC) = list(entity = entity, 1:nboot)
  out=list(index = input, indexMC = indexMC, rankingMC = rankingMC)
  out$call <- match.call()
  class(out) <- "montecarlo"
  return(out)
}
