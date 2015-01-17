#################################################################
## Compute the composite index for a given dataset and control argument
##  calls ComputeSingleYearIndex to calculate index for each and every year
##
## Args:
##   formula: Input as format cs~ ind1+ind2+ind3..
##   data: dataset
##   time: the name of the column corresponding to time/year
##   for.period : In case the time has multiple timeperiod and you need to
##                compute index only for one timeperiod, ex: for.period=2000
##   ctr: Process Control inputs
## Returns:
##   A list of index, ranking, arrays of indicator, normindicator, weight
##   Also returns call and ctr in the out


##  The S3 version
compind <- function (x, ...) UseMethod("compind")

compind.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    optx <- options("na.action")
    if(missing(na.action))
        options("na.action"="na.pass")

    m <- match.call(expand.dots = FALSE)
    m$... <- NULL
    m[[1]] <- as.name("model.frame")

    m <- eval.parent(m)
    Terms <- attr(m, "terms")

    cs <- model.response(m)
    if(is.null(cs))
        stop("response is missing in formula")

    options("na.action"=optx)

    cs.name <- colnames(m)[1]
    x <- model.matrix(Terms, m)
    xint <- match("(Intercept)", colnames(x), nomatch=0)
    if(xint > 0)
        x <- x[, -xint, drop=FALSE]
    x <- cbind.data.frame(cs, x)
    colnames(x)[1] <- cs.name

    res <- compind.default(x, cs=cs.name, ...)

    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl <- match.call()
    cl[[1]] <- as.name("compind")
    res$call <- cl

    res
}

compind.default <- function(x, cs=NULL, time=NULL, for.period=NULL, ctr=ProcessControl(), ...)
{

    ## inputs checking
    if (missing(x))
        stop ('data is missing')
    data <- if(!is.data.frame(x))
                as.data.frame(x)
            else
                x
    n <- NROW(data)
    p <- NCOL(data)

    hdr <- colnames(data)
    if(is.null(hdr))
        hdr <- paste("V", 1:p, sep="")

    ## creating new dataframe final.data to be used for index calculations
    ## if cs is null, assume all data belongs to one time frame and the cs names come from rownames.
    if (is.null(cs)){
        cs.column <- as.data.frame(rownames(data))
    }
    else {
        cs.number <- which(colnames(data) == cs)
        if(length(cs.number) != 1)
            stop(paste("wrong argument 'cs': ", cs, "not found in the columns of data."))

        cs.column <- data[, cs.number, drop=FALSE]
        data[, cs.number] <- NULL
    }

    ## if time is null, assume all data belongs to one time frame named TimePeriod
    if (is.null(time)){
        row.number <- nrow(data)
        time.column <- as.data.frame(rep("TimePeriod", row.number))
    }
    else {
        time.number <- which(colnames(data)==time )
        if(length(time.number) != 1)
            stop(paste("wrong argument 'time': ", time, "not found in the columns of data."))

        time.column <- data[, time.number, drop=FALSE]
        data[, time.number] <- NULL
    }

    eid.indicators <- cbind(cs.column, data)

    final.data <- cbind(time.column, eid.indicators)

    ## complete the data
    final.data <- CompleteData(final.data, ctr)

    ## clean the data
    final.data <- CleanData(final.data, ctr)
    nfinal.data <- ncol(final.data)
    nindicators <- (nfinal.data-2)
    name.indicators <- colnames(final.data[,3:nfinal.data])

    ## take a subset of data if year provided
    if (!is.null(for.period))
    {
        pos <- !is.na(match(as.character(final.data[,1]), as.character(for.period)))
        if (!any(pos))
          stop('no period corresponding to this period in data')

        final.data <- final.data[pos,]
    }

    ## Retrive cs names and timeperiod
    entity  <- unique(as.character(final.data[,2]))

    nentity <- length(entity)
    time.period <- unique(as.character(final.data[,1]))
    time.length <- length(time.period)

    ## initialization of ranking and index
    index <- ranking <- matrix(data = NA, nrow = nentity, ncol = time.length)
    dimnames(index) <- dimnames(ranking) <- list(entity = entity, time.period = time.period)

    ## initialisation of indicator
    indicator.array <- array(data = NA, dim=c(nentity, nindicators, time.length))
    dimnames(indicator.array) <- list(entity = entity, name.indicators = name.indicators, time.period = time.period)

    ## initialisation of normindicators
    normindicator.array <- array(data = NA, dim=c(nentity, nindicators, time.length))
    dimnames(normindicator.array) <- list(entity = entity, name.indicators = name.indicators, time.period = time.period)

    ## initialisation of weights
    weight.array <- array(data = NA, dim=c(nentity, nindicators, time.length))
    dimnames(weight.array) <- list(entity = entity, name.indicators = name.indicators, time.period = time.period)

    ## compute index for each time period
    for (i in 1 : time.length){
        tmp <- ComputeSinglePeriodIndex(final.data, time.period[i], ctr)
        pos <- match(rownames(tmp$normindicators), entity) # position of entity in storage matrix
        index[pos,i] <- tmp$index
        ranking[pos,i] <- tmp$ranking
        indicator.array[,,i] <- tmp$indicators
        normindicator.array[,,i] <- tmp$normindicators
        weight.array[,,i] <- tmp$weights
    }

    out <- list(entity=entity, index = index, ranking = ranking, indicator.array = indicator.array, normindicator.array = normindicator.array, weight.array = weight.array)
    out$call <- match.call()
    out$ctr <- ctr

    out$time.length <- time.length
    class(out) <-  "compind"


    out
}

####################################################
## Compute the CIP for a given time.period
ComputeSinglePeriodIndex <- function(data, time.period, ctr){
  # subset of data for the selected years
  seldata <- data[as.character(data[,1]) == as.character(time.period),]
  #seldata <- droplevels(seldata)

  # determine entity for the time period
  entity  <- as.character(seldata[,2])
  nentity <- length(entity)
  if (length(unique(entity)) != nentity){
    stop('several time same entity for same time period')
  }
  # select indicators
  indicators <- seldata[,3:ncol(seldata)]
  rownames(indicators) <- entity

  # normalization of indicators
  normindicators <- NormalizeIndicators(indicators, ctr)

  # weights for indicators
  weights <- ComputeWeights(normindicators, ctr)

  # aggregation of indicators
  pos <- colSums(weights)>0
  # avoid that NA of indicator not in definition propagates

  if (ctr$aggregation == 'linear'){
    if(ctr$missingindicator == 'reweight'){
      index <- apply(normindicators[,pos] * weights[,pos], 1, sum, na.rm = TRUE)
    }else{
      index <- apply(normindicators[,pos] * weights[,pos], 1, sum, na.rm = FALSE)
    }
  }

  else if (ctr$aggregation == 'geometric'){
    if(ctr$missingindicator == 'reweight'){
      index <- apply(normindicators[,pos]^weights[,pos], 1, prod, na.rm = TRUE)
    }else{
      index <- apply(normindicators[,pos]^weights[,pos], 1, prod, na.rm = FALSE)
    }
  }
  else{
    stop('control$method not well defined')
  }
  ranking <- rank(-index, na.last = "keep")
  #index <- round(index,4)
  indicators <- as.matrix(indicators)

  out <- list(index = index, ranking = ranking, indicators = indicators ,
              normindicators = normindicators, weights = weights)

  return(out)
}
