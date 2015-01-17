###################################################################
#  Plot for compind
# Arg:
#  object belonging to compind class
#
# Returns:
#  Plots the graphs for compind results
# Please note, when plotting multiple years, we exclude countries with missing values

plot.compind <- function(x,
                         entity=NULL, entity.num= NULL,
                         which=c("default", "bar", "bar2", "map", "radial", "line"),
                         sort=c("descending", "ascending"),
                         xlab.name = "Entity", ylab.name="Index", title = "default",
                         trace=TRUE)
{
    #Note that when multi year data, the default plot would be a line plot
    #Whereas in single year data, the default plot type is bar
    #Initial sorting of data
    sort <- match.arg(sort)
    which <- match.arg(which)

    ## Enter the loop if compind data for single year
    if(x$time.length == 1)
    {
        tmp.index <- as.data.frame(x$index)
        ## preparing data for plotting
        tmp.index$Entity <- rownames(tmp.index)

        ## sorted.norm is created to use normindicators for radial plot
        sorted.norm <- cbind(x$normindicator.array[,,1], x$index)

        ## to get the column no.of index
        nsorted.norm <- ncol(sorted.norm)

        ## Sort default is decending, sorted using index/score
        if(sort=='ascending')
        {
            tmp.index <- as.data.frame(tmp.index[order(tmp.index[,1]),])
            sorted.norm <- as.data.frame(sorted.norm[order(sorted.norm[,nsorted.norm]),])
        }
        if(sort=='descending')
        {
            tmp.index <- as.data.frame(tmp.index[order(tmp.index[,1], decreasing = TRUE),])
            sorted.norm <- as.data.frame(sorted.norm[order(sorted.norm[,nsorted.norm], decreasing = TRUE),])
        }

        sorted.norm[,nsorted.norm]<- NULL

        ## Sorting of data is complete
        ## naming the column of dataframe according to criteria if ISO code there in dataset
        colnames(tmp.index) <- c("Index", "Entity")
        tmp.index$Entity <- factor(tmp.index$Entity, levels = tmp.index$Entity, ordered = TRUE)

        ## above tmp.index is the final data to be used for plotting
        if(!is.null(entity.num) && !is.null(entity))
            stop('One of entity.num and entity need to be NULL')

        ## The following subsets the data according to the entity.num requirements
        if(trace)
        {
            cat("\nParameters entity and entity.num...\n")
            print(entity)
            print(entity.num)
        }
        if(!is.null(entity.num))
        {
            entity.num <- strsplit(entity.num, " ")
            entity.num <- unique(rapply(entity.num, function(x) head(x)))

            ## nenetity is the number in the entity.num input
            nentity <- as.numeric(entity.num[2])

            ## number of rows in the sub setted tmp.index
            ntmp.index <- nrow(tmp.index)

            ## error checking
            if(is.na(nentity))
                stop('The argument inserted is not acceptable in entity.num')

            if(entity.num[1]=="first")
            {
                if(is.na(entity.num[3]))
                {
                    pos <- (1:nentity)
                }
                else if(entity.num[3]=='percent')
                {
                    if(nentity>=0 && nentity <= 100)
                    {
                        nentity <- round(((nentity*ntmp.index)/100))
                        pos <- (1:nentity)
                    }
                    else
                    {
                        stop('The percentage number should be between 0 and 100')
                    }
                }
                else
                {
                    stop('The argument inserted is not correct in entity.num')
                }
            }else if(entity.num[1]=='last')
            {
                if(is.na(entity.num[3]))
                {
                    pos <- ((ntmp.index-nentity+1):ntmp.index)
                }
                else if(entity.num[3]=='percent')
                {
                    if(nentity>=0 && nentity <= 100)
                    {
                        nentity <- round(((nentity*ntmp.index)/100))
                        pos <- ((ntmp.index-nentity+1):ntmp.index)
                    }
                    else
                    {
                        stop('The percentage number should be between 0 and 100')
                    }
                }
                else
                {
                    stop('The argument inserted is not correct in entity.num')
                }
            }
            else
            {
                stop('The argument inserted is not correct in entity.num')
            }

            ## Create a new column with values 0, then selectively fill the value 1 using pos
            tmp.index$value <- rep(0)
            tmp.index[pos,3] <- 1
        } else if(!is.null(entity))
        {
            entity.match <- entity %in% tmp.index$Entity
            entity.match.value <- all(entity.match)


            if(!isTRUE(entity.match.value))
                stop('The elements of entity vector do not match the entities in calculated index')

            pos <- match(entity, tmp.index$Entity)
            tmp.index$value <- rep(0)
            tmp.index[pos, 3] <- 1
        }

        if(which == "map")
        {
            ## plot with selected entities
            if(!is.null(entity) || !is.null(entity.num))
            {
                tmp.map.index <- tmp.index[pos,]
            }
            else
            {
                tmp.map.index <- tmp.index
            }

            ## VT::18.09.2014
            ## Guess the type of country code (ISO3, ISO2, UN or NAME)
            ## If name or UN, replace by ISO3
            ##
            nameJoinColumn <- "Entity"
            myx <- as.character(tmp.map.index$Entity)
            lmyx <- length(unique(nchar(myx)))

            joinCode <- if(lmyx == 1 & nchar(myx[1]) == 2) "ISO2"
                        else if(lmyx == 1 & nchar(myx[1]) == 3)
                            if(sum(grepl("^[[:digit:]]*$", myx)) == length(myx)) "UN"
                            else "ISO3"
                        else "NAME"
            if(joinCode == "NAME" | joinCode == "UN")
            {
                myx <- getCountryCodeISO3(myx)
                joinCode <- "ISO3"
                tmp.map.index$myx <- myx
                nameJoinColumn <- "myx"
            }

            sPDF.index <- joinCountryData2Map(tmp.map.index, joinCode = joinCode, nameJoinColumn = nameJoinColumn)
            map.index <- mapCountryData(sPDF.index, nameColumnToPlot= "Index")

            return(invisible(map.index))
        }   # map type plots finished above

        ## Bar plot type starts here
        if(which == 'bar' || which == 'default')
        {
            if(trace)
                cat("\nBar plot, single year...\n")

            if(!is.null(entity) || !is.null(entity.num))
            {
                tmp.bar.index <- tmp.index[pos,]
            }
            else
            {
                tmp.bar.index <- tmp.index
            }
            index.bar.plot <- ggplot(data=tmp.bar.index, aes(x=Entity, y=Index, fill=Entity)) + geom_bar(colour="black",stat="identity")+ guides(fill=FALSE)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  xlab(xlab.name) + ylab(ylab.name)
            if(title == 'default')
                index.bar.plot <- index.bar.plot + ggtitle(paste("Bar plot for", xlab.name, "vs", ylab.name))
            else
            {
                index.bar.plot <- index.bar.plot + ggtitle(title)
            }
            return(index.bar.plot)
        }

        ## Bar plot with respect to other starts here
        if(which == 'bar2')
        {
            if(trace)
                cat("\nBar plot, single year...\n")

            if(is.null(entity) && is.null(entity.num))
                stop('entity.num and entity both can not be NULL when plotting bar plot with respect to other')

            index.bar.plot.withrest <- ggplot(data=tmp.index, aes(x=Entity, y=Index, fill=factor(value))) + geom_bar(colour="black", stat="identity")+
            scale_fill_manual(values = c("1" = "red", "0" = "white"),guide=FALSE)+ guides(fill=FALSE)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab(xlab.name) + ylab(ylab.name)
            if(title == 'default')
                index.bar.plot.withrest <- index.bar.plot.withrest + ggtitle(paste("Bar plot for selected", xlab.name, "with respect to rest"))
            else
            {
                index.bar.plot.withrest <- index.bar.plot.withrest + ggtitle(title)
            }
            return(index.bar.plot.withrest)
        }

        if(which == 'line')
            stop('Line plot is plotted for multiple year index dataset.')

        ## Spider plot for underlying indicators for different countries
        if(which == 'radial')
        {
            if(!is.null(entity) || !is.null(entity.num))
            {
                sorted.norm <- sorted.norm[pos,]
            }

            col <- nrow(sorted.norm):1
            lty <- nrow(sorted.norm):1
            radial.lim=c(0, max(sorted.norm))
            if(title == 'default')
            {
                radial.plot(sorted.norm, labels=colnames(sorted.norm), rp.type="p", clockwise=TRUE,
                    radial.lim=radial.lim, line.col=col, lwd=2.5, lty=lty, ylim=c(0,1),
                    main=paste('Radial Decomposition plot for underlying indicators for', toString(entity)))
            }
            else
            {
                radial.plot(sorted.norm, labels=colnames(sorted.norm), rp.type="p", clockwise=TRUE,
                    radial.lim=radial.lim, line.col=col, lwd=2.5, lty=lty, ylim=c(0,1), main=title)
            }

            usr <- par("usr")
            legend.position <- xy.coords(1.2*usr[2], usr[3]-usr[3]/2)
            legend(legend.position, legend=rownames(sorted.norm), col=col, lty=lty, lwd=2.5, cex=1, bty="o")
        }
        if( which == 'line')
            stop('Line plot can be plotted only for multiyear data.')
    }    ## Single year if loop ended above
    else
    {

        ## select line plot as default for multiyear data
        tmp.index <- x$index
        tmp.index <- na.omit(tmp.index)

        ## if enitity is present
        if(!is.null(entity))
        {
            entity.match <- entity %in% rownames(tmp.index)

            entity.match.value <- all(entity.match)
            if(!isTRUE(entity.match.value))
                stop('The elements of entity vector do not match the entities in calculated index')
            pos <- match(entity, rownames(tmp.index))
        }


        if(which == 'line' || which == 'default')
        {
            if(trace)
            {
                cat("\nLine plot, several years: ", x$time.length, "\n")
                print(names(x))
            }

            if(!is.null(entity))
            {
                tmp.index <- tmp.index[pos,]
            }

            tmp.index <- t(tmp.index)

            mdf <- melt(tmp.index, id.vars="entity", value.name="Index_Value", variable.name="time.period")
            colnames(mdf) <- c("time.period", "entity", "Index_Value")
            line.plot <- ggplot(data=mdf, aes(x=time.period, y=Index_Value, group = entity, colour = entity)) +
            geom_line() + geom_point( size=2, shape=22) + xlab(xlab.name) + ylab(ylab.name)
            if(title == 'default')
                line.plot <- line.plot +ggtitle("Line plot of Index Values across time periods")
            else
            {
                line.plot <- line.plot + ggtitle(title)
            }

            return(line.plot)
        }

        if(which == 'radial')
        {
            if(is.null(entity))
                stop('Select entity for radial plot.')

            if(length(entity)>1)
                stop('For Multiyear, select single entity for radial plot.')
            time.length <- x$time.length
            time.seq <- 1:time.length
            entity.data <- x$normindicator.array[pos,,time.seq]
            entity.data <- t(entity.data)
            col <- nrow(entity.data):1
            lty <- nrow(entity.data):1
            radial.lim=c(0, max(entity.data))
            if(title == 'default')
            {
                radial.plot(entity.data, labels=colnames(entity.data), rp.type="p", clockwise=TRUE,
                    radial.lim=radial.lim, line.col=col, lwd=2.5, lty=lty, ylim=c(0,1),
                    main=paste('Radial Decomposition plot for underlying indicators for', entity))
            }
            else
            {
                radial.plot(entity.data, labels=colnames(entity.data), rp.type="p", clockwise=TRUE,
                    radial.lim=radial.lim, line.col=col, lwd=2.5, lty=lty, ylim=c(0,1), main=title)
            }

            usr <- par("usr")
            legend.position <- xy.coords(1.25*usr[2], usr[3]- usr[3]/2)
            legend(legend.position, legend=rownames(entity.data), col=col, lty=lty, lwd=2.5, cex=1, bty="o")
        }
    } # multiyear loop code ends here
}
