lcy.venn.diagram <- 
function(Sets,file.name=NULL,Weight=NULL,SetNames=NULL, type='ellipses',doWeights=FALSE,doEuler=FALSE,colourAlgorithm='sequential',width=120,height=120,res=300){
#   used to construct venn diagrams using Erkka's modified script, compute.Venn.fix.r, in anduril component. He used Vennerable package.
#   
#   INPUT:
#       Sets    : List. sets to be used to construct Venndiagram, when list is NOT null, weight and SetNames are ignored. Sets should have.
#       weight  : Vector. when list is not given, weight are directly used to construc Venndiagram
#       SetNames: Vector. when list is not given, weight are directly used to construc Venndiagram.
#                 Both weight and SetNames are required, when list is not given.
#       doWeights: logic. show Venndiagram is weighted.
#       doEuler : logic. 
#       type    : string. One of ‘"circles","squares","triangles","ellipses","AWFE","ChowRuskey","AWFE"’
#       colourAlgorithm : one of c('signature','binary','sequential')
#
#   OUTPUT:
#
#   author      : Chengyu Liu, Erkka,
#   modified    : 2013-03-11

    # functions used in this function
	SetList.new <- function(set.names=NULL, set.contents=NULL) {
	    stopifnot(is.list(set.contents) || is.null(set.contents))
	    if (!is.null(set.names) && !is.null(set.contents)) {
		stopifnot(length(set.names) == length(set.contents))
		stopifnot(all(!duplicated(set.names)))
	    }
	    
	    if (is.null(set.names)) set.names <- character(0)
	    
	    set.list <- data.frame(ID=character(), Members=character(), stringsAsFactors=F)
	    if (is.null(set.contents)) {
		members <- rep(NA, length(set.names))
	    }else {
		for(i in 1:length(set.names)) {
		    set.list <- SetList.assign(set.list, set.names[i], set.contents[[i]])
		}
	    }
	    return(set.list)
	}

	SetList.assign <- function(sets, id, contents) {
	    if(!(length(contents) == 1 && is.na(contents))) {    
		stopifnot(is.character(contents) && all(!is.na(contents)))
		contents <- unique(contents)        
		contents <- paste(contents, collapse=',')
	    }
	    index <- match(id, sets$ID)
	    if (is.na(index)) {
		sets[nrow(sets)+1,] <- c(id, contents)
	    } else {
		sets$Members[index] <- contents
	    }
	    return(sets)
	}
    SetList.get.members <- function(sets, set.id) {
        index <- match(set.id, sets$ID)
        if(is.na(index)) {
            return(NULL)
        } else {
            members <- sets$Members[index]
            return(split.trim(members, ','))
        }
    }

    get.venn <- function(sets.venn, set.list) {
        input <- lapply(sets.venn, FUN= function(x) SetList.get.members(set.list, x))
        names(input) <- sets.venn
        venn <- Venn(input)
        return(venn)
    }
    ## Writes the Venn diagram sets in to sets output.
    write.sets.output <- function(venn, sets.output.file) {
        sets.output <- SetList.new()
        for(set.indicator in rownames(venn@IndicatorWeight)) {
            set.members <- venn@IntersectionSets[[set.indicator]]
            # Parse set name for output
            set.names.index <- !(colnames(venn@IndicatorWeight) == ".Weight")
            set.names <- colnames(venn@IndicatorWeight)[set.names.index]
            membership.indicator <- venn@IndicatorWeight[set.indicator, set.names]
            output.set.name <- ""
            for(set.name in set.names) {
                if(output.set.name != "")
                        output.set.name <- paste(output.set.name, "_AND_", sep="")
                if(membership.indicator[set.name] == 0)
                    output.set.name <- paste(output.set.name, "NOT_", sep="")
                output.set.name <- paste(output.set.name, set.name, sep="")
            }
            if(is.null(set.members)) set.members <- ""
            sets.output <- SetList.assign(sets.output, output.set.name, set.members)
        }
        lcy.table.write(file=sets.output.file,table=sets.output)
        sets.output <- as.matrix(sets.output)
        rownames(sets.output) <- sets.output[,1]
        sets.output <- sets.output[,-1,drop=FALSE]
    }
    #
    require(Vennerable)
    if(!is.null(file.name)){
        #pdf(file=paste(file.name,'.pdf',sep=''),width=width,height=height)
        pdf(file=paste(file.name,'.pdf',sep=''))
    }
    numberOfSets    <- length(Sets)
    vobj            <- Venn(Sets, Weight, SetNames, numberOfSets)
    vennDrawing     <- compute.Venn(vobj, type=type,doWeights=doWeights, doEuler=doEuler)
    gpList          <- VennThemes(drawing=vennDrawing,colourAlgorithm=colourAlgorithm)
    plot(vennDrawing, gpList=gpList)
    if(!is.null(file.name)){
        dev.off()
    }
    
    sets.output <- write.sets.output(vobj,paste(file.name,'.csv',sep=''))
    sets.output
}
