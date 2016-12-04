lcy.perpas <- function(paths,g, data, group=list(ctrl, treat), score.type = "APD", use = "none", min.neighbor = 3, database = "both",min.overlap=0.3){
    std.data    <- lcy.tORz.score(data=data,
                    group=group,
                    byrow=T, scale=TRUE, method='median', type='zscore')
    if(missing(paths) & missing(g)){
        score   <- lcy.pathway.scoring(data=std.data, 
                                        score.type=score.type, 
                                        use=use, 
                                        min.neighbor=min.neighbor, 
                                        database=database,
                                        min.overlap=min.overlap
                                        )
    }else if(missing(paths) & !missing(g)){
        score   <- lcy.pathway.scoring(g=g, 
                                        data=std.data, 
                                        score.type=score.type, 
                                        use=use, 
                                        min.neighbor=min.neighbor, 
                                        database=database,
                                        min.overlap=min.overlap
                                        )
    }else if(!missing(paths) & missing(g)){
        score   <- lcy.pathway.scoring(paths=paths, 
                                        data=std.data, 
                                        score.type=score.type, 
                                        use=use, 
                                        min.neighbor=min.neighbor, 
                                        database=database,
                                        min.overlap=min.overlap
                                        )
    }
    return(score)
}

