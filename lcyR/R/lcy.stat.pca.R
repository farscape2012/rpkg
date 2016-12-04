lcy.stat.pca <- function(d, label=NULL, choices=1:2, output='pca.pdf'){
    pca <- prcomp(d, center=TRUE, scale=TRUE) 
    if(length(choices) == 2){
        if(!is.null(output)){
            if(!grepl(pattern="\\.pdf$",x=output)){
                output = paste(output, '.pdf', sep='')
            }
            pdf(output)
        }
        require(ggbiplot)
        print(ggbiplot(pca, choices=choices, obs.scale=1, var.scale=1, groups=label, ellipse=TRUE, circle=TRUE) +
                            scale_color_discrete(name='') +
                            theme(legend.direction='horizontal', legend.position='top'))

        if(!is.null(output)){
            dev.off()
        }
    }else if(length(choices) == 3){
        if(is.null(label)){
            label = rep("", ncol(d))
        }
        len.label   <- length(unique(label))
        uni.label   <- unique(label)
        uni.color   <- rainbow(len.label)
        color       <- rep(uni.color[1],length(label))
        for(i in 2:length(uni.color)){
            color[label == uni.label[i]] <- uni.color[i]
        }
        require(rgl)
        plot3d(pca$x[,choices], col=color)
        if(!is.null(output)){
            dir.create(output)
            for (i in 1:30) {
              view3d(userMatrix=rotationMatrix(2*pi * i/90, 1, -1, -1))
              rgl.snapshot(filename=paste(output, "/frame-",
              sprintf("%03d", i), ".png", sep=""))
            }
            for (i in 31:60) {
              view3d(userMatrix=rotationMatrix(2*pi * i/90, -1, -1, -1))
              rgl.snapshot(filename=paste(output, "/frame-",
              sprintf("%03d", i), ".png", sep=""))
            }
            for (i in 61:90) {
              view3d(userMatrix=rotationMatrix(2*pi * i/90, 1, -1, 1))
              rgl.snapshot(filename=paste(output, "/frame-",
              sprintf("%03d", i), ".png", sep=""))
            }
            output = sub(pattern="\\.gif$",x=output, replacement='')
            cmd = sprintf("convert -delay 10 -loop 0 %s/frame*.png %s.gif", output, output)
            system(cmd)
            cmd = sprintf("rm -rf %s", output)
            system(cmd)
        }
    }
    return(pca)
}
