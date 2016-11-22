circle <- function (filename, current = i, selector) {
        #Suppress warnings for the execution of this file
        oldw <- getOption("warn")
        options(warn = -1)
        sink()
        #Send the output into the file "filename.output.txt" in the working directory
        sink(file = paste(filename,".",selector,".txt", sep=""))
        #input the file, skip the unusable sweeps, select the outcome to analyse
        input <- read.csv(filename)
        clean <- subset(input, input$SKIP == 0)
        e <- (split(clean, clean$CURR.TYPE)$e)
        i <- (split(clean, clean$CURR.TYPE)$i)
        print(current)
        print("Mean unclamped currents")
        avcur <- cbind(mean(c(mean(current$lpre), mean(current$lpos))), deg(mean.circular(c(mean.circular(current$apre), mean.circular(current$apos)))))
        colnames(avcur) <- c("R", "Theta")
        print(avcur)
        print("Variance of unclamped currents")
        avvar <- cbind((var.default(c(current$lpre),current$lpos)),var.circular(c(current$apre, current$apos)))
        colnames(avvar) <- c("R", "Theta")
        print(avvar)
        sumtab <- data.frame()
        means <- data.frame()
        kzmeans <- data.frame()
        datalist <- list()
        datalist2 <- list()
        plotlen <- data.frame()
        #compute the quadrants 
        for (j in c("a", "b", "c", "d")) {
        filter <- switch(j, 
                       a = c(1, 1),
                       b = c(1, -1),
                       c = c(-1, 1),
                       d = c(-1, -1)) 
                output <- current[current[["F.OUTCOME"]] == (filter[1]) & current[["A.OUTCOME"]] == (filter[2]), ] #select the F/A quadrant for calculation
                dimtab <- data.frame()
                kztab <- data.frame()
                #check if the selection is valid, write NAs to the mean table if not
                if(nrow(output) == 0) {
                        means <- rbind(means, c(0, 0))
                        kzmeans <- rbind(kzmeans, c(0, 0))
                        plotlen <- rbind(plotlen, 0)
                        next
                }
                print(paste("OUTPUT FILE FOR", (switch(j,
                                             a = "F+ A+",
                                             b = "F+ A-",
                                             c = "F- A+",
                                             d = "F- A-"))))
                print(output[, c(1,3:6,11:16)])
                plotlen <- rbind(plotlen, nrow(output))
                #print("Counter Table for Plots")
                #print(plotlen)
                #the actual calculation for each outcome table
                for (i in 1:nrow(output)) {
                        selection <- data.frame()
                        selection <- output[i, ]
                        aprepos <- deg(mean.circular(c(as.circular(selection$apre), as.circular(selection$apos))))
                        aclamp <- deg(selection$acla)
                        lprepos <- mean(c(selection$lpre), (selection$lpos)) 
                        lclamp <- selection$lcla
                        xprepos <- lprepos * cos(rad(aprepos))
                        yprepos <- lprepos * sin(rad(aprepos))
                        xclamp <- lclamp * cos(rad(aclamp))
                        yclamp <- lclamp * sin(rad(aclamp))
                        dvec <- (log10(sqrt(((xclamp - xprepos)^2)+((yclamp - yprepos)^2)))+1)
                        dang <- deg(atan2((yclamp - yprepos), (xclamp - xprepos)))
                        dimtab <- rbind(dimtab, c(dvec, dang))
                        kztab <- rbind(kztab, c(((log10(lclamp/lprepos))+1), (aclamp - aprepos)))
                        }
                colnames(dimtab) <- c("R", "Theta")
                #print(c("Table for", (switch(j,
                                             #a = "F+ A+",
                                             #b = "F+ A-",
                                             #c = "F- A+",
                                             #d = "F- A-"))))
                colnames(kztab) <- c("R", "Theta")
                print("Dima's Table")
                print(kztab)
                means <- rbind(means, c(mean(dimtab$R), deg(mean.circular(rad(dimtab$Theta)))))
                kzmeans <- rbind(kzmeans, c(mean(kztab$R), deg(mean.circular(rad(kztab$Theta)))))
                dat <- dimtab
                dat$j <- (switch(j,
                                 a = "F+ A+",
                                 b = "F+ A-",
                                 c = "F- A+",
                                 d = "F- A-"))
                datalist[[j]] <- dat
                dat2 <- kztab
                dat2$j <- (switch(j,
                                 a = "F+ A+",
                                 b = "F+ A-",
                                 c = "F- A+",
                                 d = "F- A-"))
                datalist2[[j]] <- dat2
        } 
        sumtab = do.call(rbind, datalist)
        kzsumtab = do.call(rbind, datalist2)
        print ("The Summary Table")
        print(sumtab)
        print ("The Summary Dima's Table")
        print(kzsumtab)
        means <- cbind(means, c("F+ A+", "F+ A-", "F- A+", "F- A-"))
        colnames(means) <- c("R", "Theta", "Operation")
        print("The Means Table")
        print(means)
        print("The Dima's Means Table")
        kzmeans <- cbind(kzmeans, c("F+ A+", "F+ A-", "F- A+", "F- A-"))
        colnames(kzmeans) <- c("R", "Theta", "Operation")
        print(kzmeans)
        means <- na.exclude(means)
        blackdots <- rep(1, nrow(sumtab))
        fuau <- rep(1, plotlen[1,1])
        fuad <- rep(2, plotlen[2,1])
        fdau <- rep(3, plotlen[3,1])
        fdad <- rep(5, plotlen[4,1])
        
        #plot Dimitri's graph
        polar.plot(rp.type = "s", radial.lim = c(0,3), lengths = c(0, as.vector(sumtab$R)), 
                   polar.pos = c(0, as.vector(sumtab$Theta)),main=c(filename, "DMK plot", selector),lwd=3,line.col=c(3,4), 
                   clockwise = FALSE, cex = 2, point.col = c(0, fuau, fuad, fdau, fdad,1,2,3,5),point.symbols = c(0, fuau, fuad, fdau, fdad))
        COLORS <- c(1,2,3,5)
        LINE.TYPES <- c("solid", "dotted", "dashed")
        SYMBOL.TYPES <- c(1,2,3,5)
        legend("bottomleft",
               c("F+ A+", "F+ A-", "F- A+", "F- A-"),
               col = COLORS, pch = SYMBOL.TYPES)
        polar.plot(add = TRUE, radial.lim = c(0,3), rp.type = "r", lengths = c(0, as.vector(means$R)), 
                   polar.pos = c(0, as.vector(means$Theta)),main=c(filename,current),lwd=3,line.col=c(0,1,2,3,5), clockwise = FALSE, cex = 3)
        
        #plot Dima's graph
        polar.plot(rp.type = "s", radial.lim = c(0,3), lengths = c(0, as.vector(kzsumtab$R)), 
                   polar.pos = c(0, as.vector(kzsumtab$Theta)),main=c(filename,"DKZ plot",selector),lwd=3,line.col=c(3,4), 
                   clockwise = FALSE, cex = 2, point.col = c(0, fuau, fuad, fdau, fdad,1,2,3,5),point.symbols = c(0, fuau, fuad, fdau, fdad))
        legend("bottomleft",
               c("F+ A+", "F+ A-", "F- A+", "F- A-"),
               col = COLORS, pch = SYMBOL.TYPES)
        polar.plot(add = TRUE, radial.lim = c(0,3), rp.type = "r", lengths = c(0, as.vector(kzmeans$R)), 
                   polar.pos = c(0, as.vector(kzmeans$Theta)),main=c(filename, current),lwd=3,line.col=c(0,1,2,3,5), clockwise = FALSE, cex = 3)
        sink()
}      