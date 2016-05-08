cmean <- function(filename) {
        input <- read.csv(filename)
        names(input) <- c("file", "X", "lpre", "apre", "lcla", "acla", "lpos", "apos")
        head(input)
        spl <- split(input, input$X)
        head(spl)
        e <- spl$e
        i <- spl$i
        #slice and plot e
        eaprepos <- deg(mean.circular(c(mean.circular(e$apre), mean.circular(e$apos))))
        eaclamp <- deg(mean.circular(e$acla))
        elprepos <- mean(c(mean(e$lpre), mean(e$lpos)))
        elclamp <- mean(e$lcla)
        etab <- rbind(c(elprepos, elclamp), c(eaprepos, eaclamp))
        print(etab)
        polar.plot(lengths = c(0, elprepos, elclamp), polar.pos = c(0, eaprepos, eaclamp),main=c(filename, "E"),lwd=3,line.col=c(3,4), clockwise = TRUE, start = 90)
        #slice and plot i
        iaprepos <- deg(mean.circular(c(mean.circular(e$apre), mean.circular(e$apos))))
        iaclamp <- deg(mean.circular(e$acla))
        ilprepos <- mean(c(mean(i$lpre), mean(i$lpos)))
        ilclamp <- mean(i$lcla)
        itab <- rbind(c(ilprepos, ilclamp), c(iaprepos, iaclamp))
        print(itab)
        polar.plot(lengths = c(0, ilprepos, ilclamp), polar.pos = c(0, iaprepos, iaclamp),main=c(filename, "I"),lwd=3,line.col=c(3,4), clockwise = TRUE, start = 90)
}