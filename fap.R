fap <- function(filename, current = e, foutcome, aoutcome, poutcome) {
        #input the file, skip the unusable sweeps, select the outcome to analyse
        input <- read.csv(filename)
        clean <- subset(input, input$SKIP == 0)
        print(clean)
        e <- (split(clean, clean$CURR.TYPE)$e)
        i <- (split(clean, clean$CURR.TYPE)$i)
        output <- subset(current, current$F.OUTCOME == foutcome)
        print(output)
        #slice and print the chosen current for the selected outcome
        aprepos <- deg(mean.circular(c(mean.circular(output$apre), mean.circular(output$apos))))
        aclamp <- deg(mean.circular(output$acla))
        lprepos <- mean(c(mean(output$lpre), mean(output$lpos))) 
        lclamp <- mean(output$lcla)
        tab <- rbind(c(lprepos, lclamp), c(aprepos, aclamp))
        print(tab)
        #print a vector of [log(ratio of vectors), angle difference]
        ldiff <- lclamp / lprepos
        adiff <- aclamp - aprepos
        print(c(log(ldiff), adiff))
        #plot clamped vs unclamped
        polar.plot(radial.lim = c(0,3), lengths = c(0, log(lprepos), log(lclamp)), polar.pos = c(0, aprepos, aclamp),main=c(filename, "E"),lwd=3,line.col=c(3,4), clockwise = FALSE)
}
