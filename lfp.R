lfp <- function (filename) {
        #read libraries etc
        library(dplyr)
        
        #read the lfp table, remake it into nsweepsx12, bind proper names 
        lfptab <- read.csv(filename)
        r1 <- lfptab[seq(3, nrow(lfptab), 4), ]
        r2 <- lfptab[seq(4, nrow(lfptab), 4), ]
        r3 <- lfptab[seq(5, nrow(lfptab), 4), ]
        r4 <- lfptab[seq(6, nrow(lfptab), 4), ]
        outcome <- cbind(r1, r2, r3, r4)
        colnames(outcome) <- c("SWEEP","A","B", "FPRE", "FCLA", "FPOS", "APRE", "ACLA", "APOS", "PPRE", "PCLA", "PPOS")
        fs <- select(outcome, starts_with("F"))
        fs <- as.data.frame(fs)
        fs <- mutate(fs, FOUTCOME = FPRE / FPOS)
        outcome <- mutate(outcome, A.OUTCOME = FCLA + APOS)
        fnum <- as.numeric(outcome$FPRE)
        print(fnum)
        #
        
        
        #fs <- vector()
        #for (i in 1:nrow(outcome)) {
        #       foutcome <- (2 * outcome[[i, outcome$FCLA]]) / (outcome[[i, outcome$FPRE]] + outcome[[i, outcome$FPOS]])
        #       print(foutcome)
               #if (foutcome < 0.9) {fs <- c(fs, -1)}
               
        #}
        print(fs)
        print(outcome)
}
