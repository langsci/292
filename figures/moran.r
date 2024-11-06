library(ggplot2)
library(ggsci)
library(gridExtra)
library(plyr)
library(scatterpie)


nn <- 2:10

rr <- 10^seq(-1,1,length.out=11)

rho <- function(n,r) {
    if (r == Inf) return(1)
    if (r == 1) return(1/n)
    if (r == 0) return(0)
    pBA <- ((r^-1 -1)/(r^-n -1))
    return(pBA)
}



mockData = data.frame()
for (n in nn) {
    for (r in rr) {
        pBA = rho(n,r)
        rw <- c(n,r,pBA)
        mockData <- rbind(mockData,rw)
    }
}
colnames(mockData) <- c('N','r', "pBA")

mockData$pBB <- 1-mockData$pBA

mockData$rho <- log10(mockData$r)
mockData$popSize <- mockData$N / max(mockData$N)

rBreaks <- c(0.1, 0.5, 1, 5, 10)
rhoBreaks <- log10(rBreaks)

nBreaks <- c(0, 2, 4, 6, 8, 10)
pBreaks <- nBreaks/ max(mockData$N)


p <- ggplot() +
    geom_scatterpie(data=mockData,
                    aes(x=rho, y=popSize, r=.03),
                    cols=c("pBA", "pBB")) +
    scale_fill_manual(values=c('black', 'white')) +
    coord_fixed() +
    ylab('population size') +
    xlab("p(A)/p(B)") +
    scale_x_continuous(breaks=rhoBreaks,labels=rBreaks) +
    scale_y_continuous(breaks=pBreaks,labels=nBreaks)




svg('weakSelection.svg',width=12,height=6)
p
dev.off()
