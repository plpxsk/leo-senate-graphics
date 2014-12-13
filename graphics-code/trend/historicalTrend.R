################################################################################
## WHAT         plot of historical trend of senate poll results
## HOW          ggplot graphic
## NOTES        NONE
## AUTHOR	Pawel Paczuski [github.com/pavopax]
################################################################################

library(ggplot2)
library(reshape2)                       # for melt()
library(dplyr)                          # for data manipulation
library(ggthemes)                       # WSJ theme

## (A) DATA MANIPULATION
df0 <- read.csv("../../data-publisher/public/_big_assets/senate-likelihood.tsv",
                sep="\t", stringsAsFactors=FALSE)
df0$date <- as.Date(df0$date,  "%d-%b-%y")


## add republican trend and convert to long format
df1 <- data.frame(cbind(df0, rep=100-df0$dem))
trend <- melt(df1, id.vars = "date")
##summary(trend)



## (B) GRAPHICS
## basic plot
theme_set(theme_bw())

p0 <- ggplot(df0, aes(x=date, y=dem, group="NA")) + geom_line() +
    ylim(c(30,70))
p0 + geom_hline(yintercept=50)


## wall st journal theme
p1 <- ggplot(trend, aes(x=date, y=value, group=variable, colour=variable)) +
    geom_line(size=1.3) + 
    scale_colour_manual(values=c("#42698A", "#B47E7C")) +
    ylim(c(0,100)) +
    ggtitle("Back and Forth") + 
    theme_wsj() +
    theme(  panel.grid.minor.x=element_blank()
          , panel.grid.major.x=element_blank()
          , legend.position="none"
          )
p1



## final plot
p2 <- ggplot(trend, aes(x=date, y=value, group=variable, colour=variable)) +
    geom_line(size=1.3) + 
    scale_colour_manual(values=c("dodgerblue4", "darkred")) +
    ylim(c(0,100)) +
    geom_hline(yintercept=50, size=.4) +
    theme(panel.grid.minor.x=element_blank()
          , panel.grid.major.x=element_blank()
          , legend.title=element_blank()
          ) +
    ylab("") + xlab("") +
    ggtitle(expression(atop("Back and forth",
        atop(italic("The likelihood of each party winning the Senate has varied throughout 2014"),""))))
p2

## output
png("../../graphics-output/trend.png", width = 800, height=400)
p2
dev.off()



