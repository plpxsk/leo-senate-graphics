###############################################################################
## WHAT         histogram of senate seat results
## HOW          ggplot graphic
## NOTES        three different graphics
## AUTHOR	Pawel Paczuski [github.com/pavopax]
###############################################################################

## graphics, data manipulation, themes
library(ggplot2)
library(dplyr)
library(ggthemes)


## (A) DATA MANIPULATION
df0 <- read.csv("../../data-publisher/public/_big_assets/histogram.tsv",
                sep="\t")

df0$rep <- 100-df0$dem

## new dataframe (df) for (g)graphic
dfg <- filter(df0, chance>0.001)
dfg$label <- ifelse(dfg$chance < 0.01, "<1%",
                    paste0(round(100*dfg$chance, 0), "%"))
## for bar color
dfg$col <- ifelse(dfg$dem > 50, 1, 0)
## label "likeliest" bar
dfg$star <- ""
dfg$star[which.max(dfg$chance)] <- "Likeliest"




## (B) GRAPHICS
theme_set(theme_bw())

## basic histogram
qplot(x=dem, y=chance, data = dfg, geom="bar", stat="identity") +
    coord_flip() + annotate("text", y = rep(-.03, dim(dfg)[1]),
                            x = dfg$dem, label = dfg$label, size=4)


## wall st journal theme
qplot(x=dem, y=chance, data = dfg, geom="bar", stat="identity") +
    coord_flip() + annotate("text", y = rep(-.03, dim(dfg)[1]),
                            x = dfg$dem, label = dfg$label, size=4) +
    ggtitle("Need to annotate axes..") +
    theme_wsj()



## FURTHER HISTOGRAM CUSTOMIZATIONS
## need some dimensions for text placement
lx <- dim(dfg)[1]
s.first <- dfg$dem[1]
s.last <- dfg$dem[lx]

p1 <- ggplot(dfg, aes(x=dem, y=chance, fill=factor(col))) +
    geom_bar(stat="identity") + coord_flip() +
    scale_fill_manual(values=c("1"="#455897", "0"="#BC1736")) +
    scale_y_continuous(minor_breaks=seq(0,100,1)) +
    geom_text(aes(label=star), hjust=1.2, fontface="italic", size=5) + 
    annotate("text", y = rep(-.04, lx),
             x = dfg$dem, label=dfg$label, size=5) +
    annotate("text", y = rep(-.08, length(dfg$dem)),
             x = dfg$dem, label=dfg$rep, size=5) +
    annotate("text", y = rep(-.12, length(dfg$dem)),
             x = dfg$dem, label=dfg$dem, size=5) +
    annotate("text", y = -.04,
             x = 2+dfg$rep[1], label="Likelihood", size=5) +
    annotate("text", y = -.08,
             x = 2+dfg$rep[1], label="Rep.\nSeats", size=5) +
    annotate("text", y = -.12,
             x = 2+s.last, label="Dem. \nSeats", size=5) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line( size=.1, color="black" ),
          panel.border=element_blank(),
          legend.position="none"
          ) +
    ## minor grid lines
    scale_x_continuous(minor_breaks = seq(s.first-.5, s.last+.5,1))
p1


## save
png("../../graphics-output/histogram.png", width=800, height=600)
p1
dev.off()

