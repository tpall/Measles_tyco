library(magrittr)
library(dplyr)
library(reshape2)
library(gplots)

# skip first two lines
measles <- read.csv("~/Downloads/MEASLES_Cases_1909-2001_20150410025549.csv",skip=2,na.strings = "-")
pdf("/home/taavi/Dropbox/PlayaroundCode/Measles_heatmap.pdf",width=8)
measles %>%
  select(-X) %>% # extra column gets imported by read.csv
  filter(YEAR>=1930) %>%
  melt(id.vars=c("YEAR","WEEK")) %>%
  group_by(YEAR,variable) %>%
  summarise(value=sum(value, na.rm = TRUE)) %>%
  acast(variable~YEAR) %>%
  heatmap.2(., 
            Rowv=NULL, 
            Colv=NULL,
            dendrogram="none", 
            trace="none", 
            labCol={coln <- colnames(.)
                    coln[!coln%in%seq(1930,2000,by=10)]<-" "
                    coln},
            srtCol=0,
            cexRow=0.75,
            main="Measles cases in US states 1930-2001\nVaccine introduced 1961\n(data from Project Tycho)",
            lhei=c(0.25,1), 
            lwid=c(0.1,1), 
            margins=c(5,12),
            col=c(colorRampPalette(c("white", "cornflowerblue"))(10),
                  colorRampPalette(c("yellow", "red"))(1261)), 
            breaks=c(0,0,100,200,300,400,500,600,700,seq(800,127000,by=100)),
            colsep=1:72, rowsep=1:57, sepcolor="white",
            add.expr=lines(c(32,32),c(0,1000),lwd=2))
dev.off()


