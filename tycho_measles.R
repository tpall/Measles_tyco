library(magrittr)
library(dplyr)
library(reshape2)
library(gplots)

# skip first two lines
# measles <- read.csv("~/Downloads/MEASLES_Cases_1909-2001_20150410025549.csv",skip=2,na.strings = "-")
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

library(ggplot2)
measles %>%
  select(-X) %>% # extra column gets imported by read.csv
  filter(YEAR>=1930) %>%
  melt(id.vars=c("YEAR","WEEK")) %>%
  group_by(YEAR,variable) %>%
  summarise(value=sum(value, na.rm = TRUE)) %>%
  ggplot(aes(y=variable, x=YEAR, fill=value)) + 
  geom_tile(colour="white", linewidth=2, 
            width=.9, height=.9) + theme_minimal() +
  scale_fill_gradientn(colours=c(colorRampPalette(c("white", "cornflowerblue"))(10),
                                 colorRampPalette(c("yellow", "red"))(1261)), limits=c(0, 4000),
                       breaks=seq(0, 4e3, by=1e3), 
                       na.value=rgb(246, 246, 246, max=255),
                       labels=c("0k", "1k", "2k", "3k", "4k"),
                       guide=guide_colourbar(ticks=T, nbin=50,
                                             barheight=.5, label=T, 
                                             barwidth=10)) +
  scale_x_continuous(expand=c(0,0), 
                     breaks=seq(1930, 2010, by=10)) +
  geom_segment(x=1961, xend=1961, y=0, yend=59.5) +
  labs(x="", y="", fill="") +
  ggtitle("Measles") +
  theme(legend.position=c(.5, -.13),
        legend.direction="horizontal",
        legend.text=element_text(colour="grey20"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=6, family="Helvetica", 
                                 hjust=1),
        axis.text.x=element_text(size=8),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-.07, face="bold", vjust=1, 
                           family="Helvetica"),
        text=element_text(family="URWHelvetica")) +
  annotate("text", label="Vaccine introduced", x=1961, y=61, 
           vjust=1, hjust=0, size=I(3), family="Helvetica")

