## Usage: Plot VCF birthday
## Author: Brittany Howell (bh10@sanger.ac.uk)
## Date: 29th May 2019

args = commandArgs(TRUE)
numPlot = args[1]
print(numPlot)

setwd("~/Documents/data/runtime/")
convertMonth <- function(x) {
  if (x == "Jan") {
    val <- 1
  } else if (x == "Feb"){
    val <- 2
  } else if (x == "Mar"){
    val <- 3
  } else if (x == "Apr"){
    val <- 4
  } else if (x == "May"){
    val <- 5
  } else if (x == "Jun"){
    val <- 6
  } else if (x == "Jul"){
    val <- 7
  } else if (x == "Aug"){
    val <- 8
  }
  return(val)
} 
suppressMessages(library(ggplot2))
suppressMessages(library(viridis))
suppressMessages(library(RColorBrewer))
suppressMessages(library(stringr))
suppressMessages(library(tidyr))
suppressMessages(library(tibble))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))

# full.table <- read.table("fullLine.txt", stringsAsFactors = F)
full.table.new <- as_tibble(read.table("fullLine.txt", stringsAsFactors = F))
# full.table.historic <- as_tibble(read.table("full_table_distinct.txt",stringsAsFactors = F))

# full.table.combine <- rbind(full.table.new,full.table.historic)
# full.table.distinct <- full.table.combine %>% distinct(.keep_all = FALSE)
# write.table(full.table.distinct,"full_table_distinct.txt",sep=" ",quote = F,row.names = F,col.names = F)

full.table <- as.data.frame(full.table.new)
# cd ${wkdir}
# cat fullLine.txt  | cut -f2-10 -d'M'  > firstChop.txt
# cat firstChop.txt |  cut -f4 -d'/' | cut -f1 -d'.'  > samplenames.txt
# cat fullLine.txt |  cut -f5 -d' ' | cut -f1 -d'/'  > batch.txt
# cat fullLine.txt |  cut -f2,3,4 -d' ' | tr " " "\t"  > date.txt
# 
# 
# paste date.txt batch.txt samplenames.txt > vcf_dates.txt

# head(full.table)

samplename.col <- as.character(unlist(as.data.frame(as.character(full.table[,8]))))
# samplenames <- strsplit(x = as.character(samplenames), split = "/")
samplenames <- NULL

for (i in 1:nrow(full.table)) {
  samplenames <- (
    append(samplenames,
        gsub(pattern = ".gt.vcf",replacement = "",
              str_split(samplename.col[i],
                  "/")[[1]][4]))
  )
}


# samplename.col <- as.character(unlist(as.data.frame(as.character(full.table[,8]))))
# samplenames <- strsplit(x = as.character(samplenames), split = "/")
batch <- NULL

for (i in 1:nrow(full.table)) {
  batch <- (
    append(batch,
           str_split(samplename.col[i],
                     "/")[[1]][1])
  )
}

stuck <- t(rbind(samplenames,batch))
tib.stuck <- as_tibble(stuck)


month <- sapply(full.table[,5],convertMonth)

dates <- paste(full.table[,6],month,sep="/")#as.data.frame()
date <- as.Date(dates,"%d/%m", tz = "UTC")
# date <- head(date,n=1000)


# newDates <- "1/1"

# info <- as.data.frame(cbind(date,stuck))
date.tib <- as_tibble(date)
info.tib <- cbind(date.tib,tib.stuck)
colnames(info.tib) <- c("date", "name", "batchF")

info.tib.distinct <- info.tib %>% distinct()


##
# info.tib <- as_tibble(info)
# plus.date <- tibble(date=as.Date(newDates,"%d/%m", tz = "UTC"), name="1",batch="BATCH1") %>%
  # rbind(info.tib)



# info <- unname(info)
# colnames(info) <- c("date", "name", "batch")
# newDate <- t(data.frame(
  # c(as.Date(newDates,"%d/%m", tz = "UTC"),"1","BATCH1")
  # ))
# newDate <- unname(newDate)
# newInfo <- rbind(info, newDate)

# SetCol <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf","#999999")
info.tib$batch<-  factor(info.tib$batchF , levels = c("BATCH1"  ,"BATCH2"  ,"BATCH3"  ,"BATCH4"  ,"BATCH5"  ,"BATCH6"  ,"BATCH7"  ,"BATCH8"  ,"BATCH9"  ,"BATCH10","BATCH11","BATCH12","BATCH13","BATCH14","BATCH15","BATCH16","BATCH17"))

info.tib <- info.tib[which(info.tib$name != "test"),]

# levels(info.tib$batch) <- 
pdf("VCF_birthday.pdf", height=4,width=10)
ggplot(head(n=13757,info.tib)) +
  geom_bar(aes(# position="dodge", 
    x = date,fill=batch
  )) +
  theme_bw() + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(11,"RdYlBu"))(17)) +
  # scale_fill_viridis() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))# +
  ylab("number of VCFs produced") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  scale_x_date(date_breaks = "1 week", date_labels =  "%b %d",date_minor_breaks = "1 week") + 
  guides(fill = guide_legend(nrow = 9))
  graphics.off()

  
  complete = tibble(batch=c("BATCH1"  ,"BATCH2"  ,"BATCH3"  ,"BATCH4"  ,"BATCH5"  ,"BATCH6"  ,"BATCH7"  ,"BATCH8"  ,"BATCH9"  ,"BATCH10","BATCH11","BATCH12","BATCH13","BATCH14","BATCH15","BATCH16","BATCH17"),total=c(500,1000,1000,1000,999,1000,999,1000,1000,1000,1000,1000,1000,1000,841,500,1000))
  complete$batch = factor(complete$batch , levels = c("BATCH1"  ,"BATCH2"  ,"BATCH3"  ,"BATCH4"  ,"BATCH5"  ,"BATCH6"  ,"BATCH7"  ,"BATCH8"  ,"BATCH9"  ,"BATCH10","BATCH11","BATCH12","BATCH13","BATCH14","BATCH15","BATCH16","BATCH17"))
  
  pdf(format(Sys.time(), "totals-%b-%d-%Y.pdf"), height=4,width=10)
  ggplot(as.data.frame(table(info.tib$batch))) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          legend.position = "none") +
    geom_col(data=complete, aes(x=batch,y=total)) +
    geom_col(aes(y=Freq, x=Var1,fill=Var1)) +
    ylab("number of VCFs produced") +
    scale_fill_manual(values = colorRampPalette(brewer.pal(11,"RdYlBu"))(18)) +
    xlab(date()) 
  graphics.off()
  
  
  
  
  
  ## Create a dataframe with number required to hit, to reach the target. 
  project.dates <- seq(as.Date("2019-05-20"),as.Date("2019-09-02"),1)
  project.num.output <- seq(5500,16000,(10500/(length(project.dates)-1)))
  project.df <- data.frame(date=project.dates,num=project.num.output)
  deleteme <- seq(0, nrow(project.df), 2)
  project.df <- project.df[-deleteme,]


   project.early.dates <- seq(as.Date("2019-05-20"),as.Date("2019-07-15"),1)
  project.early.num.output <- seq(5500,16000,(10500/(length(project.early.dates)-1)))
  project.early.df <- data.frame(date=project.early.dates,num=project.early.num.output)
  deleteme <- seq(0, nrow(project.early.df), 2)
  project.early.df <- project.early.df[-deleteme,]

  project.earlier.dates <- seq(as.Date("2019-06-1"),as.Date("2019-07-01"),1)
  project.earlier.num.output <- seq(7200,16000,(8800/(length(project.earlier.dates)-1)))
  project.earlier.df <- data.frame(date=project.earlier.dates,num=project.earlier.num.output)
  deleteme <- seq(0, nrow(project.earlier.df), 2)
  project.earlier.df <- project.earlier.df[-deleteme,]



  
  pdf("VCF_projection.pdf", height=3,width=10) 
  ggplot(info.tib) +
    annotate("rect",
             xmin=Sys.Date(),
             xmax=as.Date("2019-09-02"),
             ymin=0,
             ymax=16000,
             fill="cornflowerblue",
             alpha=0.5) +
    geom_line(data=project.df,aes(x=date,y=num),size=0.3,col="red")+
    # geom_line(data=project.early.df,aes(x=date,y=num),size=0.3,col="purple")+
    geom_line(data=project.earlier.df,aes(x=date,y=num),size=0.3,col="orange")+
    stat_bin(aes( x = date, y=cumsum(..count..)),
             geom="step",binwidth = 3) +
    theme_bw() + 
    scale_fill_manual(values = colorRampPalette(brewer.pal(9,"PuBuGn"))(10)) +
    # scale_fill_viridis() +
    # theme(axis.text.x = element_text(angle = 90, hjust = 1))# +
    ylab("cumulative number of VCFs produced") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d",date_minor_breaks = "1 week") 
  
    graphics.off()
  


