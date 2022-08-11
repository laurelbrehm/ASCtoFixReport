### txt -> fixreport conversion
### converts extracted data from asc (after using bash script to get relevant lines) to fix report
### laurel brehm, summer 2022

library(tidyverse)

### get the files-- need to be in file path
##setwd()  ## put your path in here, if you aren't already in the directory of files
flist <- list.files(pattern="*out.txt")

cnames <- c("RECORDING_SESSION_LABEL","MSG","MSG_TIME","X1","EYE_USED","CURRENT_FIX_START","CURRENT_FIX_END","P","CURRENT_FIX_X","CURRENT_FIX_Y","X2","TRIAL_IMAGE")
ds <- read.table(flist[1],fill=T,col.names=cnames)

for (i in 2:length(flist)){
  dsa <- read.table(flist[i],fill=T,col.names=cnames)
  ds <- rbind(ds,dsa)
}

rm(dsa)

## substitutions to separate MSG_TIME for trial onset and screen 2 onset
## I'm using base R here because it's more compact and I find it readable here.
ds$CURRENT_FIX_MSG_LIST_TIME <- ""
ds[ds$X1=="!V",]$CURRENT_FIX_MSG_LIST_TIME <- ds[ds$X1=="!V",]$MSG_TIME
ds[ds$X1=="!V",]$MSG_TIME <- NA
ds[ds$X1!="!V",]$TRIAL_IMAGE <- NA


## then use tidy to fill in MSG_TIME for each trial, from onset
ds <- ds %>% fill(MSG_TIME)

## and fill in eye stuff for the synch screen onset
ds <- ds %>% fill(CURRENT_FIX_START)
ds <- ds %>% fill(CURRENT_FIX_END)
ds <- ds %>% fill(CURRENT_FIX_X)
ds <- ds %>% fill(CURRENT_FIX_Y)
ds <- ds %>% fill(EYE_USED)

## identify trials & associated trial images per participant
tt <- ds %>% group_by(RECORDING_SESSION_LABEL,MSG_TIME,TRIAL_IMAGE) %>% summarise() 
tt <- tt %>% drop_na()
tt2 <- ds %>% group_by(RECORDING_SESSION_LABEL) %>% summarise(TRIAL_INDEX=1,MSG_TIME=min(MSG_TIME))
tt <- merge(tt,tt2,all=T)
tt[is.na(tt)] = 0

ds$TRIAL_IMAGE <- NULL

for (i in 1:dim(tt)[1]){
  if(tt[i,4]==1) {
    j<-1
    } else {
    j<-j+1
    tt[i,4]<-j}
}

ds <- merge(ds,tt)

## drop the starting rows and drop unused/no longer useful columns.
ds <- ds %>% filter(X1 != '!MODE') %>% select(-c('X1','MSG',"P","X2"))

## and correct the time stamps
ds$CURRENT_FIX_START <- as.numeric(as.character(ds$CURRENT_FIX_START)) - as.numeric(as.character(ds$MSG_TIME))
ds$CURRENT_FIX_END <- as.numeric(as.character(ds$CURRENT_FIX_END)) - as.numeric(as.character(ds$MSG_TIME))

## fix the time stamp on synchtime message. 
ds$CURRENT_FIX_MSG_TEXT_1 <- 'SYNCTIME'
ds[ds$CURRENT_FIX_MSG_LIST_TIME=="",]$CURRENT_FIX_MSG_TEXT_1 <- "."
ds[ds$CURRENT_FIX_MSG_TEXT_1=='SYNCTIME',]$CURRENT_FIX_MSG_LIST_TIME <- as.numeric(as.character(ds[ds$CURRENT_FIX_MSG_TEXT_1=="SYNCTIME",]$CURRENT_FIX_MSG_LIST_TIME)) - 
                                                                    as.numeric(as.character(ds[ds$CURRENT_FIX_MSG_TEXT_1=="SYNCTIME",]$MSG_TIME)) 
#add brackets-- the brackets might be a weirdness based upon the version of dataviewer I used, and so line 71 might be better commented out?
ds$CURRENT_FIX_MSG_LIST_TIME <- paste0('[',ds$CURRENT_FIX_MSG_LIST_TIME ,']')                                                                                                                    

## fix eye used
ds$EYE_USED <- gsub('R',"RIGHT",ds$EYE_USED)
ds$EYE_USED <- gsub('L',"LEFT",ds$EYE_USED)

##reorder as I want them-- you might change this!
ds <- ds %>% select('RECORDING_SESSION_LABEL',"TRIAL_INDEX","CURRENT_FIX_X","CURRENT_FIX_Y","CURRENT_FIX_START","CURRENT_FIX_END","TRIAL_IMAGE","EYE_USED","CURRENT_FIX_MSG_TEXT_1","CURRENT_FIX_MSG_LIST_TIME")

##output  (change your file name as wanted!)
write.table(ds,'fixreport.txt',quote=F,sep="\t",row.names = F)
