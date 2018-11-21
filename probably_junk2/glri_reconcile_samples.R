## GLRI data public availability check:

library(dplyr)
library(readxl)
library(USGSHydroTools)



# Test cases for water quality retrieval
testParms <- c("31743","39630","00665")
names(testParms) <- c("Lachno","Atrazine","Phosphorus") #test a bacteria, pesticide, and nutrient

siteNames <- c("Milwaukee","Manitowoc","Menominee","Clinton","Rouge","Raisin","Maumee","Portage")
sites <- c("04087170", "04085427", "04067500", "04165500","04166500","04176500","04193500","04195500")
names(sites) <- siteNames

## GLRI:
load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
dfAll$STAID <- sites[dfAll$Site]

qw <- read_excel(file.path("raw","GLRI","QWsampleBacteria.xlsx"))
names(QW) <- make.names(names(QW))

dfAll <- filter(dfAll, !is.na(Lachno.2.cn.100ml))
dfAll <- dfAll[,-which(names(dfAll)=="psdate")]

dfAll$psdate

qw$psdate <- as.POSIXct(qw$SAMPLE_START_DT,format="%Y%m%d%H%M",tz="GMT")
qw$pedate <- as.POSIXct(qw$SAMPLE_END_DT,format="%Y%m%d%H%M",tz="GMT")
qw$ndate <- as.numeric(qw$psdate)

qw <- filter(qw,MEDIUM_CD == "WS")

# qw$psdate <- as.POSIXct(format(as.POSIXct(qw$psdate,tz="CST6CDT"),tz="UTC",usetz=TRUE),tz="GMT")
# qw[(qw$STAID %in% sites[4:8]),"psdate"] <- qw[(qw$STAID %in% sites[4:8]),"psdate"] - 3600

qw <- as.data.frame(arrange(qw,STAID,psdate))
dfAll <- as.data.frame(arrange(dfAll,STAID,GMTStartTime))

sum(dfAll$GMTStartTime %in% qw$psdate)

dfAllSample <- paste(dfAll$STAID,dfAll$GMTStartTime)
qwSample <- paste(qw$STAID,qw$psdate)

sum(dfAllSample %in% qwSample)

dfAllMissing <- dfAll[!(dfAllSample %in% qwSample),c("STAID","GMTStartTime")]
qwMissing <- qw[!(qwSample %in% dfAllSample),]



#dfAll$qwtime <- qw[match(dfAll$GMTStartTime,qw$psdate),"psdate"]

test <- merge(qw,dfAll,by.x = c("STAID","psdate","pedate"),by.y = c("STAID","GMTStartTime")) 
test <- filter(test,is.na(GMTStartTime))

test <- merge(qw,dfAll,by.x = c("pedate"),by.y = c("GMTEndTime")) 
sum(test$STAID.x != test$STAID.y)

test <- filter(test,is.na(GMTStartTime))


df <- full_join(qw,dfAll,by=c("STAID"="STAID","psdate"="GMTStartTime"))
test <- filter(df,is.na(GMTEndTime))
