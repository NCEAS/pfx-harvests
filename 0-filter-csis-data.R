#Filter subsistence harvest data

#Set working directory and read in data
wd<-setwd("C:/Users/abeaudreau/Documents/Harvest Portfolio Subgroup/Analysis")
all.data<-read.csv("CSIS_Original.csv")
proj.list<-read.csv("CSIS_ProjectList.csv")

#Add samp.type and spp.type columns to all.data
all.data<-merge(all.data,proj.list,by="comm_yr_id")
all.data<-all.data[,-c(43)]

#Select only samp.type not equal to "OPP" and baseline equal to 1
select.data<-all.data[!all.data$samp.type=="OPP" & all.data$baseline==1,]

#Select taxa
select.data$rescode.top<-as.numeric(substr(select.data$rescode,3,4))
select.data$rescode.bottom<-as.numeric(substr(select.data$rescode,5,9))
	select.data$rescode.top[is.na(select.data$rescode.top)]=0
	select.data$rescode.bottom[is.na(select.data$rescode.bottom)]=0
select.data<-select.data[select.data$rescode.top>0 & select.data$rescode.bottom==0,]

#Recalculate xtotlbs and percap
select.data$xtotlbs_recalc<-select.data$avglbhrv*as.numeric(as.character(select.data$commhh))
select.data$percap_recalc<-select.data$xtotlbs_recalc/as.numeric(as.character(select.data$commpop))

#Export file
write.csv(select.data,"SelectData.csv")
