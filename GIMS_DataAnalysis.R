###########################################
# GIMS Output Analysis Script v0.1 Analysis
###########################################

rm(list=ls())    #Clears memory
graphics.off()    #Clears graphs


##### Parameters

#~~~ Specify full path and file name of source files ~~~#
SourceFiles<-list.files("C:/FilesForAnalysis/.",pattern="^[0-9]{6}_[0-9]{4}.xls",full.names=T,recursive=F)
QSourceFiles<-list.files("C:/FilesForAnalysis/.",pattern="^[0-9]{6}_[0-9]{4}.sbj",full.names=T,recursive=F)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Tables<-c("globals","subjects","transactions", "timelog")    #Specify tables which should be read
RemovePracticePeriodTables<-Tables[!Tables=="session"]    #Subset of tables which have life < session
RemovePracticePeriods<-T   #Switch to determine whether practice period information should be omitted or not


##### Read in data
source("C:/FilesForAnalysis/GIMS_v7.5.0_DataPrepraration.r")


##### Analysis