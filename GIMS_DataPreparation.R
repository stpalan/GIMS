##################################################
# GIMS Output Analysis Script v0.1 DataPreparation
##################################################

##### Preparing workspace and necessary packages

require("zTree")


##### Data preparation

RawQuestionnaires<-zTreeSbj(QSourceFiles)    #Reads questionnaire data
RawData<-zTreeTables(SourceFiles,tables=Tables)    #Reads data
Data<-RawData    #Generates working copy
Q<-RawQuestionnaires    #Generates working copy



# Adds R.SessionTreatmentPeriodID, R.SessionTreatmentID and R.Session variables to tables in RawData file to allow for identifying unique periods
for (i in Tables) {
    Data[[i]][,"R.PeriodID"]<-paste(Data[[i]]$Date,"_Tr",Data[[i]]$Treatment,"_P",Data[[i]]$Period,sep="")
    Data[[i]][,"R.TreatmentID"]<-paste(Data[[i]]$Date,"_Tr",Data[[i]]$Treatment,sep="")
}


# Generates lookup matrix which holds unique session, treatment and period IDs and numbers, and an indicator variable whether to include these periods in the data to be analyzed
Lookup<-matrix(Data$globals$R.PeriodID,ncol=1,dimnames=list(1:length(Data$globals$R.PeriodID),"R.PeriodID"))    #Generates variable to hold unique period identifier
Lookup<-cbind(Lookup,R.TreatmentID=Data$globals$R.TreatmentID)   #Generates variable to hold unique treatment identifier
Lookup<-cbind(Lookup,R.Period=as.numeric(factor(Lookup[,1])))   #Adds period ID number
Lookup<-cbind(Lookup,R.Treatment=as.numeric(factor(Lookup[,2])))   #Adds treatment ID number
Lookup<-cbind(Lookup,R.Session=as.numeric(factor(Data$globals$Date)))   #Adds session ID number

# Adds variables from Lookup to tables, then selects only those rows where R.PracticePeriod==1 and writes the resulting table back into the data file, omitting the R.PracticePeriod variable
for (i in Tables) {
    Temp1<-merge(x=Data[[i]],y=subset(Lookup,select=-c(R.TreatmentID)), by.x="R.PeriodID", by.y="R.PeriodID")    #Temporary variable containing table merged with Lookup, omitting variables occurring in both
    if(i %in% RemovePracticePeriodTables&RemovePracticePeriods){Temp1<-Temp1[!(Temp1$R.PracticePeriod==1|is.na(Temp1$R.PracticePeriod)),]}    #Removes practice period rows for tables of life period and treatment
    Data[[i]]<-Temp1    #Writes result into Data$table
}

# Deletes experimenter subject(s) from subjects table
Data$subjects<-Data$subjects[Data$subjects$IsExperimenter==0,]
Q<-Q[Q$Subject!=max(Q$Subject),]
Q[,"R.Session"]<-rep(1:(nrow(Q)/max(Q$Subject)),each=max(Q$Subject))

# Merges questionnaires into subjects table
Data$subjects<-merge(Data$subjects,Q,by=c("R.Session","Subject"))


##### Cleanup
rm(i,Temp1)