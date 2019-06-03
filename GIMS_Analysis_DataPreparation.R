#####################################################################
# GIMS Output Analysis Script v0.2
#####################################################################

##### Preparing workspace and necessary packages

#install.packages("zTree")
#library("zTree")
require("zTree")
#source("D:/Institut/#CurrentWork/GIMS/zTree.R") # Loads R import script by Oliver Kirchkamp (https://www.kirchkamp.de//lab/zTree.html#zTreeR)


##### Data preparation

RawData<-zTreeTables(Params$SourceFiles,tables=Tables)    #Reads data
RawQuestionnaires<-zTreeSbj(Params$QSourceFiles)    #Reads questionnaire data
Data<-RawData # Generates working copy
Q<-RawQuestionnaires


# Adds R.SessionTreatmentPeriodID, R.SessionTreatmentID and R.Session variables to tables in RawData file to allow for identifying unique periods
for (i in Tables) {
    Data[[i]][,"R.PeriodID"]<-paste(Data[[i]]$Date,"_Tr",Data[[i]]$Treatment,"_P",Data[[i]]$Period,sep="")
    Data[[i]][,"R.TreatmentID"]<-paste(Data[[i]]$Date,"_Tr",Data[[i]]$Treatment,sep="")
}

# Generates lookup matrix which holds unique session, treatment and period IDs and numbers, and an indicator variable whether to include these periods in the data to be analyzed
Lookup<-matrix(Data$globals$R.PeriodID,ncol=1) # Generates variable to hold unique period identifier
Lookup<-cbind(Lookup,Data$globals$R.TreatmentID) # Generates variable to hold unique treatment identifier
Lookup<-cbind(Lookup,as.numeric(factor(Lookup[,1]))) # Adds period ID number
Lookup<-cbind(Lookup,as.numeric(factor(Lookup[,2]))) # Adds treatment ID number
Lookup<-cbind(Lookup,as.numeric(factor(Data$globals$Date))) # Adds session ID number
Lookup<-cbind(Lookup,Data$globals$PracticePeriod) # Adds binary column for which data to include in data, excluding \PracticePeriod=1 data
colnames(Lookup)<-c("R.PeriodID","R.TreatmentID","R.Period","R.Treatment","R.Session","R.PracticePeriod") # Names columns

# Adds variables from Lookup to tables, then selects only those rows where R.PracticePeriod==0 and writes the resulting table back into the data file, omitting the R.PracticePeriod variable
for (i in Tables) {
    Temp1<-merge(x=Data[[i]],y=subset(Lookup,select=-c(R.TreatmentID)), by.x="R.PeriodID", by.y="R.PeriodID") # Temporary variable containing table merged with Lookup, omitting variables occurring in both
    if(i %in% RemovePracticePeriodTables&Params$RemovePracticePeriods){Temp1<-Temp1[Temp1$R.PracticePeriod==0,]} # Removes practice period rows for tables of life period and treatment
    Data[[i]]<-Temp1 # Writes result into Data$table
}

# Merges questionnaires into subjects table
Data$subjects<-merge(Data$subjects,Q,by=c("Date","Subject"))

# Deletes experimenter subject(s) from subjects table
Data$subjects<-Data$subjects[Data$subjects$IsExperimenter==0,]

##### Cleanup
rm(i,Temp1)