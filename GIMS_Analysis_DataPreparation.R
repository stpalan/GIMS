#####################################################################
# GIMS Output Analysis Script v0.2
#####################################################################

##### Preparing workspace and necessary packages

#install.packages("zTree")
#library("zTree")
require("zTree")
#source("D:/Institut/#CurrentWork/GIMS/zTree.R") # Loads R import script by Oliver Kirchkamp (https://www.kirchkamp.de//lab/zTree.html#zTreeR)

# SPTools -----------------------------------------------------------------
install.packages("devtools") #Installs necessary package
library("devtools") #Loads necessary package
install_github("stpalan/SPTools") #Installs necessary package
library("SPTools") #Loads necessary package


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

#Merges TreatmentPEAD into transactions table
Data$transactions<-merge(Data$transactions,Data$globals[,c("Date","TreatmentPEAD","Period")],by=c("Date","Period"))

#Reconstructs order book

library(foreach)
library(doParallel)
# NumCores<-6
registerDoParallel()
# 
# for(fRow in 1:2) {
#     print(
#         foreach(fCol=1:20,.combine=c) %dopar% {
#             fCol*2
#         })
# }

OB<-list() #Prepares order book list object

#Saves timings of offers and transactions
OB$Timings<-data.frame(
    R.Session=integer(),
    Period=integer(),
    Market=integer(),
    Time=numeric(),
    ID.Offer=integer(),
    ID.Transaction=integer(),
    stringsAsFactors=F
)
for (fS in unique(SPNum(Data$transactions$R.Session))){
    for (fP in unique(Data$transactions$Period[SPNum(Data$transactions$R.Session)==fS])){
        I<-Data$globals$I[SPNum(Data$globals$R.Session)==fS&Data$globals$Period==fP] #Checks value of globals.I variable in z-Tree output
        for (fM in (1:Data$globals$NumMarkets[SPNum(Data$globals$R.Session)==fS&Data$globals$Period==fP])){
            
            #Extracts timings and IDs from Data$offers
            Temp<-as.data.frame(matrix(c(
                #Writes times offers were created
                Data$offers$OfferTime[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM],
                #Writes times at which final offer status was set, if this status was not a transaction (since transaction times are added separately below)
                Data$offers$StatusTime[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM&Data$offers$StatusTime!=I&Data$offers$Status!=1],
                #Writes offer IDs for offer creation times
                Data$offers$ID[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM],
                #Writes offer IDs for status times, if this status was not a transaction (since transaction times are added separately below)
                Data$offers$ID[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM&Data$offers$StatusTime!=I&Data$offers$Status!=1]
                )
                ,ncol=2))
            colnames(Temp)<-c("Time","ID.Offer")
            Temp$R.Session<-fS
            Temp$Period<-fP
            Temp$Market<-fM
            Temp$ID.Transaction<-NA
            OB[["Timings"]]<-rbind(OB[["Timings"]],Temp)
            
            #Exctracts timings and IDs from Data$transactions
            Temp<-as.data.frame(matrix(c(
                #Writes times transactions occurred
                Data$transactions$Time[SPNum(Data$transactions$R.Session)==fS&Data$transactions$Period==fP&Data$transactions$Market==fM],
                #Writes transaction IDs
                Data$transactions$AcceptanceID[SPNum(Data$transactions$R.Session)==fS&Data$transactions$Period==fP&Data$transactions$Market==fM],
                #Writes offer IDs
                Data$transactions$OfferID[SPNum(Data$transactions$R.Session)==fS&Data$transactions$Period==fP&Data$transactions$Market==fM]
            )
            ,ncol=3))
            colnames(Temp)<-c("Time","ID.Transaction","ID.Offer")
            Temp$R.Session<-fS
            Temp$Period<-fP
            Temp$Market<-fM
            OB[["Timings"]]<-rbind(OB[["Timings"]],Temp)
        }

    }
}

OB$Timings<-OB$Timings[order(OB$Timings$R.Session,OB$Timings$Period,OB$Timings$Market,OB$Timings$Time,OB$Timings$ID.Offer),] #Sorts
OB$Timings$ID<-1:nrow(OB$Timings) #Generates a unique ID for each record in the table
OB$Timings$ID.inMarket<-as.data.frame(OB$Timings %>% group_by(OB$Timings$R.Session,OB$Timings$Period,OB$Timings$Market) %>% mutate(ID.inMarket = row_number()))$ID.inMarket #Generates an ID for events within a given market

#Saves complete order books

#Loops through all events and re-creates (step-by-step)
OB[["Book"]]<-list()
OB[["Portfolios"]]<-list()

for (fS in 1){#unique(SPNum(Data$transactions$R.Session))){
    OB$Books[[paste("S",fS,sep="")]]<-list() #Generates session list object
    OB$Portfolios[[paste("S",fS,sep="")]]<-list() #Generates session list object
    for (fP in unique(Data$transactions$Period[SPNum(Data$transactions$R.Session)==fS])){
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fS,sep="")]]<-list() #Generates period list object
        OB$Portfolios[[paste("S",fS,sep="")]][[paste("P",fS,sep="")]]<-list() #Generates period list object
        for (fM in (1:Data$globals$NumMarkets[SPNum(Data$globals$R.Session)==fS&Data$globals$Period==fP])){
            OB$Books[[paste("S",fS,sep="")]][[paste("P",fS,sep="")]][[paste("M",fM,sep="")]]<-list() #Generates market list object
            OB$Portfolios[[paste("S",fS,sep="")]][[paste("P",fS,sep="")]][[paste("M",fM,sep="")]]<-list() #Generates market list object
            for(fT in unique(OB$Timings$Time[OB$Timings$R.Session==fS&OB$Timings$Period==fP&OB$Timings$Market==fM])){ #Loops through all unique event times
                for (fID in OB$Timings$ID[OB$Timings$R.Session==fS&OB$Timings$Period==fP&OB$Timings$Market==fM&OB$Timings$Time==fT]){ #Loop through all IDs happening at the same time
                    if(fID==1){ #If this is the first event in this market, create empty order book
                        Temp<-data.frame(
                            ID.Offer=integer(),
                            ID.inMarket=integer(),
                            Type=integer(),
                            Price=numeric(),
                            Volume=integer()
                        )
                    } else { #else, copy order book from last event time
                        Temp<-OB$Books[[paste("S",fS,sep="")]][[paste("P",fS,sep="")]][[paste("M",fM,sep="")]][[paste("ID.inMarket",OB$Timings$ID.inMarket[OB$Timings$ID==fID-1],sep="")]]
                    }
                    Temp.ID.Offer<-OB$Timings$ID.Offer[OB$Timings$ID==fID]
                    Temp.Offer<-Data$offers[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM&Data$offers$ID==Temp.ID.Offer,]
                    if(Temp.Offer$OfferTime==fT){ #Offer was created
                        Temp[nrow(Temp)+1,]<-list( #Adds offer
                            ID.Offer=Temp.ID.Offer,
                            ID.inMarket=OB$Timings$ID.inMarket[OB$Timings$ID==fID],
                            Type=Temp.Offer$Type,
                            Price=Temp.Offer$Price,
                            Volume=Temp.Offer$Volume)
                    } else { #Offer status changed means offer is no longer valid (because it was transacted, cancelled, invalidated ...)
                            Temp<-Temp[Temp$ID.Offer!=Temp.ID.Offer,] #Deletes offer
                    }
                }
            }
            OB$Books[[paste("S",fS,sep="")]][[paste("P",fS,sep="")]][[paste("M",fM,sep="")]]$Book<-Temp
        }
    }
}

##### Cleanup
rm(i,Temp1)