#####################################################################
# GIMS Output Analysis Script v0.2
#####################################################################

##### Preparing workspace and necessary packages

#install.packages("zTree","dplyr")
library("zTree")
library("dplyr")
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

#Merges TreatmentPEAD into transactions table
Data$transactions<-merge(Data$transactions,Data$globals[,c("Date","TreatmentPEAD","Period")],by=c("Date","Period"))

#Merges TreatmentPEAD into remaining tables (except globals, transactions, and session)
for (i in Tables[Tables!="globals" & Tables!="transactions" & Tables!="session"]) {
  Data[[i]]<-merge(Data[[i]], Data$globals[,c("Date","TreatmentPEAD","Period")],by=c("Date","Period"))
}

# Adds R.SessionTreatmentPeriodID, R.SessionTreatmentID and R.Session variables to tables in RawData file to allow for identifying unique periods
for (i in Tables) {
  Data[[i]][,"R.PeriodID"]<-paste(Data[[i]]$Date,"_Tr",Data[[i]]$TreatmentPEAD,"_P",Data[[i]]$Period,sep="")
  Data[[i]][,"R.TreatmentID"]<-paste(Data[[i]]$Date,"_Tr",Data[[i]]$TreatmentPEAD,sep="")
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
  Temp1<-merge(x=subset(Data[[i]], select=-c(R.Period, R.Treatment, R.Session, R.PracticePeriod)),y=subset(Lookup,select=-c(R.TreatmentID)), by.x="R.PeriodID", by.y="R.PeriodID") # Temporary variable containing table merged with Lookup, omitting variables occurring in both
  if(i %in% RemovePracticePeriodTables&Params$RemovePracticePeriods){
    Temp1<-Temp1[Temp1$R.PracticePeriod==0,]} # Removes practice period rows for tables of life period and treatment
  Data[[i]]<-Temp1 # Writes result into Data$table
}

# Merges questionnaires into subjects table
Data$subjects<-merge(Data$subjects,Q,by=c("Date","Subject"))

# Deletes experimenter subject(s) from subjects table
Data$subjects<-Data$subjects[Data$subjects$IsExperimenter==0,]


#Reconstructs order book

#library(foreach)
#library(doParallel)
# NumCores<-6
#registerDoParallel()
#
# for(fRow in 1:2) {
#     print(
#         foreach(fCol=1:20,.combine=c) %dopar% {
#             fCol*2
#         })
# }

OB<-list() #Prepares order book list object

#Prepares function to format timing information uniformly
UTime<-function(Time){
  return(as.character(formatC(digits=3,width=9,format="f",flag="0",x=Time)))
}

#Saves timings of offers and transactions
OB$Timings<-data.frame(
  R.Session=integer(),
  Period=integer(),
  Market=integer(),
  Time=character(),
  ID.Offer=integer(),
  ID.Transaction=integer(),
  stringsAsFactors=F
)
for (fS in unique(SPNum(Data$transactions$R.Session))){
  for (fP in unique(Data$transactions$Period[SPNum(Data$transactions$R.Session)==fS])){
    I<-Data$globals$I[SPNum(Data$globals$R.Session)==fS&Data$globals$Period==fP] #Checks value of globals.I variable in z-Tree output
    for (fM in (1:Data$globals$NumMarkets[SPNum(Data$globals$R.Session)==fS&Data$globals$Period==fP])){
      
      #Extracts timings and IDs from Data$offers
      Temp<-as.data.frame(list(
        Time=c(
          #Writes times offers were created
          UTime(Data$offers$OfferTime[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM]),
          #Writes times at which final offer status was set, if this status was not a transaction (since transaction times are added separately below)
          UTime(Data$offers$StatusTime[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM&Data$offers$StatusTime!=I&Data$offers$Status!=1])
        ),
        #Writes offer IDs for offer creation times
        ID.Offer=c(
          Data$offers$ID[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM],
          #Writes offer IDs for status times, if this status was not a transaction (since transaction times are added separately below)
          Data$offers$ID[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM&Data$offers$StatusTime!=I&Data$offers$Status!=1]
        )
      )
      )
      #colnames(Temp)<-c("Time","ID.Offer")
      Temp$R.Session<-fS
      Temp$Period<-fP
      Temp$Market<-fM
      Temp$ID.Transaction<-NA
      OB[["Timings"]]<-rbind(OB[["Timings"]],Temp)
      
      #Exctracts timings and IDs from Data$transactions
      Temp<-as.data.frame(matrix(c(
        #Writes times transactions occurred
        UTime(Data$transactions$Time[SPNum(Data$transactions$R.Session)==fS&Data$transactions$Period==fP&Data$transactions$Market==fM]),
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

OB$Timings$Time<-as.character(OB$Timings$Time)
OB$Timings<-OB$Timings[order(OB$Timings$R.Session,OB$Timings$Period,OB$Timings$Market,OB$Timings$Time,OB$Timings$ID.Offer),] #Sorts
OB$Timings$ID<-1:nrow(OB$Timings) #Generates a unique ID for each record in the table
OB$Timings$ID.inMarket<-as.data.frame(OB$Timings %>% group_by(OB$Timings$R.Session,OB$Timings$Period,OB$Timings$Market) %>% mutate(ID.inMarket = row_number()))$ID.inMarket #Generates an ID for events within a given market

#Saves complete order books

#Loops through all events and re-creates (step-by-step)
OB[["Books"]]<-list()
OB[["Subjects"]]<-list()

for (fS in unique(SPNum(Data$transactions$R.Session))){
  OB$Books[[paste("S",fS,sep="")]]<-list() #Generates session list object
  OB$Subjects[[paste("S",fS,sep="")]]<-list() #Generates session list object
  for (fP in unique(Data$transactions$Period[SPNum(Data$transactions$R.Session)==fS])){
    OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]]<-list() #Generates period list object
    OB$Subjects[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]]<-list() #Generates period list object
    for (fM in (1:Data$globals$NumMarkets[SPNum(Data$globals$R.Session)==fS&Data$globals$Period==fP])){
      OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("M",fM,sep="")]]<-list() #Generates market list object
      OB$Subjects[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("M",fM,sep="")]]<-list() #Generates market list object
      for(fT in UTime(unique(OB$Timings$Time[OB$Timings$R.Session==fS&OB$Timings$Period==fP&OB$Timings$Market==fM]))){ #Loops through all unique event times (formatting time consistently throughout)
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("M",fM,sep="")]][[paste("T",fT,sep="")]]<-list() #Generates time list object
        OB$Subjects[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("M",fM,sep="")]][[paste("T",fT,sep="")]]<-list() #Generates time list object
        for (fID in OB$Timings$ID[OB$Timings$R.Session==fS&OB$Timings$Period==fP&OB$Timings$Market==fM&OB$Timings$Time==fT]){ #Loop through all IDs happening at the same time
          if(fT==UTime(unique(OB$Timings$Time[OB$Timings$R.Session==fS&OB$Timings$Period==fP&OB$Timings$Market==fM])[1])){ #If this is the first event in this market, fill Templ with empty order book template
            #Prepares temporary dataframe for order book data
            Temp<-data.frame(
              ID.Offer=integer(),
              ID.inMarket=integer(),
              Type=integer(),
              Price=numeric(),
              Volume=integer()
            )
            
            #Prepares temporary dataframe for subjects data
            Temp.Subjects<-data.frame(ID=1:length(Data$subjects$Subject[SPNum(Data$subjects$R.Session)==fS&Data$subjects$Period==fP]))
            Temp.Subjects$Cash<-Data$subjects$InitialCash[SPNum(Data$subjects$R.Session)==fS&Data$subjects$Period==fP][order(Data$subjects$Subject[SPNum(Data$subjects$R.Session)==fS&Data$subjects$Period==fP])]
            #for(f.M in 1:Data$globals$NumMarkets[SPNum(Data$globals$R.Session)==fS&Data$globals$Period==fP]){ #Generates variables for number of assets held in the different markets
            Temp.Subjects$Assets<-Data$subjects[,paste("InitialAssets[",fM,"]",sep="")][SPNum(Data$subjects$R.Session)==fS&Data$subjects$Period==fP][order(Data$subjects$Subject[SPNum(Data$subjects$R.Session)==fS&Data$subjects$Period==fP])]
            #}
          }
          
          Temp.ID.Offer<-OB$Timings$ID.Offer[OB$Timings$ID==fID] #Exctract offer ID of current offer
          Temp.Offer<-Data$offers[SPNum(Data$offers$R.Session)==fS&Data$offers$Period==fP&Data$offers$Market==fM&Data$offers$ID==Temp.ID.Offer,] #Extract entire offers table entry
          if(UTime(Temp.Offer$OfferTime)==fT){ #Offer was created
            Temp[nrow(Temp)+1,]<-list( #Adds offer
              ID.Offer=Temp.ID.Offer,
              ID.inMarket=OB$Timings$ID.inMarket[OB$Timings$ID==fID],
              Type=Temp.Offer$Type,
              Price=Temp.Offer$Price,
              Volume=Temp.Offer$Volume)
          } else { #Offer status changed means offer is no longer valid (because it was transacted, cancelled, invalidated ...)
            Temp<-Temp[Temp$ID.Offer!=Temp.ID.Offer,] #Deletes offer
            if(Temp.Offer$Status==1){ #Processes change in subjects' holdings if offer was traded
              Temp.Transaction<-Data$transactions[SPNum(Data$transactions$R.Session)==fS&Data$transactions$Period==fP&Data$transactions$Market==fM&Data$transactions$OfferID==Temp.Offer$ID,] #Finds transaction
              Temp.Subjects$Assets[Temp.Subjects$ID==Temp.Offer$Offerer]<-Temp.Subjects$Assets[Temp.Subjects$ID==Temp.Offer$Offerer]+Temp.Offer$Type*Temp.Transaction$Volume #Reflects change in number of assets for offerer
              Temp.Subjects$Assets[Temp.Subjects$ID==Temp.Transaction$AccepterID]<-Temp.Subjects$Assets[Temp.Subjects$ID==Temp.Transaction$AccepterID]-Temp.Offer$Type*Temp.Transaction$Volume #Reflects change in number of assets for accepter
              Temp.Subjects[Temp.Subjects$ID==Temp.Offer$Offerer,"Cash"]<-Temp.Subjects[Temp.Subjects$ID==Temp.Offer$Offerer,"Cash"]-Temp.Offer$Type*Temp.Transaction$Volume*Temp.Transaction$Price #Reflects change in cash for offerer
              Temp.Subjects[Temp.Subjects$ID==Temp.Transaction$AccepterID,"Cash"]<-Temp.Subjects[Temp.Subjects$ID==Temp.Transaction$AccepterID,"Cash"]+Temp.Offer$Type*Temp.Transaction$Volume*Temp.Transaction$Price #Reflects change in cash for accepter
            }
          }
        }
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("M",fM,sep="")]][[paste("T",fT,sep="")]]$Book<-Temp #Writes order book to OB
        OB$Subjects[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("M",fM,sep="")]][[paste("T",fT,sep="")]]<-Temp.Subjects #Writes order book to OB
        
        Temp.Summary<-data.frame(Time=fT) #Prepares temporary summary variable
        Temp.Summary<-within(Temp.Summary,{ #Fills in summary data
          Bid.best<-ifelse(length(Temp$Price[Temp$Type==1])==0,NA,max(Temp$Price[Temp$Type==1]))
          Ask.best<-ifelse(length(Temp$Price[Temp$Type==-1])==0,NA,min(Temp$Price[Temp$Type==-1]))
          Bid.depth<-sum(Temp$Volume[Temp$Type==1&Temp$Price==Bid.best])
          Ask.depth<-sum(Temp$Volume[Temp$Type==-1&Temp$Price==Ask.best])
          Midpoint<-(Bid.best+Ask.best)/2
          Spread.ECU<-Ask.best-Bid.best
          Spread.pct<-ifelse(is.na(Spread.ECU),NA,Spread.ECU/Midpoint)
        })
        
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("M",fM,sep="")]][[paste("T",fT,sep="")]]$Summary<-Temp.Summary #Writes summary to OB
      }
    }
  }
}

#Generate Arbitrage data
for (fS in unique(SPNum(Data$transactions$R.Session))){
  for (fP in unique(Data$transactions$Period[SPNum(Data$transactions$R.Session)==fS])){
    for(fT in UTime(unique(OB$Timings$Time[OB$Timings$R.Session==fS&OB$Timings$Period==fP]))[order(as.numeric(UTime(unique(OB$Timings$Time[OB$Timings$R.Session==fS&OB$Timings$Period==fP]))))]){ #Loops through all unique event times in order (formatting time consistently throughout)
      #Extract, for each market, the name of the last order book element before or at the time of fT (current for loop value)
      Temp.M1time<-paste("T",tail(substr(names(OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M1"]]),start=2,stop=10)[as.numeric(substr(names(OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M1"]]),start=2,stop=10))<=as.numeric(fT)],n=1),sep="")
      Temp.M2time<-paste("T",tail(substr(names(OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M2"]]),start=2,stop=10)[as.numeric(substr(names(OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M2"]]),start=2,stop=10))<=as.numeric(fT)],n=1),sep="")
      
      #Calculates sum of midpoints, best asks and bids in both markets
      Temp.Midpoint.sum=
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M1"]][[Temp.M1time]][["Summary"]]$Midpoint+
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M2"]][[Temp.M2time]][["Summary"]]$Midpoint
      Temp.Bid.best.sum=
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M1"]][[Temp.M1time]][["Summary"]]$Bid.best+
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M2"]][[Temp.M2time]][["Summary"]]$Bid.best
      Temp.Ask.best.sum=
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M1"]][[Temp.M1time]][["Summary"]]$Ask.best+
        OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][["M2"]][[Temp.M2time]][["Summary"]]$Ask.best
      
      #Writes them to OB if at least one of them exists
      if(!length(Temp.Midpoint.sum)==0){ #Tests if one of the markets does not have any event yet
        #if(!(is.na(Temp.Midpoint.sum)&is.na(Temp.Bid.best.sum)&is.na(Temp.Ask.best.sum))){ #Tests if at least one value has been filled
          OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("A",fT,sep="")]]<-data.frame( #Writes object
            list(
              Midpoint.sum=Temp.Midpoint.sum,
              Bid.best.sum=Temp.Bid.best.sum,
              Ask.best.sum=Temp.Ask.best.sum
            )
          )
        #}
      } else {
      OB$Books[[paste("S",fS,sep="")]][[paste("P",fP,sep="")]][[paste("A",fT,sep="")]]<-data.frame( #Writes object
        list(
          Midpoint.sum=NA,
          Bid.best.sum=NA,
          Ask.best.sum=NA
        )
      )
      }
    }
  }
}


##### Cleanup
rm(i,Temp1)

Temp<-0
##### Testing
for (fS in names(OB$Subjects)){
  for (fP in names(OB$Subjects[[fS]])){
    for (fM in names(OB$Subjects[[fS]][[fP]])){
      for(fT in names(OB$Subjects[[fS]][[fP]][[fM]])){
        Temp<-min(c(Temp,OB$Subjects[[fS]][[fP]][[fM]][[fT]]$Assets))
        if(Temp<(-9)){stop()}
      }
    }
  }
}
Temp