#####################################################################
# PEAD Analysis Script
#####################################################################

rm(list=ls()) # Clears memory
graphics.off() # Closes all plot windows

##### Parameters

Params<-list()
#~~~ Specify full path and file name of source files ~~~#
#SourceFiles<-c("D:/Institut/#CurrentWork/WhatIsRiskMarket/Results/161212_1032_1_R.xls","D:/Institut/#CurrentWork/WhatIsRiskMarket/Results/161212_1032_2_R.xls","D:/Institut/#CurrentWork/WhatIsRiskMarket/Results/161212_1032_3_R.xls")
Params$SourceFiles<-list.files("D:/Institut/#CurrentWork/PEAD/Results/FilesForAnalysis.",pattern="[0-9]{6}_[0-9]{4}.xls",full.names=T,recursive=F)
Params$QSourceFiles<-list.files("D:/Institut/#CurrentWork/PEAD/Results/FilesForAnalysis.",pattern="[0-9]{6}_[0-9]{4}.sbj",full.names=T,recursive=F)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Tables<-c("globals","PEADsignals","subjects","summary","timelog","transactions", "contracts", "dividends", "endowments", "offers", "results", "marketsummary", "session", "layout", "profit")
RemovePracticePeriodTables<-Tables[!Tables=="session"] # Subset of tables which the practice period should be removed from. This includes only tables which have life < session.

Params$ShowPlots<-F #Should plots be shown on screen or only written to disk?
Params$UpdateData<-F #Should data be newly read-in?
Params$RemovePracticePeriods<-T
Params$PlotFileType<-"jpeg"
setwd("d:/institut/#CurrentWork/PEAD/Results")

if(Params$UpdateData){
  source("D:/Institut/#CurrentWork/GIMS/GitLab/GIMS/GIMS_Analysis_DataPreparation.r") # Reads in and prepares data
  save.image("PEAD.Rdata")
} else {load("PEAD.Rdata")}

################################### R01 ##########################################

### Plot period prices

### Prepares transactions table
Lookup<-merge(Lookup["R.PeriodID"],Data$globals[,c("R.PeriodID","StartTime","StartTimeCDA")])[,-1] # Generates matrix containing R.PeriodID and several variables from the globals table
Data$transactions<-merge(Data$transactions,Lookup) # Adds additional information to transactions table
Data$transactions<-cbind(Data$transactions, R.TradeTime=Data$transactions$Time-(Data$transactions$StartTimeCDA-Data$transactions$StartTime))# Adds precise timing for trades

NumSessions<-max(as.numeric(Data$transactions$R.Session))
NumMarkets<-Data$globals$NumMarkets[1]

### Signal of Stock A is 'Up' or 'Down' in Data$PEADsignals
Data$PEADsignals[,"SignalDirectionA"]<-"Down"
for (rownum in 1:nrow(Data$PEADsignals)) {
  if (Data$PEADsignals[rownum,"Set"]==1 & Data$PEADsignals[rownum,"PEADValueA"]>5) {
    Data$PEADsignals[rownum,"SignalDirectionA"]<-"Up"
  } else if (Data$PEADsignals[rownum,"Set"]>1 & Data$PEADsignals[rownum,"PEADValueA"]>Data$PEADsignals[rownum-1,"PEADValueA"]) {
    Data$PEADsignals[rownum,"SignalDirectionA"]<-"Up"
  } 
}

### Adds FV to PEADSignals
Data$PEADsignals[,c("FV.A", "FV.B")]<-0
for (rownum in 1:nrow(Data$PEADsignals)) {
  if (Data$PEADsignals[rownum,"TreatmentPEAD"]==1) {
    Data$PEADsignals[rownum,"FV.A"]<-Data$PEADsignals[rownum,"PEADValueA"]*FV.Factor
  } else {
    Data$PEADsignals[rownum,"FV.A"]<-FV$Correl$FV[FV$Correl$Earnings==Data$PEADsignals[rownum,"PEADValueA"]&
                                                    FV$Correl$Announcement==Data$PEADsignals[rownum,"Set"]&
                                                    FV$Correl$ID%%2==ifelse(Data$PEADsignals[rownum,"SignalDirectionA"]=="Up",0,1)][1]
  }
}
Data$PEADsignals[,"FV.B"]<-200-Data$PEADsignals[,"FV.A"]

### Plot period prices

XLIM<-c(min(Data$transactions$R.TradeTime),max(Data$transactions$R.TradeTime))
YLIM<-c(50,150)#c(min(Data$transactions$Price),max(Data$transactions$Price))
LWD<-1.5

Temp1<-0
for (Session in 1:NumSessions) {
  if(Params$ShowPlots){dev.new("PricePlot")} else {jpeg(paste("PricePlot_S",Session,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
  par(mfrow=c(2,2))
  
  for(Period in 1:4){
    
    # Plot if period exists and saw trade
    if(nrow(Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Period==Period,])>0){
      
      # Calculates announcement times
      {
        Temp1<-Temp1+1
        CDAStart<-Data$timelog$Time[Data$timelog$R.Session==Session&Data$timelog$Period==Period&Data$timelog$Event==2]
        
        
        if(Temp1==1){
          Temp2<-c("R.PeriodID","R.Session","Period","R.Time1","R.Time2","R.Time3","R.Time4","R.Value0","R.ValueA1","R.ValueA2","R.ValueA3","R.ValueA4","R.ValueB1","R.ValueB2","R.ValueB3","R.ValueB4")
          AnnouncementData<-data.frame(matrix(-77777,nrow=1,ncol=length(Temp2),dimnames=list(1,Temp2)))
        }
        
        if(Temp1>1){
          AnnouncementData<-rbind(AnnouncementData,rep(-77777,length(Temp2)))
        }
        AnnouncementData[Temp1,"R.PeriodID"]<-Data$timelog$R.PeriodID[Data$timelog$R.Session==Session&Data$timelog$Period==Period][1]
        AnnouncementData[Temp1,"R.Session"]<-Session
        AnnouncementData[Temp1,"Period"]<-Period
        AnnouncementData[Temp1,"R.Time1"]<-round(Data$timelog$Time[Data$timelog$R.Session==Session&Data$timelog$Period==Period&Data$timelog$Event==3013]-CDAStart,3)
        AnnouncementData[Temp1,"R.Time2"]<-round(Data$timelog$Time[Data$timelog$R.Session==Session&Data$timelog$Period==Period&Data$timelog$Event==3014]-CDAStart,3)
        AnnouncementData[Temp1,"R.Time3"]<-round(Data$timelog$Time[Data$timelog$R.Session==Session&Data$timelog$Period==Period&Data$timelog$Event==3015]-CDAStart,3)
        AnnouncementData[Temp1,"R.Time4"]<-round(Data$timelog$Time[Data$timelog$R.Session==Session&Data$timelog$Period==Period&Data$timelog$Event==3016]-CDAStart,3)
        AnnouncementData[Temp1,"R.Value0"]<-100
        AnnouncementData[Temp1,"R.ValueA1"]<-Data$PEADsignals[,"FV.A"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==1]
        AnnouncementData[Temp1,"R.ValueA2"]<-Data$PEADsignals[,"FV.A"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==2]
        AnnouncementData[Temp1,"R.ValueA3"]<-Data$PEADsignals[,"FV.A"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==3]
        AnnouncementData[Temp1,"R.ValueA4"]<-Data$PEADsignals[,"FV.A"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==4]
        AnnouncementData[Temp1,"R.ValueB1"]<-Data$PEADsignals[,"FV.B"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==1]
        AnnouncementData[Temp1,"R.ValueB2"]<-Data$PEADsignals[,"FV.B"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==2]
        AnnouncementData[Temp1,"R.ValueB3"]<-Data$PEADsignals[,"FV.B"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==3]
        AnnouncementData[Temp1,"R.ValueB4"]<-Data$PEADsignals[,"FV.B"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period&Data$PEADsignals$Set==4]
      }
    }
    
    plot(x=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==1&Data$transactions$Period==Period,][,"R.TradeTime"],y=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==1&Data$transactions$Period==Period,][,"Price"], type="l", col=4, lwd=LWD, xlim=XLIM, ylim=YLIM, xlab="Time (seconds)", ylab="Price (Taler)", main=paste("Treatment ",Data$globals$TreatmentPEAD[Data$globals$R.Session==Session&Data$globals$Period==Period],", Session ",Session,", Period ",Period,sep="")) # Plots market 1
    lines(x=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==2&Data$transactions$Period==Period,][,"R.TradeTime"],y=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==2&Data$transactions$Period==Period,][,"Price"], type="l", col=3, lwd=LWD) # Plots market 2
    abline(v=AnnouncementData[Temp1,c("R.Time1","R.Time2","R.Time3","R.Time4")]) # Adds announcement times
    abline(v=XLIM,lwd=2) # Adds period start and end
    lines(x=c(XLIM[1],AnnouncementData[Temp1,"R.Time1"]),y=c(AnnouncementData[Temp1,"R.Value0"],AnnouncementData[Temp1,"R.Value0"])) # Fundamental value before first announcement
    lines(x=c(AnnouncementData[Temp1,"R.Time1"],AnnouncementData[Temp1,"R.Time2"]),y=c(AnnouncementData[Temp1,"R.ValueA1"],AnnouncementData[Temp1,"R.ValueA1"]),col=if(AnnouncementData[Temp1,"R.ValueA1"]==100){"black"}else{4}) # Fundamental value after first announcement, asset A
    lines(x=c(AnnouncementData[Temp1,"R.Time2"],AnnouncementData[Temp1,"R.Time3"]),y=c(AnnouncementData[Temp1,"R.ValueA2"],AnnouncementData[Temp1,"R.ValueA2"]),col=if(AnnouncementData[Temp1,"R.ValueA2"]==100){"black"}else{4}) # Fundamental value after second announcement, asset A
    lines(x=c(AnnouncementData[Temp1,"R.Time3"],AnnouncementData[Temp1,"R.Time4"]),y=c(AnnouncementData[Temp1,"R.ValueA3"],AnnouncementData[Temp1,"R.ValueA3"]),col=if(AnnouncementData[Temp1,"R.ValueA3"]==100){"black"}else{4}) # Fundamental value after third announcement, asset A
    lines(x=c(AnnouncementData[Temp1,"R.Time4"],XLIM[2]),y=c(AnnouncementData[Temp1,"R.ValueA4"],AnnouncementData[Temp1,"R.ValueA4"]),col=if(AnnouncementData[Temp1,"R.ValueA4"]==100){"black"}else{4}) # Fundamental value after fourth announcement, asset A
    lines(x=c(AnnouncementData[Temp1,"R.Time1"],AnnouncementData[Temp1,"R.Time2"]),y=c(AnnouncementData[Temp1,"R.ValueB1"],AnnouncementData[Temp1,"R.ValueB1"]),col=if(AnnouncementData[Temp1,"R.ValueB1"]==100){"black"}else{3}) # Fundamental value after first announcement, asset B
    lines(x=c(AnnouncementData[Temp1,"R.Time2"],AnnouncementData[Temp1,"R.Time3"]),y=c(AnnouncementData[Temp1,"R.ValueB2"],AnnouncementData[Temp1,"R.ValueB2"]),col=if(AnnouncementData[Temp1,"R.ValueB2"]==100){"black"}else{3}) # Fundamental value after second announcement, asset B
    lines(x=c(AnnouncementData[Temp1,"R.Time3"],AnnouncementData[Temp1,"R.Time4"]),y=c(AnnouncementData[Temp1,"R.ValueB3"],AnnouncementData[Temp1,"R.ValueB3"]),col=if(AnnouncementData[Temp1,"R.ValueB3"]==100){"black"}else{3}) # Fundamental value after third announcement, asset B
    lines(x=c(AnnouncementData[Temp1,"R.Time4"],XLIM[2]),y=c(AnnouncementData[Temp1,"R.ValueB4"],AnnouncementData[Temp1,"R.ValueB4"]),col=if(AnnouncementData[Temp1,"R.ValueB4"]==100){"black"}else{3}) # Fundamental value after fourth announcement, asset B
  }
  if(Params$ShowPlots){
    dev.copy(jpeg,paste("PricePlot_T",Data$globals$TreatmentPEAD[Data$globals$R.Session==Session&Data$globals$Period==Period],"S",Session,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
  }
  dev.off() # Turns off graphics device
}

################################### R02 ##########################################

### Prepare data to plot PEAD

### Adds additional data to transactions table

Data$transactions<-merge(x=Data$transactions,y=Data$contracts[,c("R.PeriodID","Market","Period","TransactionID","BestBid","BestAsk")],by.x=c("R.PeriodID","Market","Period","AcceptanceID"),by.y=c("R.PeriodID","Market","Period","TransactionID")) # Adds best bid and ask information to transactions table
Data$transactions$BestBid[Data$transactions$BestBid>100000|Data$transactions$BestBid<1]=NA # Identifies missing best bid and ask instances
Data$transactions$BestAsk[Data$transactions$BestAsk>100000|Data$transactions$BestAsk<1]=NA # Identifies missing best bid and ask instances

### Times of period starts and announcements

Announcements<-merge(Data$PEADsignals[,c("R.PeriodID","Period","PEADSignal[1]","PEADSignal[2]","PEADValue[1]","PEADValue[2]")],
                     AnnouncementData[,c("R.PeriodID","R.Session","R.Time1","R.Time2","R.Time3","R.Time4")],by="R.PeriodID") # Adds signal data to announcements

Announcements[(length(Announcements[,1])+1):(length(Announcements[,1])*2),]<-Announcements # Doubles matrix as to have separate rows for markets 1 and 2
Announcements[,"Market"]<-c(rep(1,length(Announcements[,1])/2),rep(2,length(Announcements[,1])/2)) # Fills in market number
Announcements[,"R.Signal"]<-ifelse(Announcements$Market==1,Announcements$"PEADSignal[1]",Announcements$"PEADSignal[2]") # Writes signal column
Announcements[,"R.Phase"]<-rep(0:4,length(Announcements[,1])/5) # Generates phase ID

### Announcement type phase (-1,0,1) is not correct (uses Type of next phase, not of this phase). Commented out code below is the original:
#Announcements[,"R.Type"]<-ifelse(Announcements$R.Phase==0,0,ifelse(Announcements$R.Signal>0,1,-1)) # Adds phase type

### Announcement type phase now corrected:
Announcements[,"R.Type"]<-0
for (i in 1:nrow(Announcements)) {
  if (Announcements$R.Phase[i]!=0) {
    if (Announcements$R.Signal[i-1]>0) {
      Announcements$R.Type[i]<- 1
    } else {Announcements$R.Type[i]<- -1}
  }
}

Announcements[,"R.PhaseStart"]<-ifelse(Announcements$R.Phase==0,0,
                                       ifelse(Announcements$R.Phase==1,Announcements$R.Time1,
                                              ifelse(Announcements$R.Phase==2,Announcements$R.Time2,
                                                     ifelse(Announcements$R.Phase==3,Announcements$R.Time3,Announcements$R.Time4)))) # Writes starting time of this phase

Announcements<-Announcements[,!(names(Announcements) %in% c("PEADSignal[1]","PEADSignal[2]","PEADValue[1]","PEADValue[2]","R.Time1","R.Time2","R.Time3","R.Time4","Event","Time"))] # Removes superfluous columns

Data$transactions<-merge(Data$transactions,AnnouncementData[,c("R.PeriodID","R.Time1","R.Time2","R.Time3","R.Time4")],by=c("R.PeriodID")) # Adds announcement data to transactions table

Data$transactions[,"R.Phase"]<-ifelse(Data$transactions$R.TradeTime<=Data$transactions$R.Time1,0,ifelse(Data$transactions$R.TradeTime<=Data$transactions$R.Time2,1,ifelse(Data$transactions$R.TradeTime<=Data$transactions$R.Time3,2,ifelse(Data$transactions$R.TradeTime<=Data$transactions$R.Time4,3,4)))) # Writes R.Phase into Data$transactions

Data$transactions<-merge(Data$transactions,Announcements[,(names(Announcements) %in% c("R.PeriodID","Market","R.Phase","R.Type","R.PhaseStart"))],by=c("R.PeriodID","Market","R.Phase")) # Adds announcement data to transactions table

Data$transactions[,"R.WithinPhaseTime"]<-ifelse(Data$transactions$R.Phase==0,Data$transactions$R.TradeTime,Data$transactions$R.TradeTime-Data$transactions$R.PhaseStart) # Adds within phase time variable


### Plot PEAD

Data$transactions<-Data$transactions[order(Data$transactions$R.PeriodID, Data$transactions$Market, Data$transactions$AcceptanceID),] # Sorts data

XLIM<-c(0,200)
YLIM<-c(-max(Data$transactions$Price),max(Data$transactions$Price))
#LWD<-2

### Prepares average price list
TempDesiredColumns<-3
AvgPrices<-list(Base=list(),Correl=list())
AvgPrices$Base<-list(Down=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==-1&Data$transactions$TreatmentPEAD==1])),TempDesiredColumns),ncol=TempDesiredColumns),
                     Start=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==0&Data$transactions$TreatmentPEAD==1])),TempDesiredColumns),ncol=TempDesiredColumns),
                     Up=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==1&Data$transactions$TreatmentPEAD==1])),TempDesiredColumns),ncol=TempDesiredColumns)) # Fills in unique trade times per type. Second column is only placeholder for price data.
AvgPrices$Correl<-list(Down=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==-1&Data$transactions$TreatmentPEAD==2])),TempDesiredColumns),ncol=TempDesiredColumns),
                       Start=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==0&Data$transactions$TreatmentPEAD==2])),TempDesiredColumns),ncol=TempDesiredColumns),
                       Up=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==1&Data$transactions$TreatmentPEAD==2])),TempDesiredColumns),ncol=TempDesiredColumns)) # Fills in unique trade times per type. Second column is only placeholder for price data.

# Writes column R.NormPrice which contains NA in R.Phase 0 and the last price in the same unique trading session/market/period's previous R.Phase
Data$transactions[,"R.NormPrice"]<-NA
for (Transaction in 1:length(Data$transactions[,1])){
  if (Data$transactions$R.Phase[Transaction]>0){
    MaxTimePhase0<-max(Data$transactions[Data$transactions$Market==Data$transactions$Market[Transaction]&
                                           Data$transactions$R.Session==Data$transactions$R.Session[Transaction]&
                                           Data$transactions$Period==Data$transactions$Period[Transaction]&
                                           Data$transactions$R.Phase==Data$transactions$R.Phase[Transaction]-1,][,"R.WithinPhaseTime"])
    Data$transactions$R.NormPrice[Transaction]<-Data$transactions[Data$transactions$Market==Data$transactions$Market[Transaction]&
                                                                    Data$transactions$R.Session==Data$transactions$R.Session[Transaction]&
                                                                    Data$transactions$Period==Data$transactions$Period[Transaction]&
                                                                    Data$transactions$R.Phase==Data$transactions$R.Phase[Transaction]-1&
                                                                    Data$transactions$R.WithinPhaseTime==MaxTimePhase0,][1,"Price"]
  }
  if(Transaction/500==ceiling(Transaction/500)){
    print(paste(Transaction," of ",length(Data$transactions[,1]),sep=""))
  }
}

#R02
save.image("d:/institut/#currentwork/pead/results/PEAD_R02.RData")
save.image("C:/Users/Josef Fink/Documents/Privat/PhD/Paper/Results/00_R/PEAD_R02.RData")
#load("d:/institut/#currentwork/pead/results/PEAD_R02.RData")


# For every type, loops through the unique trade times and, for each of those, calculates the average of all unique phases' latest trades
Temp.TotalTimes<-
  nrow(AvgPrices$Base$Down)+
  nrow(AvgPrices$Base$Start)+
  nrow(AvgPrices$Base$Up)+
  nrow(AvgPrices$Correl$Down)+
  nrow(AvgPrices$Correl$Start)+
  nrow(AvgPrices$Correl$Up) #Calculates total number of steps to be taken

AvgPricesMkt<-as.data.frame(AvgPrices$Base$Down)
AvgPricesMkt$Type<-(-1)
AvgPricesMkt<-rbind(AvgPricesMkt,cbind(AvgPrices$Base$Start,Type=rep(0,nrow(AvgPrices$Base$Start))))
AvgPricesMkt<-rbind(AvgPricesMkt,cbind(AvgPrices$Base$Up,Type=rep(1,nrow(AvgPrices$Base$Up))))
AvgPricesMkt$Treat<-1

AvgPricesMkt<-rbind(AvgPricesMkt,cbind(AvgPrices$Correl$Down,Type=rep(-1,nrow(AvgPrices$Correl$Down)),Treat=rep(2,nrow(AvgPrices$Correl$Down))))
AvgPricesMkt<-rbind(AvgPricesMkt,cbind(AvgPrices$Correl$Start,Type=rep(0,nrow(AvgPrices$Correl$Start)),Treat=rep(2,nrow(AvgPrices$Correl$Start))))
AvgPricesMkt<-rbind(AvgPricesMkt,cbind(AvgPrices$Correl$Up,Type=rep(1,nrow(AvgPrices$Correl$Up)),Treat=rep(2,nrow(AvgPrices$Correl$Up))))

#AvgPricesMkt$Treat[is.na(AvgPricesMkt$Treat)]<-2

Temp.StartClockTime<-Sys.time() #Saves starting system time
for (Treat in c("Base","Correl")){
  Treat.num<-ifelse(Treat=="Base",1,2)
  for (Type in -1:1) {
    for (Time in 1:length(AvgPrices[[Treat]][[Type+2]][,1])){
      Temp.Time<-
        Time+
        ifelse(Type==0,nrow(AvgPrices[[Treat]]$Down),0)+
        ifelse(Type==1,nrow(AvgPrices[[Treat]]$Down)+nrow(AvgPrices[[Treat]]$Start),0)+
        ifelse(Treat=="Correl",nrow(AvgPrices$Base$Down)+nrow(AvgPrices$Base$Start)+nrow(AvgPrices$Base$Up),0) #Calculates percentage of all steps performed
      Time.Elapsed<-round(difftime(Sys.time(),Temp.StartClockTime,units="mins"),1) #Calculates time elapsed
      print(paste(round((Temp.Time/Temp.TotalTimes*100),2),"% --- Time elapsed (mins): ",Time.Elapsed,", Time remaining (mins): ",round(Time.Elapsed/(Temp.Time/Temp.TotalTimes),1),sep="")) #Prints progress report
      TempSum<-0
      TempN<-0
      for (Session in 1:NumSessions){
        for (Period in 1:4){
          for (Market in 1:2){
            for (Phase in if(Type==0){0} else {1:4}){
              if (nrow(Data$transactions[Data$transactions$TreatmentPEAD==Treat.num&
                                         Data$transactions$R.Type==Type&
                                         Data$transactions$Market==Market&
                                         Data$transactions$R.Session==Session&
                                         Data$transactions$Period==Period&
                                         Data$transactions$R.Phase==Phase&
                                         Data$transactions$R.WithinPhaseTime<=AvgPrices[[Treat]][[Type+2]][Time],])>0){ # Run if there was a trade fulfilling the criteria
                TempN<-TempN+1
                TempSum<-TempSum+tail(Data$transactions[Data$transactions$TreatmentPEAD==Treat.num&
                                                          Data$transactions$R.Type==Type&
                                                          Data$transactions$Market==Market&
                                                          Data$transactions$R.Session==Session&
                                                          Data$transactions$Period==Period&
                                                          Data$transactions$R.Phase==Phase&
                                                          Data$transactions$R.WithinPhaseTime<=AvgPrices[[Treat]][[Type+2]][Time],],n=1)[,"Price"]-
                  ifelse(Phase==0,0,Data$transactions$R.NormPrice[Data$transactions$TreatmentPEAD==Treat.num&
                                                                    Data$transactions$Market==Market&
                                                                    Data$transactions$R.Session==Session&
                                                                    Data$transactions$Period==Period&
                                                                    Data$transactions$R.Phase==Phase][1])
              }
            }
          }
        }
      }
      AvgPrices[[Treat]][[Type+2]][Time,2]<-TempSum/TempN
      AvgPrices[[Treat]][[Type+2]][Time,3]<-TempN
    }
    colnames(AvgPrices[[Treat]][[Type+2]])<-c("R.WithinPhaseTime","R.AvgPrice","R.N") # Writes column names
  }
}


#R01
save.image("d:/institut/#currentwork/pead/results/PEAD_R01.RData")
save.image(paste("d:/institut/#currentwork/pead/results/PEAD_R01.RData_",substr(Sys.time(),1,10),"_",substr(Sys.time(),12,13),substr(Sys.time(),15,16),sep=""))
#load("d:/institut/#currentwork/pead/results/PEAD_R01.RData")

### Plots graphs


XLIM<-c(0,180)
YLIM<-c(-max(Data$transactions$Price),max(Data$transactions$Price))
YLIM<-c(-10,10)
LWD<-2

for (Treat in 1:2){
  for (Type in -1:1) {
    Temp1<-0 # Sets temporary variable counting lines already drawn
    Temp2<-0 # Sets temporary variable counting PowerData lines already added
    if(T){dev.new("PricePlot")} else {jpeg(paste("PEADPlot_T",Type,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
    for(Market in 1:2){
      for(Session in 1:NumSessions){
        for(Period in 1:4) {
          for (Phase in if(Type==0){0} else {1:4}){
            #print(paste("Session",Session,"Period",Period,"Phase",Phase,"Type",Type,"Market",Market))
            # Plot if phase exists and saw trade
            if(nrow(Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,])>0){
              if(Temp1==0){
                Temp1<-Temp1+1 # Increases count of lines already drawn
                PriceNormalization<-
                  if(Phase==0){Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][1,"Price"]}
                else {Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,][length(Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,1]),"Price"]} # Sets base price equal to first price in time series in phase 0, and equal to last price in time series in phase > 0
                plot(x=Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"R.WithinPhaseTime"],y=Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization, type="l", col="gray", lwd=LWD, xlim=XLIM, ylim=YLIM, xlab="Time (seconds)", ylab="Price (Taler)", main=paste("Treatment ",c("Base","Correlated")[Treat],", Type ",Type,sep="")) # Plots market 1
                
              } else {
                Temp1<-Temp1+1 # Increases count of lines already drawn
                PriceNormalization<-
                  if(Phase==0){Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][1,"Price"]}
                else {Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,][length(Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,1]),"Price"]} # Sets base price equal to first price in time series in phase 0, and equal to last price in time series in phase > 0
                lines(x=Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"R.WithinPhaseTime"],y=Data$transactions[Data$transactions$TreatmentPEAD==Treat&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization, type="l", col="gray", lwd=LWD) # Plots market 2+
              }
            }
          }
        }
      }
    }
    
    # Plots means
    lines(x=AvgPrices[[c("Base","Correl")[Treat]]][[Type+2]][,1],y=AvgPrices[[c("Base","Correl")[Treat]]][[Type+2]][,2]-AvgPrices[[c("Base","Correl")[Treat]]][[Type+2]][1,2], type="l", col="black", lwd=LWD+2) # Plots mean prices
    plot(x=AvgPrices[[c("Base","Correl")[Treat]]][[Type+2]][,1],y=AvgPrices[[c("Base","Correl")[Treat]]][[Type+2]][,2]-AvgPrices[[c("Base","Correl")[Treat]]][[Type+2]][1,2], type="l", col="black", lwd=LWD+2,xlab="Time (seconds)", ylab="Price (Taler)", main=paste("Treatment ",c("Base","Correlated")[Treat],", Type ",Type,sep="")) # Plots mean prices
    
    #if(Params$ShowPlots){
    dev.copy(jpeg,paste("PEADPlot_",c("Base","Correl")[Treat],"_T",Type,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
    #}
    dev.off() # Turns off graphics device if even Distribution number
  }
}


################################### R03 ##########################################

### Long-Short plot compared to last period's FV (incl. surprise)


### Create Returns table
Returns<-Data$globals[,c("R.PeriodID", "TreatmentPEAD", "R.Session", "Period")]
Returns<-merge(Returns, Announcements[,c("R.PeriodID", "R.Session", "Period", "R.Type", "R.Phase","R.PhaseStart", "Market")], by = c("R.PeriodID", "R.Session", "Period"))
Returns<-merge(Returns, Data$PEADsignals[,c("R.PeriodID", "Set", "FV.A", "FV.B")], by.x = c("R.PeriodID", "R.Phase"), by.y = c("R.PeriodID", "Set"), all.x = TRUE)
Returns<-Returns[order(Returns$Market, Returns$R.PeriodID, Returns$R.Phase),]
Returns[is.na(Returns)]<-100
Returns$Surprise<-0
Returns$FV<-0
Returns$FV[Returns$Market==1]<-Returns$FV.A[Returns$Market==1]
Returns$FV[Returns$Market==2]<-Returns$FV.B[Returns$Market==2]
Returns<-Returns[,-which(names(Returns)==c("FV.A", "FV.B"))]
for (row in 1:nrow(Returns)) {
  Returns$FV.Previous[row]<-ifelse(Returns$R.Phase[row]!=0, Returns$FV[row-1], NA)
}


# Add variable to indicate if announcement was a surprise, i.e., a large FV change (Surprise==1 always after 1st announcement and always when signal direction changes, otherwise Surprise==0)
for (i in 1:nrow(Returns)) {
  if (Returns$R.Phase[i]!=0) {
    if (Returns$R.Type[i]!=Returns$R.Type[i-1]) {
      Returns$Surprise[i]<-1
    }
  }
}


### Add return windows to Returns (to calculate announcement window returns and PEAD over different time frames). For each 180 sec. phase there are 18 windows of 10 sec. each. For each announcement there is an 180 sec. window in the past and the future
Returns<-Returns[rep(row.names(Returns), 36),]
Returns<-Returns[order(Returns$Market, Returns$R.Session, Returns$Period, Returns$R.Phase),]
Returns$Window<-0
Returns$WindowStart<-0
Returns$WindowEnd<-0
Returns$Phase.ID<-0

# Add Phase.ID to identify all unique PEAD phases
Temp1<-0
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      for (Phase in 0:4) {
        Temp1<-Temp1+1
        Returns$Phase.ID[Returns$Market==Market&
                           Returns$R.Session==Session&
                           Returns$Period==Period&
                           Returns$R.Phase==Phase] <- Temp1
      }
    }
  }
}

# Add Window number to Returns table
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      for (Phase in 0:4) {
        for (Window in 1:36) {
          Returns$Window[Returns$Market==Market&
                           Returns$R.Session==Session&
                           Returns$Period==Period&
                           Returns$R.Phase==Phase][Window] <- Window-19 #Add Window Number
        }
      }
    }
  }
}

# Add start time of window
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      for (Phase in 0:4) {
        for (Window in -18:17) {
          
          if (Window==-18 & Phase==0) {
            Returns$WindowStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window] <- Window*10
          }
          else if (Window==-18 & Phase!=0) {
            Returns$WindowStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window] <- 
              Returns$R.PhaseStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase-1]
          } 
          else if (Window<0) {
            Returns$WindowStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window] <- 
              Returns$WindowStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==-18] + (Window+18)*10
          } 
          else {
            Returns$WindowStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window] <- 
              Returns$R.PhaseStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase] + Window*10
          }
        }
      }
    }
  }
}

# Add end time of window
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      for (Phase in 0:4) {
        for (Window in -18:17) {
          
          if (Phase==4 & Window==17) {
            Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window] <- 
              max(max(Data$transactions$R.TradeTime[Data$transactions$Market==Market & Data$transactions$R.Session==Session & Data$transactions$Period==Period]), 900)
          } else if (Window==17) {
            Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window] <- 
              Returns$R.PhaseStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase+1][1]
          } else {
            Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window] <-
              Returns$WindowStart[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window+1]
          }
        }
      }
    }
  }
}


# Add average, last and first transaction price and times within trade windows
Returns$NumTransactions<-NA
Returns$AvgPrice<-NA
Returns$LastPrice<-NA
Returns$FirstPrice<-NA
Returns$TimeAvgTrade<-NA
Returns$TimeLastTrade<-NA
Returns$TimeFirstTrade<-NA
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      for (Phase in 0:4) {
        for (Window in -18:17) {
          TempTransactions<-Data$transactions[Data$transactions$Market==Market&
                                                Data$transactions$R.Session==Session&
                                                Data$transactions$Period==Period&
                                                Data$transactions$R.TradeTime>Returns$WindowStart[Returns$Market==Market&
                                                                                                    Returns$R.Session==Session&
                                                                                                    Returns$Period==Period&
                                                                                                    Returns$R.Phase==Phase&
                                                                                                    Returns$Window==Window]&
                                                Data$transactions$R.TradeTime<=Returns$WindowEnd[Returns$Market==Market&
                                                                                                   Returns$R.Session==Session&
                                                                                                   Returns$Period==Period&
                                                                                                   Returns$R.Phase==Phase&
                                                                                                   Returns$Window==Window],]
          NumTransactions<-nrow(TempTransactions)
          
          if (NumTransactions>0) {
            LastPrice<-tail(TempTransactions$Price, 1)
            AvgPrice<-mean(TempTransactions$Price)
            FirstPrice<-head(TempTransactions$Price, 1)
            TimeLastTrade<-tail(TempTransactions$R.TradeTime, 1)
            TimeAvgTrade<-mean(TempTransactions$R.TradeTime)
            TimeFirstTrade<-head(TempTransactions$R.TradeTime, 1)
          }
          else if (NumTransactions==0 &  Window<0) {
            LastPrice<-ifelse(Phase==0, NA, Returns$LastPrice[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase-1 & Returns$Window==Window+18])
            AvgPrice<-ifelse(Phase==0, NA, Returns$AvgPrice[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase-1 & Returns$Window==Window+18])
            FirstPrice<-ifelse(Phase==0, NA, Returns$FirstPrice[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase-1 & Returns$Window==Window+18])
            TimeLastTrade<-ifelse(Phase==0, NA, Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase-1 & Returns$Window==Window+18])
            TimeAvgTrade<-ifelse(Phase==0, NA, Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase-1 & Returns$Window==Window+18])
            TimeFirstTrade<-ifelse(Phase==0, NA, Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase-1 & Returns$Window==Window+18])
          }
          else if (NumTransactions==0) {
            LastPrice<-Returns$LastPrice[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window-1]
            AvgPrice<-Returns$AvgPrice[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window-1]
            FirstPrice<-Returns$FirstPrice[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window-1]
            TimeLastTrade<-Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window-1]
            TimeAvgTrade<-Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window-1]
            TimeFirstTrade<-Returns$WindowEnd[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase & Returns$Window==Window-1]
          }
          
          print(paste("Market:", Market,"S", Session, "P", Period, "Phase", Phase, "Window", Window, ": ", "NumTrans", NumTransactions, "LastPrice:", LastPrice, "AvgPr:", AvgPrice, "TimeLastTrade:", TimeLastTrade, "TimeAvgTrade:", TimeAvgTrade, "NumTransactions:", NumTransactions))
          
          Returns$NumTransactions[Returns$Market==Market&
                                    Returns$R.Session==Session&
                                    Returns$Period==Period&
                                    Returns$R.Phase==Phase&
                                    Returns$Window==Window]<- NumTransactions
          Returns$LastPrice[Returns$Market==Market&
                              Returns$R.Session==Session&
                              Returns$Period==Period&
                              Returns$R.Phase==Phase&
                              Returns$Window==Window]<- LastPrice
          Returns$AvgPrice[Returns$Market==Market&
                             Returns$R.Session==Session&
                             Returns$Period==Period&
                             Returns$R.Phase==Phase&
                             Returns$Window==Window]<- AvgPrice
          Returns$FirstPrice[Returns$Market==Market&
                               Returns$R.Session==Session&
                               Returns$Period==Period&
                               Returns$R.Phase==Phase&
                               Returns$Window==Window]<- FirstPrice
          Returns$TimeLastTrade[Returns$Market==Market&
                                  Returns$R.Session==Session&
                                  Returns$Period==Period&
                                  Returns$R.Phase==Phase&
                                  Returns$Window==Window]<- TimeLastTrade
          Returns$TimeAvgTrade[Returns$Market==Market&
                                 Returns$R.Session==Session&
                                 Returns$Period==Period&
                                 Returns$R.Phase==Phase&
                                 Returns$Window==Window]<- TimeAvgTrade
          Returns$TimeFirstTrade[Returns$Market==Market&
                                   Returns$R.Session==Session&
                                   Returns$Period==Period&
                                   Returns$R.Phase==Phase&
                                   Returns$Window==Window]<- TimeFirstTrade
        }
      }
    }
  }
}


# Get OB data weighted into Results table (Midpoints, spreads, depth, best bid/ask) (3h)
(Temp.StartClockTime<-Sys.time()) #Saves starting system time
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      NumActions<-length(names(OB$Books[[Session]][[Period]][[Market]]))
      StartProgram<-Data$globals[Data$globals$R.Session==Session&Data$globals$Period==Period, "StartTime"]
      StartCDA<-Data$globals[Data$globals$R.Session==Session&Data$globals$Period==Period, "StartTimeCDA"]
      for (Phase in 0:4) {
        for (Window in -18:17) { # For each Window we record all midpoints and the duration they applied within the Window
          
          StartWindow <- Returns$WindowStart[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]
          EndWindow <- Returns$WindowEnd[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]
          Temp<-data.frame(TimeInWindow=numeric(), 
                           Midpoint=numeric(), Midpoint.1=numeric(), 
                           Spread.pct=numeric(), Spread.ECU=numeric(), 
                           Ask.depth=numeric(), Bid.depth=numeric(), 
                           Ask.best=numeric(), Bid.best=numeric()) # Data frame to temporarily hold all OB information and times they applied in this window
          counter<-0
          
          for (i in 1:NumActions) { 
            
            # When did the previous action, this action, and the next action in the order book happen
            ActionTimePrevious<-ifelse(i==1, 0,round(as.numeric(levels(OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1,1])),3) + StartProgram - StartCDA)
            ActionTime<-round(as.numeric(levels(OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1,1])),3) + StartProgram - StartCDA
            ActionTimeNext<-ifelse(i==NumActions, EndWindow,round(as.numeric(levels(OB$Books[[Session]][[Period]][[Market]][[i+1]][["Summary"]][1,1])),3) + StartProgram - StartCDA)
            
            if (ActionTimePrevious<StartWindow & ActionTime>StartWindow & i!=1) { # if previous action falls into last window and reaches into this window: Time is measured from the start of this window until the next action or until the end of the window (does not work if it is the first action)
              Temp <- rbind(Temp,rep(-77777,length(Temp)))
              counter<-counter+1
              Temp$TimeInWindow[counter]<- min(ActionTime, EndWindow)-StartWindow
              Temp$Midpoint[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Midpoint"]
              Temp$Midpoint.1[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Midpoint.1"]
              Temp$Spread.pct[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Spread.pct"]
              Temp$Spread.ECU[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Spread.ECU"]
              Temp$Ask.depth[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Ask.depth"]
              Temp$Bid.depth[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Bid.depth"]
              Temp$Ask.best[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Ask.best"]
              Temp$Bid.best[counter] <- OB$Books[[Session]][[Period]][[Market]][[i-1]][["Summary"]][1, "Bid.best"]
            } 
            if (ActionTime>StartWindow & ActionTime<=EndWindow) { # if action falls into this window: Time is measured either until the next action or until the end of the window
              Temp <- rbind(Temp,rep(-77777,length(Temp)))
              counter<-counter+1
              Temp$TimeInWindow[counter]<- min(ActionTimeNext, EndWindow)-ActionTime
              Temp$Midpoint[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Midpoint"]
              Temp$Midpoint.1[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Midpoint.1"]
              Temp$Spread.pct[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Spread.pct"]
              Temp$Spread.ECU[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Spread.ECU"]
              Temp$Ask.depth[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Ask.depth"]
              Temp$Bid.depth[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Bid.depth"]
              Temp$Ask.best[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Ask.best"]
              Temp$Bid.best[counter] <- OB$Books[[Session]][[Period]][[Market]][[i]][["Summary"]][1, "Bid.best"]
            }
            print(paste("Market: ", Market, ", Session: ", Session, ", Period: ", Period, ", Phase: ", Phase, ", Window: ", Window, ", Action: ", i, " of ", NumActions, sep = ""))
          }
          
          # If the last action in the Period occurs before this window started, the window gets the midpoint resulting from the last action
          if ((round(as.numeric(levels(OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1,1])),3) + StartProgram - StartCDA)<=StartWindow) {
            Returns$Midpoint[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Midpoint"]
            Returns$Midpoint.1[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Midpoint.1"]
            Returns$Spread.pct[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Spread.pct"]
            Returns$Spread.ECU[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Spread.ECU"]
            Returns$Ask.depth[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Ask.depth"]
            Returns$Bid.depth[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Bid.depth"]
            Returns$Ask.best[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Ask.best"]
            Returns$Bid.best[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-OB$Books[[Session]][[Period]][[Market]][[NumActions]][["Summary"]][1, "Bid.best"]
          } else { # In all other cases the time-weighted Midpoint/Midpoint.1-value from the "Temp" data frame is calculated for this particular window
            Returns$Midpoint[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Midpoint, Temp$TimeInWindow, na.rm = TRUE)
            Returns$Midpoint.1[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Midpoint.1, Temp$TimeInWindow, na.rm = TRUE)
            Returns$Spread.pct[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Spread.pct, Temp$TimeInWindow, na.rm = TRUE)
            Returns$Spread.ECU[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Spread.ECU, Temp$TimeInWindow, na.rm = TRUE)
            Returns$Ask.depth[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Ask.depth, Temp$TimeInWindow, na.rm = TRUE)
            Returns$Bid.depth[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Bid.depth, Temp$TimeInWindow, na.rm = TRUE)
            Returns$Ask.best[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Ask.best, Temp$TimeInWindow, na.rm = TRUE)
            Returns$Bid.best[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]<-weighted.mean(Temp$Bid.best, Temp$TimeInWindow, na.rm = TRUE)
          }
        }
      }
    }
  }
}
Temp.StartClockTime
Sys.time()

# Add Midpoint timing
Returns$TimeMidpoint<-(Returns$WindowEnd+Returns$WindowStart)/2

# Add R.Phase to Data$timelog table in order to correctly address the Windows
Data$timelog<-merge(Data$timelog,AnnouncementData[,c("R.PeriodID", "R.Time1", "R.Time2", "R.Time3", "R.Time4")], by="R.PeriodID")
Data$timelog<-merge(Data$timelog,Data$globals[,c("R.PeriodID", "StartTime", "StartTimeCDA")], by="R.PeriodID")
Data$timelog$R.Phase<-ifelse(Data$timelog$Time+Data$timelog$StartTime-Data$timelog$StartTimeCDA<=Data$timelog$R.Time1, 0,
                             ifelse(Data$timelog$Time+Data$timelog$StartTime-Data$timelog$StartTimeCDA<=Data$timelog$R.Time2, 1, 
                                    ifelse(Data$timelog$Time+Data$timelog$StartTime-Data$timelog$StartTimeCDA<=Data$timelog$R.Time3, 2,
                                           ifelse(Data$timelog$Time+Data$timelog$StartTime-Data$timelog$StartTimeCDA<=Data$timelog$R.Time4, 3, 4))))
Data$timelog<-Data$timelog[,!(names(Data$timelog) %in% c("R.Time1","R.Time2","R.Time3","R.Time4", "StartTime", "StartTimeCDA"))] # Removes superfluous columns


# Add variables to each window summing up the total offers created, accepted, cancelled within the window
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      StartProgram<-Data$globals[Data$globals$R.Session==Session&Data$globals$Period==Period, "StartTime"]
      StartCDA<-Data$globals[Data$globals$R.Session==Session&Data$globals$Period==Period, "StartTimeCDA"]
      for (Phase in 0:4) {
        for (Window in -18:17) {
          
          StartWindow <- Returns$WindowStart[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]
          EndWindow <- Returns$WindowEnd[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window]
          
          Returns$BuyOffersCreated[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window] <- 
            length(Data$timelog$R.PeriodID[Data$timelog$R.Session==Session&
                                             Data$timelog$Period==Period&
                                             Data$timelog$`Data[2]`==Market&
                                             Data$timelog$Event==8& # Creates buy offer
                                             Data$timelog$Time+StartProgram-StartCDA>StartWindow&
                                             Data$timelog$Time+StartProgram-StartCDA<=EndWindow])
          Returns$SellOffersCreated[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window] <- 
            length(Data$timelog$R.PeriodID[Data$timelog$R.Session==Session&
                                             Data$timelog$Period==Period&
                                             Data$timelog$`Data[2]`==Market&
                                             Data$timelog$Event==7& # Creates sell offer
                                             Data$timelog$Time+StartProgram-StartCDA>StartWindow&
                                             Data$timelog$Time+StartProgram-StartCDA<=EndWindow])
          Returns$BuyOffersCancelled[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window] <- 
            length(Data$timelog$R.PeriodID[Data$timelog$R.Session==Session&
                                             Data$timelog$Period==Period&
                                             #Data$timelog$R.Phase==Phase&
                                             Data$timelog$`Data[2]`==Market&
                                             Data$timelog$Event==6& # Cancels buy offer
                                             Data$timelog$Time+StartProgram-StartCDA>StartWindow&
                                             Data$timelog$Time+StartProgram-StartCDA<=EndWindow])
          Returns$SellOffersCancelled[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window] <- 
            length(Data$timelog$R.PeriodID[Data$timelog$R.Session==Session&
                                             Data$timelog$Period==Period&
                                             Data$timelog$`Data[2]`==Market&
                                             Data$timelog$Event==5& # Cancels buy offer
                                             Data$timelog$Time+StartProgram-StartCDA>StartWindow&
                                             Data$timelog$Time+StartProgram-StartCDA<=EndWindow])
          Returns$NumBuys[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window] <- 
            length(Data$timelog$R.PeriodID[Data$timelog$R.Session==Session&
                                             Data$timelog$Period==Period&
                                             Data$timelog$`Data[2]`==Market&
                                             Data$timelog$Event==4& # Buys (or accepts a sell offer)
                                             Data$timelog$Time+StartProgram-StartCDA>StartWindow&
                                             Data$timelog$Time+StartProgram-StartCDA<=EndWindow])
          Returns$NumSells[Returns$R.Session==Session&Returns$Period==Period&Returns$Market==Market&Returns$R.Phase==Phase&Returns$Window==Window] <- 
            length(Data$timelog$R.PeriodID[Data$timelog$R.Session==Session&
                                             Data$timelog$Period==Period&
                                             Data$timelog$`Data[2]`==Market&
                                             Data$timelog$Event==3& # Sells (or accepts a buy offer)
                                             Data$timelog$Time+StartProgram-StartCDA>StartWindow&
                                             Data$timelog$Time+StartProgram-StartCDA<=EndWindow])
          
          print(paste("Market: ", Market, ", Session: ", Session, ", Period: ", Period, ", Phase: ", Phase, ", Window: ", Window, sep = ""))
        }
      }
    }
  }
}



# Create new column to indicate the unique PEAD window (First make unique PhaseID and then offset it backwards by one Window so that PEAD window will be [-10:170] seconds from announcement)
Returns$PhaseID<-0
TempPhase<-0
for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      for (Phase in 0:4) {
        TempPhase<-TempPhase+1
        Returns$PhaseID[Returns$Market==Market & Returns$R.Session==Session & Returns$Period==Period & Returns$R.Phase==Phase]<-TempPhase
      }
    }
  }
}
Returns$PhaseID<-c(Returns$PhaseID[-seq(1)], TempPhase+1)


# Create new columns relative to announcement: Price metrics compared to previous phase's FV and Timings compared to announcement
Returns$RelativeLastPrice<-100*Returns$LastPrice/Returns$FV.Previous
Returns$RelativeAvgPrice<-100*Returns$AvgPrice/Returns$FV.Previous
Returns$RelativeFirstPrice<-100*Returns$FirstPrice/Returns$FV.Previous
Returns$RelativeMidpoint<-100*Returns$Midpoint/Returns$FV.Previous
Returns$RelativeMidpoint.1<-100*Returns$Midpoint.1/Returns$FV.Previous
Returns$TimeLastTradePhase<- Returns$TimeLastTrade-Returns$R.PhaseStart
Returns$TimeAvgTradePhase<- Returns$TimeAvgTrade-Returns$R.PhaseStart
Returns$TimeFirstTradePhase<- Returns$TimeFirstTrade-Returns$R.PhaseStart
Returns$TimeMidpointPhase<- (Returns$WindowStart+Returns$WindowEnd)/2-Returns$R.PhaseStart


# Create average returns over windows (grouped by Treatment, Surprise, R.Type, PhaseID.Window)
SummaryReturns<-aggregate(cbind(TimeAvgTrade=Returns$TimeAvgTradePhase, 
                                TimeLastTrade=Returns$TimeLastTradePhase,
                                TimeFirstTrade=Returns$TimeFirstTradePhase,
                                TimeMidpoint=Returns$TimeMidpointPhase,
                                RelativeAvgPrice=Returns$RelativeAvgPrice,
                                RelativeLastPrice=Returns$RelativeLastPrice,
                                RelativeFirstPrice=Returns$RelativeFirstPrice,
                                RelativeMidpoint=Returns$RelativeMidpoint,
                                RelativeMidpoint.1=Returns$RelativeMidpoint.1)~
                            Returns$R.Type+
                            Returns$TreatmentPEAD+
                            Returns$Surprise+
                            Returns$Window,
                          Returns, 
                          mean)

OfferSummary<-aggregate(cbind(NumTransactions=Returns$NumTransactions,
                              BuyOffersCreated=Returns$BuyOffersCreated,
                              SellOffersCreated=Returns$SellOffersCreated,
                              BuyOffersCancelled=Returns$BuyOffersCancelled,
                              SellOffersCancelled=Returns$SellOffersCancelled,
                              NumBuys=Returns$NumBuys,
                              NumSells=Returns$NumSells)~
                          Returns$R.Type+
                          Returns$TreatmentPEAD+
                          Returns$Surprise+
                          Returns$Window,
                        Returns, 
                        sum)

SummaryReturns<-merge(SummaryReturns, OfferSummary, by=c("Returns$R.Type", "Returns$TreatmentPEAD", "Returns$Surprise", "Returns$Window"))

names(SummaryReturns)<-c("R.Type", "TreatmentPEAD", "Surprise", "Window", 
                         "TimeAvgTradePhase", "TimeLastTradePhase", "TimeFirstTradePhase", "TimeMidpointPhase", 
                         "RelativeAvgPrice", "RelativeLastPrice", "RelativeFirstPrice", "RelativeMidpoint", "RelativeMidpoint.1", 
                         "NumTransactions",
                         "BuyOffersCreated", "SellOffersCreated", "BuyOffersCancelled", "SellOffersCancelled", "NumBuys", "NumSells")

Returns<-Returns[order(Returns$Window),]
SummaryReturns<-SummaryReturns[order(SummaryReturns$Window),]

### Plot Returns (with Surprise/No Surprise)
LWD1<-1
LWD2<-1.5
XLIM<-c(-10, 180)
YLIM<-c(min(SummaryReturns$RelativeAvgPrice)-10, max(SummaryReturns$RelativeAvgPrice)+10)


### Which prices and timeframes to use (average price and time in window, first, or last)
Measures<-list(Average=c("TimeAvgTradePhase", "RelativeAvgPrice", "TimeAvgTradePhase", "RelativeAvgPrice", "Avg. price", "purple"),
               Last=c("TimeLastTradePhase", "RelativeLastPrice", "TimeLastTradePhase", "RelativeLastPrice", "Last price", "red"),
               First=c("TimeFirstTradePhase", "RelativeFirstPrice", "TimeFirstTradePhase", "RelativeFirstPrice", "First price", "blue"),
               Midpoint=c("TimeMidpointPhase", "RelativeMidpoint", "TimeMidpointPhase", "RelativeMidpoint", "Midpoint", "dark green"),
               Midpoint.1=c("TimeMidpointPhase", "RelativeMidpoint.1", "TimeMidpointPhase", "RelativeMidpoint.1", "Midpoint.1", "brown"))


timeandprice<-Measures[["Midpoint.1"]] # Choose "Average", "First" or "Last" price, "Midpoint" or "Midpoint.1"
for (Treat in 1:2){
  if(F){dev.new("PricePlot")} else {jpeg(paste("R03_PEADPlot_",timeandprice[5],"_T_",c("Base", "Correl")[Treat],"_",".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
  par(mfrow=c(2,2), mar=c(4,4,2,2))
  for (Type in c(1,-1)) {
    for (Surprise in 0:1) {
      Temp1<-0
      
      for (Phase in 1:length(unique(Returns$Phase.ID))) {
        
        if (Temp1==0) {
          Temp1<-Temp1+1
          plot(x=Returns[Returns$TreatmentPEAD==Treat&
                           Returns$R.Type==Type&
                           Returns$Surprise==Surprise&
                           Returns$Phase.ID==Phase,][18:36,timeandprice[1]],
               y=Returns[Returns$TreatmentPEAD==Treat&
                           Returns$R.Type==Type&
                           Returns$Surprise==Surprise&
                           Returns$Phase.ID==Phase,][18:36,timeandprice[2]], 
               type="l", col="grey90", lwd=LWD1, xlim=XLIM, ylim=YLIM, xlab="Time relative to announcement (seconds)", ylab=paste(timeandprice[5], "in window (relative to previous phase's FV)"), 
               cex.main=0.7, cex.axis=0.7, cex.lab=0.7, 
               main=paste("Treatment: ",c("Base","Correlated")[Treat],", Type: ",Type, ", Surprise: ",paste(c("No","Yes")[Surprise+1],sep=""))) # Plots market 1
          
        } else {
          Temp1<-Temp1+1
          lines(x=Returns[Returns$TreatmentPEAD==Treat&
                            Returns$R.Type==Type&
                            Returns$Surprise==Surprise&
                            Returns$Phase.ID==Phase,][18:36,timeandprice[1]],
                y=Returns[Returns$TreatmentPEAD==Treat&
                            Returns$R.Type==Type&
                            Returns$Surprise==Surprise&
                            Returns$Phase.ID==Phase,][18:36,timeandprice[2]], type="l", col="grey90", lwd=LWD1) # Plots market 2+
        }
        
        if (Phase==length(unique(Returns$Phase.ID))) {
          print("Plot done, Avg remaining")
          lines(x=SummaryReturns[SummaryReturns$TreatmentPEAD==Treat&
                                   SummaryReturns$R.Type==Type&
                                   SummaryReturns$Surprise==Surprise,][,timeandprice[3]],
                y=SummaryReturns[SummaryReturns$TreatmentPEAD==Treat&
                                   SummaryReturns$R.Type==Type&
                                   SummaryReturns$Surprise==Surprise,][,timeandprice[4]], type="o", col=timeandprice[6], lwd=LWD2)
          
          abline(v=0, type="l", col="black", lwd=LWD1) # Adds announcement time
          abline(v=c(-10, 10), type="l", lty=3, col="black", lwd=LWD1) # Adds announcement windows
          lines(x=XLIM, y=c(100,100), type="l", col="black", lwd=LWD1)
          lines(x=c(SummaryReturns[SummaryReturns$TreatmentPEAD==Treat&SummaryReturns$R.Type==Type&SummaryReturns$Surprise==Surprise,][19,timeandprice[3]],XLIM[2]), 
                y=rep(SummaryReturns[SummaryReturns$TreatmentPEAD==Treat&SummaryReturns$R.Type==Type&SummaryReturns$Surprise==Surprise,][19,timeandprice[4]],2),
                type="l", lty=3, col=timeandprice[6], lwd=LWD2)
        }
      }
    }
  }
  #dev.copy(jpeg,paste("PEADPlot_Treat.",c("Base","Correl")[Treat],".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
  dev.off()  
}

### Plot Returns (All Surprises)

# Aggregate SummaryReturns by Treatment and Type, not surprise
SummaryReturns.1<-aggregate(cbind(TimeAvgTrade=Returns$TimeAvgTradePhase, 
                                  TimeLastTrade=Returns$TimeLastTradePhase,
                                  TimeFirstTrade=Returns$TimeFirstTradePhase,
                                  TimeMidpoint=Returns$TimeMidpointPhase,
                                  RelativeAvgPrice=Returns$RelativeAvgPrice,
                                  RelativeLastPrice=Returns$RelativeLastPrice,
                                  RelativeFirstPrice=Returns$RelativeFirstPrice,
                                  RelativeMidpoint=Returns$RelativeMidpoint,
                                  RelativeMidpoint.1=Returns$RelativeMidpoint.1)~
                              Returns$R.Type+
                              Returns$TreatmentPEAD+
                              Returns$Window,
                            Returns, 
                            mean)
OfferSummary.1<-aggregate(cbind(NumTransactions=Returns$NumTransactions,
                                BuyOffersCreated=Returns$BuyOffersCreated,
                                SellOffersCreated=Returns$SellOffersCreated,
                                BuyOffersCancelled=Returns$BuyOffersCancelled,
                                SellOffersCancelled=Returns$SellOffersCancelled,
                                NumBuys=Returns$NumBuys,
                                NumSells=Returns$NumSells)~
                            Returns$R.Type+
                            Returns$TreatmentPEAD+
                            Returns$Window,
                          Returns, 
                          sum)
SummaryReturns.1<-merge(SummaryReturns.1, OfferSummary.1, by=c("Returns$R.Type", "Returns$TreatmentPEAD", "Returns$Window"))

names(SummaryReturns.1)<-c("R.Type", "TreatmentPEAD", "Window", 
                           "TimeAvgTradePhase", "TimeLastTradePhase", "TimeFirstTradePhase", "TimeMidpointPhase", 
                           "RelativeAvgPrice", "RelativeLastPrice", "RelativeFirstPrice", "RelativeMidpoint", "RelativeMidpoint.1", 
                           "NumTransactions",
                           "BuyOffersCreated", "SellOffersCreated", "BuyOffersCancelled", "SellOffersCancelled", "NumBuys", "NumSells")

Returns<-Returns[order(Returns$Window),]
SummaryReturns.1<-SummaryReturns.1[order(SummaryReturns.1$Window),]

LWD1<-1
LWD2<-1.5
XLIM<-c(-10, 180)
YLIM<-c(min(SummaryReturns$RelativeAvgPrice)-10, max(SummaryReturns$RelativeAvgPrice)+10)


### Which prices and timeframes to use (average price and time in window, first, or last)
Measures<-list(Average=c("TimeAvgTradePhase", "RelativeAvgPrice", "TimeAvgTradePhase", "RelativeAvgPrice", "Avg. price", "purple"),
               Last=c("TimeLastTradePhase", "RelativeLastPrice", "TimeLastTradePhase", "RelativeLastPrice", "Last price", "red"),
               First=c("TimeFirstTradePhase", "RelativeFirstPrice", "TimeFirstTradePhase", "RelativeFirstPrice", "First price", "blue"),
               Midpoint=c("TimeMidpointPhase", "RelativeMidpoint", "TimeMidpointPhase", "RelativeMidpoint", "Midpoint", "dark green"),
               Midpoint.1=c("TimeMidpointPhase", "RelativeMidpoint.1", "TimeMidpointPhase", "RelativeMidpoint.1", "Midpoint.1", "brown"))


timeandprice<-Measures[["Midpoint.1"]] # Choose "Average", "First" or "Last" price, "Midpoint" or "Midpoint.1"

if(F){dev.new("PricePlot")} else {jpeg(paste("R03_PEADPlot_",timeandprice[5],"_T_All_.jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
par(mfrow=c(2,2), mar=c(4,4,2,2))
for (Type in c(1,-1)) {
  for (Treat in 1:2){
    
    Temp1<-0
    
    for (Phase in 1:length(unique(Returns$Phase.ID))) {
      
      if (Temp1==0) {
        Temp1<-Temp1+1
        plot(x=Returns[Returns$TreatmentPEAD==Treat&
                         Returns$R.Type==Type&
                         Returns$Phase.ID==Phase,][18:36,timeandprice[1]],
             y=Returns[Returns$TreatmentPEAD==Treat&
                         Returns$R.Type==Type&
                         Returns$Phase.ID==Phase,][18:36,timeandprice[2]], 
             type="l", col="grey90", lwd=LWD1, xlim=XLIM, ylim=YLIM, xlab="Time relative to announcement (seconds)", ylab=paste(timeandprice[5], "in window (relative to previous phase's FV)"), 
             cex.main=0.7, cex.axis=0.7, cex.lab=0.7, 
             main=paste("Treatment: ",c("Base","Correlated")[Treat],", Type: ",Type, ", Surprise: All",sep="")) # Plots market 1
        
      } else {
        Temp1<-Temp1+1
        lines(x=Returns[Returns$TreatmentPEAD==Treat&
                          Returns$R.Type==Type&
                          Returns$Phase.ID==Phase,][18:36,timeandprice[1]],
              y=Returns[Returns$TreatmentPEAD==Treat&
                          Returns$R.Type==Type&
                          Returns$Phase.ID==Phase,][18:36,timeandprice[2]], type="l", col="grey90", lwd=LWD1) # Plots market 2+
      }
      
      if (Phase==length(unique(Returns$Phase.ID))) {
        print("Plot done, Avg remaining")
        lines(x=SummaryReturns.1[SummaryReturns.1$TreatmentPEAD==Treat&
                                   SummaryReturns.1$R.Type==Type,][,timeandprice[3]],
              y=SummaryReturns.1[SummaryReturns.1$TreatmentPEAD==Treat&
                                   SummaryReturns.1$R.Type==Type,][,timeandprice[4]], type="o", col=timeandprice[6], lwd=LWD2)
        
        abline(v=0, type="l", col="black", lwd=LWD1) # Adds announcement time
        abline(v=c(-10, 10), type="l", lty=3, col="black", lwd=LWD1) # Adds announcement windows
        lines(x=XLIM, y=c(100,100), type="l", col="black", lwd=LWD1)
        lines(x=c(SummaryReturns.1[SummaryReturns.1$TreatmentPEAD==Treat&SummaryReturns.1$R.Type==Type,][19,timeandprice[3]],XLIM[2]), 
              y=rep(SummaryReturns.1[SummaryReturns.1$TreatmentPEAD==Treat&SummaryReturns.1$R.Type==Type,][19,timeandprice[4]],2),
              type="l", lty=3, col=timeandprice[6], lwd=LWD2)
      }
    }
  }
  #dev.copy(jpeg,paste("PEADPlot_Treat.",c("Base","Correl")[Treat],".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
}
dev.off()

################################### R04 ##########################################

### Long-Short returns compared to last period's FV (incl. surprise). Measured from window after announcement to last window [e.g., 150:170 sec from announcement]
# Midpoint.1 Version
SummaryReturns[,]
LongShort<-data.frame(R.Type=integer(), TreatmentPEAD=integer(), Surprise=integer(), Return=numeric(), FV.Change=numeric())
Temp<-0
for (Type in c(-1,1)) {
  for (Treat in 1:2) {
    for (Surprise in 0:1) {
      Temp<-Temp+1
      LongShort[Temp,]<-rep(-77777, length(names(LongShort)))
      LongShort$R.Type[Temp]<-Type
      LongShort$TreatmentPEAD[Temp]<-Treat
      LongShort$Surprise[Temp]<-Surprise
      LongShort$Return[Temp]<- ((SummaryReturns$RelativeMidpoint.1[SummaryReturns$R.Type==Type & SummaryReturns$TreatmentPEAD==Treat & SummaryReturns$Surprise==Surprise & SummaryReturns$Window==17]/
                                   SummaryReturns$RelativeMidpoint.1[SummaryReturns$R.Type==Type & SummaryReturns$TreatmentPEAD==Treat & SummaryReturns$Surprise==Surprise & SummaryReturns$Window==0])-1)
      LongShort$FV.Change[Temp]<-mean(Returns[Returns$R.Type==Type & Returns$TreatmentPEAD==Treat & Returns$Surprise==Surprise,"FV"]/
                                        Returns[Returns$R.Type==Type & Returns$TreatmentPEAD==Treat & Returns$Surprise==Surprise,"FV.Previous"])-1
    }
  }
}


LongShortSummary<-data.frame(TreatmentPEAD=integer(), Surprise=integer(), LongReturn=numeric(), ShortReturn=numeric(), LongShortReturn=numeric(), Long.FV.Change=numeric(), Short.FV.Change=numeric(), LongShort.FV.Change=numeric())
Temp<-0
for (Treat in 1:2) {
  for (Surprise in 0:1) {
    Temp<-Temp+1
    LongShortSummary[Temp,]<-rep(-77777, length(names(LongShortSummary)))
    LongShortSummary$TreatmentPEAD[Temp]<-Treat
    LongShortSummary$Surprise[Temp]<-Surprise
    LongShortSummary$LongReturn[Temp]<- LongShort$Return[LongShort$R.Type==1 & LongShort$TreatmentPEAD==Treat & LongShort$Surprise==Surprise]
    LongShortSummary$ShortReturn[Temp]<- LongShort$Return[LongShort$R.Type==-1 & LongShort$TreatmentPEAD==Treat & LongShort$Surprise==Surprise]
    LongShortSummary$LongShortReturn[Temp]<- LongShortSummary$LongReturn[Temp]-LongShortSummary$ShortReturn[Temp]
    LongShortSummary$Long.FV.Change[Temp]<- LongShort$FV.Change[LongShort$R.Type==1 & LongShort$TreatmentPEAD==Treat & LongShort$Surprise==Surprise]
    LongShortSummary$Short.FV.Change[Temp]<- LongShort$FV.Change[LongShort$R.Type==-1 & LongShort$TreatmentPEAD==Treat & LongShort$Surprise==Surprise]
    LongShortSummary$LongShort.FV.Change[Temp]<- LongShortSummary$Long.FV.Change[Temp]-LongShortSummary$Short.FV.Change[Temp]
  }
}
LongShortSummary

################################### R05 ##########################################

### Degree of adjustment from previous FV to new FV over time windows

# Create new table from Returns
Adjustment<-Returns[Returns$R.Type!=0 & Returns$Window>=(-5), c( "Market", "R.Session", "Period", "R.Phase", 
                                                                 "Phase.ID", "Window", 
                                                                 "TreatmentPEAD", "R.Type", "Surprise", "FV", "FV.Previous", 
                                                                 "AvgPrice", "LastPrice", "FirstPrice", "Midpoint", "Midpoint.1",
                                                                 "TimeLastTradePhase", "TimeAvgTradePhase", "TimeFirstTradePhase", "TimeMidpointPhase")]
Adjustment$Adjustment.Avg<- 100*(Adjustment$AvgPrice-Adjustment$FV.Previous)/(Adjustment$FV-Adjustment$FV.Previous)
Adjustment$Adjustment.First<- 100*(Adjustment$FirstPrice-Adjustment$FV.Previous)/(Adjustment$FV-Adjustment$FV.Previous)
Adjustment$Adjustment.Last<- 100*(Adjustment$LastPrice-Adjustment$FV.Previous)/(Adjustment$FV-Adjustment$FV.Previous)
Adjustment$Adjustment.Midpoint<- 100*(Adjustment$Midpoint-Adjustment$FV.Previous)/(Adjustment$FV-Adjustment$FV.Previous)
Adjustment$Adjustment.Midpoint.1<- 100*(Adjustment$Midpoint.1-Adjustment$FV.Previous)/(Adjustment$FV-Adjustment$FV.Previous)
Adjustment<-Adjustment[order(Adjustment$Phase.ID, Adjustment$Window),]


# Create Summary of adjustments for adjustment metric chosen
SummaryAdjustment<-aggregate(cbind(TimeAvgTrade=Adjustment$TimeAvgTradePhase, 
                                   TimeLastTrade=Adjustment$TimeLastTradePhase,
                                   TimeFirstTrade=Adjustment$TimeFirstTradePhase,
                                   TimeMidpoint=Adjustment$TimeMidpointPhase,
                                   Adjustment.Avg=Adjustment$Adjustment.Avg,
                                   Adjustment.First=Adjustment$Adjustment.First,
                                   Adjustment.Last=Adjustment$Adjustment.Last,
                                   Adjustment.Midpoint=Adjustment$Adjustment.Midpoint,
                                   Adjustment.Midpoint.1=Adjustment$Adjustment.Midpoint.1)~
                               Adjustment$R.Type+
                               Adjustment$TreatmentPEAD+
                               Adjustment$Surprise+
                               Adjustment$Window,
                             Adjustment, 
                             mean)

names(SummaryAdjustment)<-c("R.Type", "TreatmentPEAD", "Surprise", "Window", 
                            "TimeAvgTradePhase", "TimeLastTradePhase", "TimeFirstTradePhase", "TimeMidpointPhase", 
                            "Adjustment.Avg", "Adjustment.First", "Adjustment.Last", "Adjustment.Midpoint", "Adjustment.Midpoint.1")


# Add adjustment percentage (using the preferred price metric)
Measures<-list(Average=c("TimeAvgTradePhase", "Adjustment.Avg", "TimeAvgTradePhase", "Adjustment.Avg", "Avg. price", "purple"),
               Last=c("TimeLastTradePhase", "Adjustment.Last", "TimeLastTradePhase", "Adjustment.Last", "Last price", "red"),
               First=c("TimeFirstTradePhase", "Adjustment.First", "TimeFirstTradePhase", "Adjustment.First", "First price", "blue"),
               Midpoint=c("TimeMidpointPhase", "Adjustment.Midpoint", "TimeMidpointPhase", "Adjustment.Midpoint", "Midpoint", "dark green"),
               Midpoint.1=c("TimeMidpointPhase", "Adjustment.Midpoint.1", "TimeMidpointPhase", "Adjustment.Midpoint.1", "Midpoint.1", "brown"))


# Choose average, first or last price or midpoint and timing per window
timeandprice<-Measures[["Average"]] 


### Plot Adjustments
LWD1<-1
LWD2<-1.5
XLIM<-c(-50, 180)
YLIM<-c(-100, 200)

for (Treat in 1:2){
  if(F){dev.new("PricePlot")} else {jpeg(paste("R05_PEADPlot_",timeandprice[5],"_T_",c("Base", "Correl")[Treat],"_",".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
  par(mfrow=c(2,2), mar=c(4,4,2,2))
  
  for (Type in c(1,-1)) {
    ifelse(Type==1, YLIM.x<-YLIM, YLIM.x<-rev(YLIM))
    
    for (Surprise in 0:1) {
      Temp1<-0
      
      for (Phase in unique(Adjustment$Phase.ID)) {
        
        if (Temp1==0) {
          Temp1<-Temp1+1
          plot(x=Adjustment[Adjustment$TreatmentPEAD==Treat&
                              Adjustment$R.Type==Type&
                              Adjustment$Surprise==Surprise&
                              Adjustment$Phase.ID==Phase,][,timeandprice[1]],
               y=Adjustment[Adjustment$TreatmentPEAD==Treat&
                              Adjustment$R.Type==Type&
                              Adjustment$Surprise==Surprise&
                              Adjustment$Phase.ID==Phase,][,timeandprice[2]], 
               type="l", col="grey90", lwd=LWD1, xlim=XLIM, ylim=YLIM.x, xlab="Time relative to announcement (seconds)", ylab=paste(timeandprice[5], "based adjustment to new FV (0 = Old FV, 100 = New FV)"), 
               cex.main=0.7, cex.axis=0.7, cex.lab=0.7, 
               main=paste("Treatment: ",c("Base","Correlated")[Treat],", Type: ",Type, ", Surprise: ",paste(c("No","Yes")[Surprise+1],sep=""))) # Plots market 1
          
        } else {
          Temp1<-Temp1+1
          lines(x=Adjustment[Adjustment$TreatmentPEAD==Treat&
                               Adjustment$R.Type==Type&
                               Adjustment$Surprise==Surprise&
                               Adjustment$Phase.ID==Phase,][,timeandprice[1]],
                y=Adjustment[Adjustment$TreatmentPEAD==Treat&
                               Adjustment$R.Type==Type&
                               Adjustment$Surprise==Surprise&
                               Adjustment$Phase.ID==Phase,][,timeandprice[2]], type="l", col="grey90", lwd=LWD1) # Plots market 2+
        }
        
        if (Phase==max(Adjustment$Phase.ID)) {
          
          print("Plot done, Avg remaining")
          
          abline(v=c(-10, 0, 10), type="l", lty=3, col="black", lwd=LWD1) # Adds announcement windows
          lines(x=XLIM, y=c(0,0), type="l", lty=3, col="black", lwd=LWD1) # Adds old baseline at 0
          lines(x=XLIM, y=c(100,100), type="l", lty=3, col="black", lwd=LWD1) # Adds new baseline at 100
          lines(x=c(XLIM[1],0), y=c(0,0), type="l", col="black", lwd=LWD2) # Adds previous FV from -10:0
          lines(x=c(0,0), y=c(0,100), type="l", col="black", lwd=LWD2) # Adds vertical line to new FV level at announcement time
          lines(x=c(0, XLIM[2]), y=c(100,100), type="l", col="black", lwd=LWD2) # Adds new FV from 0:170
          
          lines(x=SummaryAdjustment[SummaryAdjustment$TreatmentPEAD==Treat&
                                      SummaryAdjustment$R.Type==Type&
                                      SummaryAdjustment$Surprise==Surprise,][,timeandprice[3]],
                y=SummaryAdjustment[SummaryAdjustment$TreatmentPEAD==Treat&
                                      SummaryAdjustment$R.Type==Type&
                                      SummaryAdjustment$Surprise==Surprise,][,timeandprice[4]], type="o", col=timeandprice[6], lwd=LWD2)
          
          
          
        }
      }
    }
  }
  #dev.copy(jpeg,paste("PEADPlot_Treat.",c("Base","Correl")[Treat],".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
  dev.off()  
}

### Adjustment without Surprise consideration

# Create new table from Returns (Adjustment.1, without aggregating by Surprise variable)
Adjustment.1<-Returns[Returns$R.Type!=0 & Returns$Window>=(-5), c( "Market", "R.Session", "Period", "R.Phase", 
                                                                   "Phase.ID", "Window", 
                                                                   "TreatmentPEAD", "R.Type", "Surprise", "FV", "FV.Previous", 
                                                                   "AvgPrice", "LastPrice", "FirstPrice", "Midpoint", "Midpoint.1",
                                                                   "TimeLastTradePhase", "TimeAvgTradePhase", "TimeFirstTradePhase", "TimeMidpointPhase")]
Adjustment.1$Adjustment.Avg<- 100*(Adjustment.1$AvgPrice-Adjustment.1$FV.Previous)/(Adjustment.1$FV-Adjustment.1$FV.Previous)
Adjustment.1$Adjustment.First<- 100*(Adjustment.1$FirstPrice-Adjustment.1$FV.Previous)/(Adjustment.1$FV-Adjustment.1$FV.Previous)
Adjustment.1$Adjustment.Last<- 100*(Adjustment.1$LastPrice-Adjustment.1$FV.Previous)/(Adjustment.1$FV-Adjustment.1$FV.Previous)
Adjustment.1$Adjustment.Midpoint<- 100*(Adjustment.1$Midpoint-Adjustment.1$FV.Previous)/(Adjustment.1$FV-Adjustment.1$FV.Previous)
Adjustment.1$Adjustment.Midpoint.1<- 100*(Adjustment.1$Midpoint.1-Adjustment.1$FV.Previous)/(Adjustment.1$FV-Adjustment.1$FV.Previous)
Adjustment.1<-Adjustment.1[order(Adjustment.1$Phase.ID, Adjustment.1$Window),]


# Create Summary of adjustments for adjustment metric chosen
SummaryAdjustment.1<-aggregate(cbind(TimeAvgTrade=Adjustment.1$TimeAvgTradePhase, 
                                     TimeLastTrade=Adjustment.1$TimeLastTradePhase,
                                     TimeFirstTrade=Adjustment.1$TimeFirstTradePhase,
                                     TimeMidpoint=Adjustment.1$TimeMidpointPhase,
                                     Adjustment.Avg=Adjustment.1$Adjustment.Avg,
                                     Adjustment.First=Adjustment.1$Adjustment.First,
                                     Adjustment.Last=Adjustment.1$Adjustment.Last,
                                     Adjustment.Midpoint=Adjustment.1$Adjustment.Midpoint,
                                     Adjustment.Midpoint.1=Adjustment.1$Adjustment.Midpoint.1)~
                                 Adjustment.1$R.Type+
                                 Adjustment.1$TreatmentPEAD+
                                 Adjustment.1$Window,
                               Adjustment.1, 
                               mean)

names(SummaryAdjustment.1)<-c("R.Type", "TreatmentPEAD", "Window", 
                              "TimeAvgTradePhase", "TimeLastTradePhase", "TimeFirstTradePhase", "TimeMidpointPhase", 
                              "Adjustment.Avg", "Adjustment.First", "Adjustment.Last", "Adjustment.Midpoint", "Adjustment.Midpoint.1")



# Add adjustment percentage (using the preferred price metric)
Measures<-list(Average=c("TimeAvgTradePhase", "Adjustment.Avg", "TimeAvgTradePhase", "Adjustment.Avg", "Avg. price", "purple"),
               Last=c("TimeLastTradePhase", "Adjustment.Last", "TimeLastTradePhase", "Adjustment.Last", "Last price", "red"),
               First=c("TimeFirstTradePhase", "Adjustment.First", "TimeFirstTradePhase", "Adjustment.First", "First price", "blue"),
               Midpoint=c("TimeMidpointPhase", "Adjustment.Midpoint", "TimeMidpointPhase", "Adjustment.Midpoint", "Midpoint", "dark green"),
               Midpoint.1=c("TimeMidpointPhase", "Adjustment.Midpoint.1", "TimeMidpointPhase", "Adjustment.Midpoint.1", "Midpoint.1", "brown"))


# Choose "Average", "First" or "Last" price or "Midpoint" or "Midpoint.1"
timeandprice<-Measures[["Midpoint.1"]] 


### Plot Adjustments
LWD1<-1
LWD2<-1.5
XLIM<-c(-50, 180)
YLIM<-c(-100, 200)

if(F){dev.new("PricePlot")} else {jpeg(paste("R05_PEADPlot_",timeandprice[5],"_T_All_.jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
par(mfrow=c(2,2), mar=c(4,4,2,2))

for (Type in c(1,-1)) {
  ifelse(Type==1, YLIM.x<-YLIM, YLIM.x<-rev(YLIM))
  for (Treat in 1:2){
    
    Temp1<-0
    
    for (Phase in unique(Adjustment.1$Phase.ID)) {
      
      if (Temp1==0) {
        Temp1<-Temp1+1
        
        plot(x=Adjustment.1[Adjustment.1$TreatmentPEAD==Treat&
                              Adjustment.1$R.Type==Type&
                              Adjustment.1$Phase.ID==Phase,][,timeandprice[1]],
             y=Adjustment.1[Adjustment.1$TreatmentPEAD==Treat&
                              Adjustment.1$R.Type==Type&
                              Adjustment.1$Phase.ID==Phase,][,timeandprice[2]], 
             type="l", col="grey90", lwd=LWD1, xlim=XLIM, ylim=YLIM.x, xlab="Time relative to announcement (seconds)", ylab=paste(timeandprice[5], "based Adjustment.1 to new FV (0 = Old FV, 100 = New FV)"), 
             cex.main=0.7, cex.axis=0.7, cex.lab=0.7, 
             main=paste("Treatment: ",c("Base","Correlated")[Treat],", Type: ",Type, ", Surprise: All",sep="")) # Plots market 1
      } else {
        Temp1<-Temp1+1
        lines(x=Adjustment.1[Adjustment.1$TreatmentPEAD==Treat&
                               Adjustment.1$R.Type==Type&
                               Adjustment.1$Phase.ID==Phase,][,timeandprice[1]],
              y=Adjustment.1[Adjustment.1$TreatmentPEAD==Treat&
                               Adjustment.1$R.Type==Type&
                               Adjustment.1$Phase.ID==Phase,][,timeandprice[2]], type="l", col="grey90", lwd=LWD1) # Plots market 2+
      }
      
      if (Phase==max(Adjustment.1$Phase.ID)) { 
        
        print("Plot done, Avg remaining")
        
        abline(v=c(-10, 0, 10), type="l", lty=3, col="black", lwd=LWD1) # Adds announcement windows
        lines(x=XLIM, y=c(0,0), type="l", lty=3, col="black", lwd=LWD1) # Adds old baseline at 0
        lines(x=XLIM, y=c(100,100), type="l", lty=3, col="black", lwd=LWD1) # Adds new baseline at 100
        lines(x=c(XLIM[1],0), y=c(0,0), type="l", col="black", lwd=LWD2) # Adds previous FV from -10:0
        lines(x=c(0,0), y=c(0,100), type="l", col="black", lwd=LWD2) # Adds vertical line to new FV level at announcement time
        lines(x=c(0, XLIM[2]), y=c(100,100), type="l", col="black", lwd=LWD2) # Adds new FV from 0:170
        
        lines(x=SummaryAdjustment.1[SummaryAdjustment.1$TreatmentPEAD==Treat&
                                      SummaryAdjustment.1$R.Type==Type,][,timeandprice[3]],
              y=SummaryAdjustment.1[SummaryAdjustment.1$TreatmentPEAD==Treat&
                                      SummaryAdjustment.1$R.Type==Type,][,timeandprice[4]], type="o", col=timeandprice[6], lwd=LWD2)
        
      }
    }
  }
  #dev.copy(jpeg,paste("PEADPlot_Treat.",c("Base","Correl")[Treat],".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
}
dev.off()  

################################### R06 ##########################################

### Microstructure (MS) analysis

# Get all returns from Returns table (for announcements 1:4, 100sec before and after the announcement)
MS<-Returns[Returns$R.Type!=0, c( "Market", "R.Session", "Period", "R.Phase", 
                                  "Phase.ID", "Window", "WindowStart", "WindowEnd",
                                  "TreatmentPEAD", "R.Type", "Surprise", "FV", "FV.Previous", 
                                  "Spread.pct", "Spread.ECU", "Ask.depth", "Bid.depth", 
                                  "NumTransactions", "BuyOffersCreated", "SellOffersCreated", "BuyOffersCancelled", "SellOffersCancelled", "NumBuys", "NumSells")]

# Aggregate 10sec windows into 20sec Windows and get standard deviation of transaction prices into the windows
MS$Window<-floor(MS$Window/2)
Temp<-0
for (Phase in unique(MS$Phase.ID)) {
  Temp<-Temp+1
  for (Window in min(MS$Window):max(MS$Window)) {
    
    MS$WindowStart[MS$Phase.ID==Phase&MS$Window==Window] <- min(MS$WindowStart[MS$Phase.ID==Phase&MS$Window==Window])
    MS$WindowEnd[MS$Phase.ID==Phase&MS$Window==Window] <- max(MS$WindowEnd[MS$Phase.ID==Phase&MS$Window==Window])
    
    MS$Spread.pct[MS$Phase.ID==Phase&MS$Window==Window] <- mean(MS$Spread.pct[MS$Phase.ID==Phase&MS$Window==Window])
    MS$Spread.ECU[MS$Phase.ID==Phase&MS$Window==Window] <- mean(MS$Spread.ECU[MS$Phase.ID==Phase&MS$Window==Window])
    MS$Ask.depth[MS$Phase.ID==Phase&MS$Window==Window] <- mean(MS$Ask.depth[MS$Phase.ID==Phase&MS$Window==Window])
    MS$Bid.depth[MS$Phase.ID==Phase&MS$Window==Window] <- mean(MS$Bid.depth[MS$Phase.ID==Phase&MS$Window==Window])
    
    MS$NumTransactions[MS$Phase.ID==Phase&MS$Window==Window] <- sum(MS$NumTransactions[MS$Phase.ID==Phase&MS$Window==Window])
    
    MS$BuyOffersCreated[MS$Phase.ID==Phase&MS$Window==Window] <- sum(MS$BuyOffersCreated[MS$Phase.ID==Phase&MS$Window==Window])
    MS$SellOffersCreated[MS$Phase.ID==Phase&MS$Window==Window] <- sum(MS$SellOffersCreated[MS$Phase.ID==Phase&MS$Window==Window])
    MS$BuyOffersCancelled[MS$Phase.ID==Phase&MS$Window==Window] <- sum(MS$BuyOffersCancelled[MS$Phase.ID==Phase&MS$Window==Window])
    MS$SellOffersCancelled[MS$Phase.ID==Phase&MS$Window==Window] <- sum(MS$SellOffersCancelled[MS$Phase.ID==Phase&MS$Window==Window])
    MS$NumBuys[MS$Phase.ID==Phase&MS$Window==Window] <- sum(MS$NumBuys[MS$Phase.ID==Phase&MS$Window==Window])
    MS$NumSells[MS$Phase.ID==Phase&MS$Window==Window] <- sum(MS$NumSells[MS$Phase.ID==Phase&MS$Window==Window])
  }
  print(paste("Phase", Temp, "of", length(unique(MS$Phase.ID))))
}

MS<-unique(MS)

for (Market in 1:NumMarkets) {
  for (Session in 1:NumSessions) {
    for (Period in 1:4) {
      for (Phase in 1:4) {
        for (Window in min(MS$Window):max(MS$Window)) {
          
          MS$Prices.SD[MS$Market==Market & MS$R.Session==Session & MS$Period==Period & MS$R.Phase==Phase & MS$Window==Window]<-
            sd(Data$transactions$Price[Data$transactions$Market==Market &
                                         Data$transactions$R.Session==Session &
                                         Data$transactions$Period==Period &
                                         Data$transactions$R.TradeTime > MS$WindowStart[MS$Market==Market & MS$R.Session==Session & MS$Period==Period & MS$R.Phase==Phase & MS$Window==Window] &
                                         Data$transactions$R.TradeTime <= MS$WindowEnd[MS$Market==Market & MS$R.Session==Session & MS$Period==Period & MS$R.Phase==Phase & MS$Window==Window]])
          
        }
      }
    }
    print(paste("Session", Session, "of", NumSessions, "in Market", Market, "of", NumMarkets))
  }
}

# Add relative values for all MS variables (relative to average per Phase.ID)
Temp<-0
for (Phase in unique(MS$Phase.ID)) {
  Temp<-Temp+1
  for (Window in min(MS$Window):max(MS$Window)) {
    
    MS$Spread.pct.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$Spread.pct[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$Spread.pct[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$Spread.ECU.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$Spread.ECU[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$Spread.ECU[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$Ask.depth.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$Ask.depth[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$Ask.depth[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$Bid.depth.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$Bid.depth[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$Bid.depth[MS$Phase.ID==Phase], na.rm = TRUE)
    
    
    MS$NumTransactions.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$NumTransactions[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$NumTransactions[MS$Phase.ID==Phase], na.rm = TRUE)
    
    MS$BuyOffersCreated.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$BuyOffersCreated[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$BuyOffersCreated[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$SellOffersCreated.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$SellOffersCreated[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$SellOffersCreated[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$BuyOffersCancelled.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$BuyOffersCancelled[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$BuyOffersCancelled[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$SellOffersCancelled.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$SellOffersCancelled[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$SellOffersCancelled[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$NumBuys.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$NumBuys[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$NumBuys[MS$Phase.ID==Phase], na.rm = TRUE)
    MS$NumSells.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$NumSells[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$NumSells[MS$Phase.ID==Phase], na.rm = TRUE)
    
    MS$Prices.SD.Relative[MS$Phase.ID==Phase&MS$Window==Window] <- MS$Prices.SD[MS$Phase.ID==Phase&MS$Window==Window] / mean(MS$Prices.SD[MS$Phase.ID==Phase], na.rm = TRUE)
    
  }
  print(paste("Phase", Temp, "of", length(unique(MS$Phase.ID))))
}


# Summarize MS over Treatment and Window
SummaryMS<-data.frame(TreatmentPEAD=c(rep(unique(MS$TreatmentPEAD)[1],length(unique(MS$Window))), rep(unique(MS$TreatmentPEAD)[2],length(unique(MS$Window)))), 
                      Window=rep(unique(MS$Window),length(unique(MS$TreatmentPEAD))))

for (Treat in 1:2) {
  for (Window in min(MS$Window):max(MS$Window)) {
    SummaryMS$Prices.SD.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Prices.SD.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #3
    SummaryMS$Spread.pct.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Spread.pct.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #4
    SummaryMS$Spread.ECU.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Spread.ECU.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #5
    SummaryMS$Ask.depth.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Ask.depth.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #6
    SummaryMS$Bid.depth.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Bid.depth.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #7
    SummaryMS$NumTransactions.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$NumTransactions.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #8
    SummaryMS$NumBuys.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$NumBuys.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #9
    SummaryMS$NumSells.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$NumSells.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #10
    SummaryMS$BuyOffersCreated.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$BuyOffersCreated.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #11
    SummaryMS$SellOffersCreated.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$SellOffersCreated.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #12
    SummaryMS$BuyOffersCancelled.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$BuyOffersCancelled.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #13
    SummaryMS$SellOffersCancelled.Relative[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$SellOffersCancelled.Relative[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #14
    
    SummaryMS$Prices.SD[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Prices.SD[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #15
    SummaryMS$Spread.pct[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Spread.pct[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #16
    SummaryMS$Spread.ECU[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Spread.ECU[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #17
    SummaryMS$Ask.depth[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Ask.depth[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #18
    SummaryMS$Bid.depth[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$Bid.depth[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #19
    SummaryMS$NumTransactions[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$NumTransactions[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #20
    SummaryMS$NumBuys[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$NumBuys[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #21
    SummaryMS$NumSells[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$NumSells[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #22
    SummaryMS$BuyOffersCreated[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$BuyOffersCreated[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #23
    SummaryMS$SellOffersCreated[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$SellOffersCreated[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #24
    SummaryMS$BuyOffersCancelled[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$BuyOffersCancelled[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #25
    SummaryMS$SellOffersCancelled[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window==Window] <- mean(MS$SellOffersCancelled[MS$TreatmentPEAD==Treat & MS$Window==Window], na.rm = TRUE) #26
  }
}



# Plot SummaryMS variables
for (Treat in 1:2) {
  
  if(F){dev.new("PricePlot")} else {jpeg(paste("R06_MS_T_", c("Base", "Correl")[Treat],"_.jpeg",sep=""), bg="white", width=3000, height=2000, res=300)} # Opens plot device
  par(mfrow=c(1,1), mar=c(4,4,2,2), oma=c(0,0,3,0))
  layout(matrix(1:12, nrow=3, byrow=TRUE))
  
  
  for (i in 3:14) {
    barplot(SummaryMS[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window>=-5 & SummaryMS$Window<=4, i], 
            main=names(SummaryMS)[i], xlab = "20 sec. window relative to announcement", ylab = "(Relative) distribution", 
            names.arg = -5:4, space = 0.5, cex.axis = 0.6, cex.names = 0.6, cex=0.4, cex.lab=0.6, cex.main=0.8)
    
    abline(v=1.70+mean(as.numeric(barplot(SummaryMS[SummaryMS$TreatmentPEAD==Treat & SummaryMS$Window>=-5 & SummaryMS$Window<=4, i], plot = FALSE))), col="blue", lty=5, lwd=1.5)
    #abline(h=mean(SummaryMS[SummaryMS$TreatmentPEAD==Treat, i]))
    abline(h=mean(MS[MS$TreatmentPEAD==Treat, names(SummaryMS)[i]], na.rm = TRUE))
    print(names(SummaryMS)[i])
  }
  print(paste("Treatment", Treat, "done"))
  title(paste("Microstructure variables, relative development around announcement (Treatment: ", c("BASE", "CORR")[Treat], ")", sep=""), outer = TRUE)
  dev.off()  
}

#layout(matrix(1:12, nrow=3, byrow=TRUE))
#layout.show(n=12)


################################### R07 ##########################################

### Regression of PEAD returns

# Get data from Returns table (here wit Midpoint.1)
RegData.1.Temp<-Returns[Returns$R.Phase>0 & Returns$Window>=0, c("R.Session", "Period", "R.Phase", "Phase.ID", "Window", "Market", 
                                                                 "R.Type", "TreatmentPEAD", "Surprise",  
                                                                 "FV", "FV.Previous",
                                                                 "NumTransactions", "RelativeMidpoint.1")]

# Set Surprise variable to 0 for all TreatmentPEAD==1
#RegData.1.Temp$Surprise[RegData.1.Temp$TreatmentPEAD==1]<-0

# Create new summary table aggregated by Phase.ID, including corresponding R.Type, TreatmentPEAD, Surprise, R.Phase
RegData.1<-data.frame(Phase.ID=integer(), R.Type=integer(), TreatmentPEAD=integer(), Surprise=integer(), R.Phase=integer(), Return=numeric(), FV.Change=numeric())

# Get the return from Window 0 to Window 17
Temp<-0
for (Phase in unique(RegData.1.Temp$Phase.ID)) {
  Temp<-Temp+1
  RegData.1[Temp,]<-rep(-77777, length(names(RegData.1)))
  RegData.1$Phase.ID[Temp]<-Phase
  RegData.1$R.Type[Temp] <- RegData.1.Temp$R.Type[RegData.1.Temp$Phase.ID==Phase][1]
  RegData.1$TreatmentPEAD[Temp] <- RegData.1.Temp$TreatmentPEAD[RegData.1.Temp$Phase.ID==Phase][1]
  RegData.1$Surprise[Temp] <-  RegData.1.Temp$Surprise[RegData.1.Temp$Phase.ID==Phase][1]
  RegData.1$R.Phase[Temp] <-RegData.1.Temp$R.Phase[RegData.1.Temp$Phase.ID==Phase][1]
  RegData.1$Return[Temp] <- (RegData.1.Temp$RelativeMidpoint.1[RegData.1.Temp$Phase.ID==Phase & RegData.1.Temp$Window==17] / 
                               RegData.1.Temp$RelativeMidpoint.1[RegData.1.Temp$Phase.ID==Phase & RegData.1.Temp$Window==0]) - 1
  RegData.1$FV.Change[Temp] <- (RegData.1.Temp$FV[RegData.1.Temp$Phase.ID==Phase][1] / 
                                  RegData.1.Temp$FV.Previous[RegData.1.Temp$Phase.ID==Phase][1]) - 1
  
}

RegData.1$TreatmentPEAD <- as.factor(RegData.1$TreatmentPEAD)
RegData.1$Surprise <- as.factor(RegData.1$Surprise)
RegData.1$R.Type <- as.factor(RegData.1$R.Type)
RegData.1$Return.abs <- abs(RegData.1$Return)
str(RegData.1)



RegData.1
summary(RegData.1$FV.Change[RegData.1$TreatmentPEAD==1 & RegData.1$Surprise==0])
summary(RegData.1$FV.Change[RegData.1$TreatmentPEAD==1 & RegData.1$Surprise==1])
summary(RegData.1$FV.Change[RegData.1$TreatmentPEAD==2 & RegData.1$Surprise==0])
summary(RegData.1$FV.Change[RegData.1$TreatmentPEAD==2 & RegData.1$Surprise==1])


### Run regressions

# Model.01 (no R.Phase, no interactions)
# Adj. R²= 0.4399 ### Type is by far strongest predictor
Model.01 <- lm(Return~
                 R.Type+
                 TreatmentPEAD+
                 Surprise, data = RegData.1)
summary(Model.01)


# Model.02 (no R.Phase, parsimonious. Interaction between Treatment and Type works best)
# Adj. R²= 0.5258 ### Interaction between Type and Treatment strongest predictor, but all variables significant at 0.01 level
Model.02 <- lm(Return~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 TreatmentPEAD*R.Type, data = RegData.1)
summary(Model.02)

# Model.03 (no R.Phase, all interactions)
# Adj. R²= 0.5731 ### Type1*Treatment2*Surprise1 is a very strong predictor, making Type1*Treatment2 insignificant
Model.03 <- lm(Return~
                 R.Type*TreatmentPEAD+
                 R.Type*Surprise+
                 TreatmentPEAD*Surprise+
                 TreatmentPEAD*Surprise*R.Type, data = RegData.1)
summary(Model.03)


testdata<-expand.grid(TreatmentPEAD=unique(RegData.1$TreatmentPEAD), Surprise=unique(RegData.1$Surprise), R.Type=unique(RegData.1$R.Type), R.Phase=unique(RegData.1$R.Phase))
testdata<-cbind(testdata, Prediction=predict(Model.03, newdata = testdata))

testdata<-testdata[order(testdata$Prediction),]

testdata

# Model.04 (with R.Phase, no interactions)
# Adj. R²= 0.4405 ### Similar to Model.01, Type dominates. R.Phase by itself insignificant
Model.04 <- lm(Return~
                 R.Phase+
                 R.Type+
                 Surprise+
                 TreatmentPEAD, data = RegData.1)
summary(Model.04)


# Model.05 (with R.Phase, interaction with Treatment works best)
# Adj. R²= 0.4451 ### All variables significant, except Phase. Phase significant in combination with Treatment
Model.05 <- lm(Return~
                 R.Phase*TreatmentPEAD+
                 R.Type+
                 Surprise+
                 TreatmentPEAD, data = RegData.1)
summary(Model.05)

# Model.06 (with R.Phase, maintain interaction with Treatment. Remaining variables included, including interaction between Treatment and Type from Model.02)
# Adj. R²= 0.5313 ### All variables significant, except Phase. Phase significant in combination with Treatment. Treatment and Type interaction is the strongest
Model.06 <- lm(Return~
                 R.Phase*TreatmentPEAD+
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 TreatmentPEAD*R.Type, data = RegData.1)
summary(Model.06)


# Model.07 (with R.Phase, maintain interaction with Treatment. Remaining variables included, including all interactions from Model.03)
# Adj. R²= 0.5829 ### All variables significant, except Phase. Phase significant in combination with Treatment. All other variables and interactions significant, most strongly between Treatment*Type*Surprise
Model.07 <- lm(Return~
                 R.Phase*TreatmentPEAD+
                 R.Type*TreatmentPEAD+
                 R.Type*Surprise+
                 TreatmentPEAD*Surprise+
                 TreatmentPEAD*Surprise*R.Type, data = RegData.1)
summary(Model.07)


# Model.08 (as Model.07 but including the FV change)
# Adj. R²= 0.6020 ### FV.Change dominates everything, but does not explain everything
Model.08 <- lm(Return~
                 FV.Change+
                 R.Phase*TreatmentPEAD+
                 R.Type*TreatmentPEAD+
                 R.Type*Surprise+
                 TreatmentPEAD*Surprise+
                 TreatmentPEAD*Surprise*R.Type, data = RegData.1)
summary(Model.08)

# Model.09 (FV.Change as only predictor)
# Adj. R²= 0.5602 ### FV.Change alone produces R² almost as large as any other model without FV.Change
Model.09 <- lm(Return~
                 FV.Change, data = RegData.1)
summary(Model.09)


# Model.10 (FV.Change plus Type and Treatment and its interaction)
# Adj. R²= 0.5944 ### When Type is 1 (positive surprise) and Treatment goes from 1 to 2, the net increase 
# from the Treatment alone is (-0.067988+0.089362)= +0.021374 (underreaction relative to Treatment 1). When Type is -1 (negative surprise) the net increase 
# from the Treatment alone is (-0.067988) = -0.067988 (underreaction relative to Treatment 1). 
# Stocks that are going down are often overpriced beforehand and the reaction remains too low when the FV goes down further.
Model.10 <- lm(Return~
                 FV.Change+
                 R.Type*TreatmentPEAD, data = RegData.1)
summary(Model.10)


# Model.11 (FV.Change plus interactions that remain significant, i.e., excluding Treatment*Surprise)
# Adj. R²= 0.6006 ### Interpretation more complex when Surprise is added. Still, multiple variables and interactions significant, despite having FV in. Surprise and FV.Change are tightly related by their definition.
Model.11 <- lm(Return~
                 FV.Change+
                 R.Type*TreatmentPEAD+
                 R.Type*Surprise, data = RegData.1)
summary(Model.11)

# Model.12 (no R.Phase, no interactions, absolute Return as dependent variable)
# Adj. R²= 0.2438 ### Treatment correlated leads to much higher returns. Type -1 has stronger absolute 
# returns (probably because stocks are often overpriced. Surprise also leads to higher returns
Model.12 <- lm(Return.abs~
                 R.Type+
                 TreatmentPEAD+
                 Surprise, data = RegData.1)
summary(Model.12)

# Model.13 (no R.Phase, all interactions, absolute Return as dependent variable)
# Adj. R²= 0.3571 ### All variables significant, and in expected direction 
# (consistently overpriced stocks suggest a negative coefficient for R.Type). Treatment Corr and Surprise has most impact
Model.13 <- lm(Return.abs~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 R.Type*TreatmentPEAD+
                 R.Type*Surprise+
                 TreatmentPEAD*Surprise, data = RegData.1)
summary(Model.13)


# Model.14 (as Model.07 (Adj. R²= 0.5829) with phase but with absolute Return)
# Adj. R²= 0.3708 ### Absolute Return leads to a lower R² than actual return with the same explanatory variables
Model.14 <- lm(Return.abs~
                 R.Phase*TreatmentPEAD+
                 R.Type*TreatmentPEAD+
                 R.Type*Surprise+
                 TreatmentPEAD*Surprise+
                 TreatmentPEAD*Surprise*R.Type, data = RegData.1)
summary(Model.14)


# Model.15 (FV.Change as only predictor for absolute Returns)
# Adj. R²= 0.0048 ### Practically no explanatory value. As expected
Model.15 <- lm(Return.abs~
                 FV.Change, data = RegData.1)
summary(Model.15)


# Model.16 (Like Model.10 but with absolute Returns)
# Adj. R²= 0.3187 ### Less explanatory power overall than Model.10, but all coefficients highly signifcant. FV.Change needs to be in absolute terms or interacted with Type
Model.16 <- lm(Return.abs~
                 FV.Change+
                 R.Type*TreatmentPEAD, data = RegData.1)
summary(Model.16)


# Model.17 (Like Model.16 but interacting FV.Change and Type)
# Adj. R²= 0.3963 ### One unit of change in FV means -0.48 change in returns. 
# If R.Type=1 then one unit change of FV means (-0.484)+1.077=0.5931
Model.17 <- lm(Return.abs~
                 FV.Change+
                 FV.Change*R.Type+
                 R.Type*TreatmentPEAD, data = RegData.1)
summary(Model.17)


# Model.18 (Like Model.16 but with absolute value of FV.Change)
# Adj. R²= 0.3967 ### A FV.Change of one unit means that absolute returns change by 0.57. This is comparable to the FV.Change coefficient in Model.10. Overall, less explanatory power
Model.18 <- lm(Return.abs~
                 abs(FV.Change)+
                 R.Type*TreatmentPEAD, data = RegData.1)
summary(Model.18)


################################### R08 ##########################################

### Return development regression

# Get data from Returns table (here with Midpoint.1, but not relative Version of price metric)
RegData.2<-Returns[Returns$R.Phase>0 & Returns$Window>=-1, c("R.Session", "Period", "R.Phase", "Phase.ID", "Window", "Market", 
                                                             "R.Type", "TreatmentPEAD", "Surprise",  
                                                             "FV", "FV.Previous",
                                                             "NumTransactions", "Midpoint.1")]
RegData.2<-RegData.2[order(RegData.2$Phase.ID, RegData.2$Window),]

# Add Window-to-Window return (abs and pct)
for (row in 1:nrow(RegData.2)) {
  if (RegData.2$Window[row]!=min(RegData.2$Window)) {
    RegData.2$Return.abs[row]<-RegData.2$Midpoint.1[row]-RegData.2$Midpoint.1[row-1]
    RegData.2$Return.pct[row]<-RegData.2$Midpoint.1[row]/RegData.2$Midpoint.1[row-1]-1
  }
}
RegData.2<-RegData.2[RegData.2$Window!=min(RegData.2$Window),]
RegData.2[1:10,]


### Run regressions of Window-to-Window returns (Return.pct provides stronger relationships)

# Model.51 (Window, Type, Treatment and Surprise, no interactions)
# Adj. R²= 0.01974 ### Type is by far strongest predictor. Window hardly significant
Model.51 <- lm(Return.pct~
                 Window+
                 R.Type+
                 TreatmentPEAD+
                 Surprise, data = RegData.2)
summary(Model.51)

# Model.52 (Window, Type, Treatment and Surprise, interaction between Window and Type)
# Adj. R²= 0.03762 ### Type is strong predictor but the interaction with window is negative. 
# Generally, Type is strongest predictor, but further into the Phase, Window*Type dampens effect.
Model.52 <- lm(Return.pct~
                 Window+
                 R.Type+
                 Window*R.Type+
                 TreatmentPEAD+
                 Surprise, data = RegData.2)
summary(Model.52)

# Model.53 (Window, Type, Treatment and Surprise, interaction between Window and Type, and Window and Treatment)
# Adj. R²= 0.03448 ### Addition of interaction between Window and Treatment reduces Adj. R²
Model.53 <- lm(Return.pct~
                 Window+
                 R.Type+
                 Window*R.Type+
                 Window*TreatmentPEAD+
                 TreatmentPEAD+
                 Surprise, data = RegData.2)
summary(Model.53)


################################### R09 ##########################################

### Regression of announcement window returns (how much adjustment happens within announcement window)

# Get data from Adjustment table (here with Midpoint.1, but not relative Version of price metric)
RegData.3<-Adjustment[Adjustment$R.Phase>0 & Adjustment$Window==0, c("R.Session", "Period", "R.Phase", "Phase.ID", "Window", "Market", 
                                                                     "R.Type", "TreatmentPEAD", "Surprise",  
                                                                     "FV", "FV.Previous",
                                                                     "Midpoint.1", "Adjustment.Midpoint.1")]
RegData.3<-RegData.3[order(RegData.3$Phase.ID, RegData.3$Window),]
RegData.3[1:10,]


RegData.3$TreatmentPEAD <- as.factor(RegData.3$TreatmentPEAD)
RegData.3$Surprise <- as.factor(RegData.3$Surprise)
RegData.3$R.Type <- as.factor(RegData.3$R.Type)
RegData.3$FV.Change <- (RegData.3$FV / RegData.3$FV.Previous -1)
str(RegData.3)

# Model.61 (Type, Treatment and Surprise, no interactions)
# Adj. R²= 0.1129 ### Type and especially surprise have strong impact. Type 1 has better adjustment than Type -1. 
# Surprise has positive impact (if there is a surprise the adjustment is better). 
# Treatment has a negative effect but not actually significant
Model.61 <- lm(Adjustment.Midpoint.1~
                 R.Type+
                 TreatmentPEAD+
                 Surprise, data = RegData.3)
summary(Model.61)


# Model.62 (Type, Treatment and Surprise, with interactions between Treatment and other variables)
# Adj. R²= 0.1110 ### Treatment variables or interactions typically have the expected sign but are insignificant
Model.62 <- lm(Adjustment.Midpoint.1~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 Surprise*TreatmentPEAD+
                 R.Type*TreatmentPEAD, data = RegData.3)
summary(Model.62)


# Model.63 (Type, Treatment and Surprise, with interactions between all variables)
# Adj. R²= 0.1111 ### Only Surprise is significant
Model.63 <- lm(Adjustment.Midpoint.1~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 Surprise*TreatmentPEAD+
                 R.Type*TreatmentPEAD+
                 Surprise*R.Type, data = RegData.3)
summary(Model.63)

# Model.64 (Type, Treatment and Surprise, control for FV change)
# Adj. R²= 0.1190 ### Surprise is still the strongest predictor. FV.Change takes away power of Type. 
# Treatment contributes (marginally) positively to underreaction
Model.64 <- lm(Adjustment.Midpoint.1~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 FV.Change, data = RegData.3)
summary(Model.64)

# Model.65 (Type, Treatment and Surprise, control for absolute FV change)
# Adj. R²= 0.1114 ### Surprise is still the strongest predictor. Type also significant
Model.65 <- lm(Adjustment.Midpoint.1~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 abs(FV.Change), data = RegData.3)
summary(Model.65)

################################### R10 ##########################################

### Regression of volatility in announcement window 

RegData.4<-MS[MS$R.Phase>0 & MS$Window==0, c("R.Session", "Period", "R.Phase", "Phase.ID", "Window", "Market", 
                                             "R.Type", "TreatmentPEAD", "Surprise",  
                                             "FV", "FV.Previous",
                                             "Prices.SD", "Prices.SD.Relative")]
RegData.4<-RegData.4[order(RegData.4$Phase.ID, RegData.4$Window),]
RegData.4$TreatmentPEAD <- as.factor(RegData.4$TreatmentPEAD)
RegData.4$Surprise <- as.factor(RegData.4$Surprise)
RegData.4$R.Type <- as.factor(RegData.4$R.Type)
RegData.4$FV.Change <- (RegData.4$FV / RegData.4$FV.Previous -1)
str(RegData.4)
RegData.4[1:10,]


# Model.71 (Type, Treatment and Surprise, no interactions)
# Adj. R²= 0.04633 ### Treatment 2 has strong positive impact on volatility 
# in announcement window, Surprise has an additional positive impact. 
Model.71 <- lm(Prices.SD~
                 R.Type+
                 TreatmentPEAD+
                 Surprise, data = RegData.4)
summary(Model.71)


# Model.72 (Type, Treatment and Surprise, no interactions, control for FV.Change)
# Adj. R²= 0.04382 ### Treatment is the strongest predictor, contributing positively. Surprise also contributes positively. FV.Change does not add anything, since the absolute FV changes are more telling
Model.72 <- lm(Prices.SD~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 FV.Change, data = RegData.4)
summary(Model.72)


# Model.73 (Type, Treatment and Surprise, no interactions, control for FV.Change in absolute terms)
# Adj. R²= 0.06497 ### Since Surprise is a manifestation of FV.Change, it is not adding any power. 
# Treatment does add a marginally significant component beyond FV.Change. FV.Change is the only strong predictor
Model.73 <- lm(Prices.SD~
                 R.Type+
                 TreatmentPEAD+
                 Surprise+
                 abs(FV.Change), data = RegData.4)
summary(Model.73)


################################### R11 ##########################################

### Trend test (is there a trend in prices, by treatment, type)

install.packages("trend")
library("trend")

# Create data frame "Trends.temp" to perform trend test on phase-level
Trends.temp<-data.frame(Phase.ID=integer(1), TreatmentPEAD=integer(1), R.Type=integer(1), Surprise=integer(1), Observation=integer(1),
                        Positive.total=integer(1), Positive.10pct=integer(1), Positive.5pct=integer(1), Positive.1pct=integer(1), Positive.0.1pct=integer(1), Positive.insignificant=integer(1),
                        Negative.total=integer(1), Negative.10pct=integer(1), Negative.5pct=integer(1), Negative.1pct=integer(1), Negative.0.1pct=integer(1), Negative.insignificant=integer(1),
                        No.Trend=integer(1))


for (i in unique(Returns$Phase.ID)) {
  Temp.df <- Returns[Returns$Phase.ID==i,] # Get the desired extract from Returns table
  Temp.df$Midpoint.1[is.na(Temp.df$Midpoint.1)==TRUE] <- Temp.df$Midpoint.1[is.na(Temp.df$Midpoint.1)==FALSE][1] # If midpoint == NA or NaN (especially in Phase 0) all leading NA's and NaN's are replaced with the first non-NA value
  Trends.temp<-rbind(Trends.temp, rep(0, length(Trends)))
  Trends.temp$Phase.ID[i] <- i
  Trends.temp$Observation[i] <- 1
  Trends.temp$TreatmentPEAD[i] <- Temp.df$TreatmentPEAD[1]
  Trends.temp$R.Type[i] <- Temp.df$R.Type[1]
  Trends.temp$Surprise[i] <- Temp.df$Surprise[1]
  Temp.test<-mk.test(Temp.df$Midpoint.1[Temp.df$Window>=0])
  print(paste("Phase.ID:", i, "___Type:", Trends.temp$R.Type[i], "___z-Val:", round(Temp.test$statistic, 4), "___p-Val:", round(Temp.test$p.value,4)))
  if (Temp.test$statistic > 0) { # Positive trend
    
    Trends.temp$Positive.total[i] <- 1
    if (Temp.test$p.value<=0.001) { Trends.temp$Positive.0.1pct[i] <- 1 } # Positive trend at 0.1% level
    if (Temp.test$p.value<=0.01) { Trends.temp$Positive.1pct[i] <- 1 }  # Positive trend at 1% level
    if (Temp.test$p.value<=0.05) { Trends.temp$Positive.5pct[i] <- 1 }  # Positive trend at 5% level
    if (Temp.test$p.value<=0.1) { Trends.temp$Positive.10pct[i] <- 1 }  # Positive trend at 10% level
    
  } else if (Temp.test$statistic < 0) { # Negative trend
    
    Trends.temp$Negative.total[i] <- 1
    if (Temp.test$p.value<=0.001) { Trends.temp$Negative.0.1pct[i] <- 1 } # Negative trend at 0.1% level
    if (Temp.test$p.value<=0.01) { Trends.temp$Negative.1pct[i] <- 1 }  # Negative trend at 1% level
    if (Temp.test$p.value<=0.05) { Trends.temp$Negative.5pct[i] <- 1 }  # Negative trend at 5% level
    if (Temp.test$p.value<=0.1) { Trends.temp$Negative.10pct[i] <- 1 }  # Negative trend at 10% level
    
  } else { # No trend
    
    Trends.temp$No.Trend[i] <- 1
    
  }
}

# Aggregate on level of Treatment, Type, Surprise
Trends<-aggregate(cbind(Observations=Trends.temp$Observation, 
                        Positive.total=Trends.temp$Positive.total,
                        Positive.10pct=Trends.temp$Positive.10pct, 
                        Positive.5pct=Trends.temp$Positive.5pct, 
                        Positive.1pct=Trends.temp$Positive.1pct, 
                        Positive.0.1pct=Trends.temp$Positive.0.1pct, 
                        Positive.insignificant=Trends.temp$Positive.insignificant,
                        Negative.total=Trends.temp$Negative.total,
                        Negative.10pct=Trends.temp$Negative.10pct, 
                        Negative.5pct=Trends.temp$Negative.5pct, 
                        Negative.1pct=Trends.temp$Negative.1pct, 
                        Negative.0.1pct=Trends.temp$Negative.0.1pct, 
                        Negative.insignificant=Trends.temp$Negative.insignificant,
                        No.Trend=Trends.temp$No.Trend)~
                    Trends.temp$R.Type+
                    Trends.temp$TreatmentPEAD+
                    Trends.temp$Surprise,
                  Trends.temp, sum)
names(Trends)[1:3]<-c("R.Type", "TreatmentPEAD", "Surprise")
Trends<-Trends[Trends$TreatmentPEAD!=0,]

# Aggregate on level of Treatment, Type (not surprise)
Trends.1<-aggregate(cbind(Observations=Trends.temp$Observation,
                          Positive.total=Trends.temp$Positive.total,
                          Positive.10pct=Trends.temp$Positive.10pct, 
                          Positive.5pct=Trends.temp$Positive.5pct, 
                          Positive.1pct=Trends.temp$Positive.1pct, 
                          Positive.0.1pct=Trends.temp$Positive.0.1pct, 
                          Positive.insignificant=Trends.temp$Positive.insignificant,
                          Negative.total=Trends.temp$Negative.total,
                          Negative.10pct=Trends.temp$Negative.10pct, 
                          Negative.5pct=Trends.temp$Negative.5pct,
                          Negative.1pct=Trends.temp$Negative.1pct, 
                          Negative.0.1pct=Trends.temp$Negative.0.1pct, 
                          Negative.insignificant=Trends.temp$Negative.insignificant,
                          No.Trend=Trends.temp$No.Trend)~
                      Trends.temp$R.Type+
                      Trends.temp$TreatmentPEAD,
                    Trends.temp, sum)
names(Trends.1)[1:2]<-c("R.Type", "TreatmentPEAD")
Trends.1<-Trends.1[Trends.1$TreatmentPEAD!=0,]



# Plot Trends.1 trends by Treatment and Type
if(F){dev.new("PricePlot")} else {jpeg(paste("R11_Trends_.jpeg",sep=""), bg="white", width=3000, height=2000, res=300)} # Opens plot device
par(mfrow=c(1,1), mar=c(4,4,2,2), oma=c(0,0,3,0))
layout(matrix(1:4, nrow=2, byrow=TRUE))


for (Treat in 1:2) {
  
  for (Type in c(-1,1)) {
    
    temp.trends<- Trends.1[Trends.1$TreatmentPEAD==Treat & Trends.1$R.Type==Type, c("Negative.0.1pct", "Negative.1pct", "Negative.5pct", "Negative.10pct", "Negative.total",
                                                                                    "No.Trend", "Positive.total", "Positive.10pct", "Positive.5pct", "Positive.1pct", "Positive.0.1pct")]
    temp.trends<- 100 * temp.trends / Trends.1$Observations[Trends.1$TreatmentPEAD==Treat & Trends.1$R.Type==Type]
    names(temp.trends) <- c(paste("(-)\n0.1pct"), paste("(-)\n1pct"), paste("(-)\n5pct"), paste("(-)\n10pct"), paste("(-)\ntotal"),
                            paste("No\ntrend"), 
                            paste("(+)\ntotal"), paste("(+)\n10pct"), paste("(+)\n5pct"), paste("(+)\n1pct"), paste("(+)\n0.1pct"))
    
    barplot(as.vector(t(temp.trends[1,])), 
            main=paste("Trends for Treatment: ", c("BASE", "CORR")[Treat], ", Type: ", Type, sep = ""), xlab = "Negative to positive trends at different significance levels", ylab = "Frequency [%]", 
            names.arg = names(temp.trends), ylim = c(0,100),
            space = 0.5, cex.axis = 0.6, cex.names = 0.6, cex=0.4, cex.lab=0.6, cex.main=0.8)
    
    abline(v=c(6.25, 10.75), col="black", lty=5, lwd=1)
    
  }
  print(paste("Treatment", Treat, "done"))
  title("Mann-Kendall trend test for Returns from window 0:17 by Treatment and Type", outer = TRUE)
  
}  
dev.off()  

layout.show(4)


save.image("C:/Users/Josef Fink/Documents/Privat/PhD/Paper/Results/00_R/PEAD_190729.RData")













