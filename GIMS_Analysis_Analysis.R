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
RemovePracticePeriodTables<-Tables[!Tables=="contracts"&!Tables=="session"] # Subset of tables which the practice period should be removed from. This includes only tables which have life < session.

Params$ShowPlots<-F #Should plots be shown on screen or only written to disk?
Params$UpdateData<-F #Should data be newly read-in?
Params$RemovePracticePeriods<-T
Params$PlotFileType<-"jpeg"
setwd("d:/institut/#CurrentWork/PEAD/Results")

if(Params$UpdateData){
    source("D:/Institut/#CurrentWork/GIMS/GitLab/GIMS/GIMS_Analysis_DataPreparation.r") # Reads in and prepares data
    save.image("PEAD.Rdata")
} else {load("PEAD.Rdata")}


############## PEAD ##########################

### Prepares transactions table
Lookup<-merge(Lookup["R.PeriodID"],Data$globals[,c("R.PeriodID","StartTime","StartTimeCDA")])[,-1] # Generates matrix containing R.PeriodID and several variables from the globals table
Data$transactions<-merge(Data$transactions,Lookup) # Adds additional information to transactions table
Data$transactions<-cbind(Data$transactions, R.TradeTime=Data$transactions$Time-(Data$transactions$StartTimeCDA-Data$transactions$StartTime))# Adds precise timing for trades

NumSessions<-max(as.numeric(Data$transactions$R.Session))
NumMarkets<-Data$globals$NumMarkets[1]


### Plot period prices

XLIM<-c(min(Data$transactions$R.TradeTime),max(Data$transactions$R.TradeTime))
YLIM<-c(50,150)#c(min(Data$transactions$Price),max(Data$transactions$Price))
LWD<-1.5

Temp1<-0
for (Session in 1:NumSessions) {
    if(Params$ShowPlots){dev.new("PricePlot")} else {jpeg(paste("PricePlot_S",Session,"P",Period,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
    par(mfrow=c(2,2))
    
    for(Period in 1:4){
        
        # Plot if period exists and saw trade
        if(length(Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Period==Period,])>0){
            
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
                AnnouncementData[Temp1,"R.Value0"]<-80
                AnnouncementData[Temp1,"R.ValueA1"]<-Data$PEADsignals[,"PEADValue[1]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][1]*20
                AnnouncementData[Temp1,"R.ValueA2"]<-Data$PEADsignals[,"PEADValue[1]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][2]*20
                AnnouncementData[Temp1,"R.ValueA3"]<-Data$PEADsignals[,"PEADValue[1]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][3]*20
                AnnouncementData[Temp1,"R.ValueA4"]<-Data$PEADsignals[,"PEADValue[1]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][4]*20
                AnnouncementData[Temp1,"R.ValueB1"]<-Data$PEADsignals[,"PEADValue[2]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][1]*20
                AnnouncementData[Temp1,"R.ValueB2"]<-Data$PEADsignals[,"PEADValue[2]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][2]*20
                AnnouncementData[Temp1,"R.ValueB3"]<-Data$PEADsignals[,"PEADValue[2]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][3]*20
                AnnouncementData[Temp1,"R.ValueB4"]<-Data$PEADsignals[,"PEADValue[2]"][Data$PEADsignals$R.Session==Session&Data$PEADsignals$Period==Period][4]*20
                }
            }
            
            plot(x=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==1&Data$transactions$Period==Period,][,"R.TradeTime"],y=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==1&Data$transactions$Period==Period,][,"Price"], type="l", col=4, lwd=LWD, xlim=XLIM, ylim=YLIM, xlab="Time (seconds)", ylab="Price (Taler)", main=paste("Treatment ",Data$globals$TreatmentPEAD[Data$globals$R.Session==Session&Data$globals$Period==Period],", Session ",Session,", Period ",Period,sep="")) # Plots market 1
            lines(x=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==2&Data$transactions$Period==Period,][,"R.TradeTime"],y=Data$transactions[Data$transactions$R.Session==Session&Data$transactions$Market==2&Data$transactions$Period==Period,][,"Price"], type="l", col=3, lwd=LWD) # Plots market 2
            abline(v=AnnouncementData[Temp1,c("R.Time1","R.Time2","R.Time3","R.Time4")]) # Adds announcement times
            abline(v=XLIM,lwd=2) # Adds period start and end
            lines(x=c(XLIM[1],AnnouncementData[Temp1,"R.Time1"]),y=c(AnnouncementData[Temp1,"R.Value0"],AnnouncementData[Temp1,"R.Value0"])) # Fundamental value before first announcement
            lines(x=c(AnnouncementData[Temp1,"R.Time1"],AnnouncementData[Temp1,"R.Time2"]),y=c(AnnouncementData[Temp1,"R.ValueA1"],AnnouncementData[Temp1,"R.ValueA1"]),col=if(AnnouncementData[Temp1,"R.ValueA1"]==80){"black"}else{4}) # Fundamental value after first announcement, asset A
            lines(x=c(AnnouncementData[Temp1,"R.Time2"],AnnouncementData[Temp1,"R.Time3"]),y=c(AnnouncementData[Temp1,"R.ValueA2"],AnnouncementData[Temp1,"R.ValueA2"]),col=if(AnnouncementData[Temp1,"R.ValueA2"]==80){"black"}else{4}) # Fundamental value after second announcement, asset A
            lines(x=c(AnnouncementData[Temp1,"R.Time3"],AnnouncementData[Temp1,"R.Time4"]),y=c(AnnouncementData[Temp1,"R.ValueA3"],AnnouncementData[Temp1,"R.ValueA3"]),col=if(AnnouncementData[Temp1,"R.ValueA3"]==80){"black"}else{4}) # Fundamental value after third announcement, asset A
            lines(x=c(AnnouncementData[Temp1,"R.Time4"],XLIM[2]),y=c(AnnouncementData[Temp1,"R.ValueA4"],AnnouncementData[Temp1,"R.ValueA4"]),col=if(AnnouncementData[Temp1,"R.ValueA4"]==80){"black"}else{4}) # Fundamental value after fourth announcement, asset A
            lines(x=c(AnnouncementData[Temp1,"R.Time1"],AnnouncementData[Temp1,"R.Time2"]),y=c(AnnouncementData[Temp1,"R.ValueB1"],AnnouncementData[Temp1,"R.ValueB1"]),col=if(AnnouncementData[Temp1,"R.ValueB1"]==80){"black"}else{3}) # Fundamental value after first announcement, asset B
            lines(x=c(AnnouncementData[Temp1,"R.Time2"],AnnouncementData[Temp1,"R.Time3"]),y=c(AnnouncementData[Temp1,"R.ValueB2"],AnnouncementData[Temp1,"R.ValueB2"]),col=if(AnnouncementData[Temp1,"R.ValueB2"]==80){"black"}else{3}) # Fundamental value after second announcement, asset B
            lines(x=c(AnnouncementData[Temp1,"R.Time3"],AnnouncementData[Temp1,"R.Time4"]),y=c(AnnouncementData[Temp1,"R.ValueB3"],AnnouncementData[Temp1,"R.ValueB3"]),col=if(AnnouncementData[Temp1,"R.ValueB3"]==80){"black"}else{3}) # Fundamental value after third announcement, asset B
            lines(x=c(AnnouncementData[Temp1,"R.Time4"],XLIM[2]),y=c(AnnouncementData[Temp1,"R.ValueB4"],AnnouncementData[Temp1,"R.ValueB4"]),col=if(AnnouncementData[Temp1,"R.ValueB4"]==80){"black"}else{3}) # Fundamental value after fourth announcement, asset B
        }
    if(Params$ShowPlots){
        dev.copy(jpeg,paste("PricePlot_T",Data$globals$TreatmentPEAD[Data$globals$R.Session==Session&Data$globals$Period==Period],"S",Session,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
    }
    dev.off() # Turns off graphics device
    }


### Adds additional data to transactions table

Data$transactions<-merge(x=Data$transactions,y=Data$contracts[,c("R.PeriodID","Market","Period","TransactionID","BestBid","BestAsk")],by.x=c("R.PeriodID","Market","Period","AcceptanceID"),by.y=c("R.PeriodID","Market","Period","TransactionID")) # Adds best bid and ask information to transactions table
Data$transactions$BestBid[Data$transactions$BestBid>100000|Data$transactions$BestBid<1]=NA # Identifies missing best bid and ask instances
Data$transactions$BestAsk[Data$transactions$BestAsk>100000|Data$transactions$BestAsk<1]=NA # Identifies missing best bid and ask instances

### Times of period starts and announcements

Announcements<-merge(Data$PEADsignals[,c("R.PeriodID","Period","PEADSignal[1]","PEADSignal[2]","PEADValue[1]","PEADValue[2]")],AnnouncementData[,c("R.PeriodID","R.Session","R.Time1","R.Time2","R.Time3","R.Time4")],by="R.PeriodID") # Adds signal data to announcements

Announcements[(length(Announcements[,1])+1):(length(Announcements[,1])*2),]<-Announcements # Doubles matrix as to have separate rows for markets 1 and 2
Announcements[,"Market"]<-c(rep(1,length(Announcements[,1])/2),rep(2,length(Announcements[,1])/2)) # Fills in market number
Announcements[,"R.Signal"]<-ifelse(Announcements$Market==1,Announcements$"PEADSignal[1]",Announcements$"PEADSignal[2]") # Writes signal column
Announcements[,"R.Phase"]<-rep(0:4,length(Announcements[,1])/5) # Generates phase ID
Announcements[,"R.Type"]<-ifelse(Announcements$R.Phase==0,0,ifelse(Announcements$R.Signal>0,1,-1)) # Adds phase type
Announcements[,"R.PhaseStart"]<-ifelse(Announcements$R.Phase==0,0,ifelse(Announcements$R.Phase==1,Announcements$R.Time1,ifelse(Announcements$R.Phase==2,Announcements$R.Time2,ifelse(Announcements$R.Phase==3,Announcements$R.Time3,Announcements$R.Time4)))) # Writes starting time of this phase
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
AvgPrices$Base<-list(Down=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==-1&Data$transactions$TreatmentPEAD==1])),TempDesiredColumns),ncol=TempDesiredColumns),Start=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==0&Data$transactions$TreatmentPEAD==1])),TempDesiredColumns),ncol=TempDesiredColumns),Up=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==1&Data$transactions$TreatmentPEAD==1])),TempDesiredColumns),ncol=TempDesiredColumns)) # Fills in unique trade times per type. Second column is only placeholder for price data.
AvgPrices$Correl<-list(Down=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==-1&Data$transactions$TreatmentPEAD==2])),TempDesiredColumns),ncol=TempDesiredColumns),Start=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==0&Data$transactions$TreatmentPEAD==2])),TempDesiredColumns),ncol=TempDesiredColumns),Up=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==1&Data$transactions$TreatmentPEAD==2])),TempDesiredColumns),ncol=TempDesiredColumns)) # Fills in unique trade times per type. Second column is only placeholder for price data.

# Writes column R.NormPrice which contains NA in R.Phase 0 and the last price in the same unique trading session/market/period's previous R.Phase 
Data$transactions[,"R.NormPrice"]<-NA
for (Transaction in 1:length(Data$transactions[,1])){
    if (Data$transactions$R.Phase[Transaction]>0){
        MaxTimePhase0<-max(Data$transactions[Data$transactions$Market==Data$transactions$Market[Transaction]&Data$transactions$R.Session==Data$transactions$R.Session[Transaction]&Data$transactions$Period==Data$transactions$Period[Transaction]&Data$transactions$R.Phase==Data$transactions$R.Phase[Transaction]-1,][,"R.WithinPhaseTime"])
        Data$transactions$R.NormPrice[Transaction]<-Data$transactions[Data$transactions$Market==Data$transactions$Market[Transaction]&Data$transactions$R.Session==Data$transactions$R.Session[Transaction]&Data$transactions$Period==Data$transactions$Period[Transaction]&Data$transactions$R.Phase==Data$transactions$R.Phase[Transaction]-1&Data$transactions$R.WithinPhaseTime==MaxTimePhase0,][1,"Price"]
    }
    if(Transaction/500==ceiling(Transaction/500)){
        print(paste(Transaction," of ",length(Data$transactions[,1]),sep=""))
    }
}

#R02
save.image("PEAD_R02.RData")
#load("PEAD_R02.RData")


# For every type, loops through the unique trade times and, for each of those, calculates the average of all unique phases' latest trades
Temp.TotalTimes<-nrow(AvgPrices$Base$Down)+nrow(AvgPrices$Base$Start)+nrow(AvgPrices$Base$Up)+nrow(AvgPrices$Correl$Down)+nrow(AvgPrices$Correl$Start)+nrow(AvgPrices$Correl$Up) #Calculates total number of steps to be taken
Temp.StartClockTime<-Sys.time() #Saves starting system time
for (Treat in c("Base","Correl")){
    Treat.num<-ifelse(Treat=="Base",1,2)
    for (Type in -1:1) {
        for (Time in 1:length(AvgPrices[[Treat]][[Type+2]][,1])){
            Temp.Time<-Time+ifelse(Type==0,nrow(AvgPrices[[Treat]]$Down),0)+ifelse(Type==1,nrow(AvgPrices[[Treat]]$Down)+nrow(AvgPrices[[Treat]]$Start),0) #Calculates percentage of all steps performed
            Time.Elapsed<-round(difftime(Sys.time(),Temp.StartClockTime,units="mins"),1) #Calculates time elapsed
            print(paste(round((Temp.Time/Temp.TotalTimes*100),2),"% --- Time elapsed (mins): ",Time.Elapsed,", Time remaining (mins): ",round(Time.Elapsed/(Temp.Time/Temp.TotalTimes),1),sep="")) #Prints progress report
            TempSum<-0
            TempN<-0
            for (Session in 1:NumSessions){
                for (Period in 1:4){
                    for (Market in 1:2){
                        for (Phase in if(Type==0){0} else {1:4}){
                            if (nrow(Data$transactions[Data$transactions$TreatmentPEAD==Treat.num&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase&Data$transactions$R.WithinPhaseTime<=AvgPrices[[Treat]][[Type+2]][Time],])>0){ # Run if there was a trade fulfilling the criteria
                                TempN<-TempN+1
                                TempSum<-TempSum+tail(Data$transactions[Data$transactions$TreatmentPEAD==Treat.num&Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase&Data$transactions$R.WithinPhaseTime<=AvgPrices[[Treat]][[Type+2]][Time],],n=1)[,"Price"]-ifelse(Phase==0,0,Data$transactions$R.NormPrice[Data$transactions$TreatmentPEAD==Treat.num&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase][1])
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
save.image("PEAD_R01.RData")
#load("PEAD_R01.RData")

### Plots PEAD graphs


XLIM<-c(0,180)
YLIM<-c(-max(Data$transactions$Price),max(Data$transactions$Price))
YLIM<-c(-10,10)
#LWD<-2

for (Treat in 1:2){
    for (Type in -1:1) {
        Temp1<-0 # Sets temporary variable counting lines already drawn
        Temp2<-0 # Sets temporary variable counting PowerData lines already added
        if(Params$ShowPlots){dev.new("PricePlot")} else {jpeg(paste("PEADPlot_T",Type,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)} # Opens plot device
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
        
        if(Params$ShowPlots){
            dev.copy(jpeg,paste("PEADPlot_",c("Base","Correl")[Treat],"_T",Type,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
        }
        dev.off() # Turns off graphics device if even Distribution number
    }
}
