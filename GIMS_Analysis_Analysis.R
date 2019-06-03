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

Params$ShowPlots<-T #Should plots be shown on screen or only written to disk?
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
AvgPrices<-list(Down=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==-1])),TempDesiredColumns),ncol=TempDesiredColumns),Start=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==0])),TempDesiredColumns),ncol=TempDesiredColumns),Up=matrix(rep(sort(unique(Data$transactions$R.WithinPhaseTime[Data$transactions$R.Type==1])),TempDesiredColumns),ncol=TempDesiredColumns)) # Fills in unique trade times per type. Second column is only placeholder for price data.

# Writes column R.NormPrice which contains NA in R.Phase 0 and the last price in the same unique trading session/market/period's previous R.Phase 
Data$transactions[,"R.NormPrice"]<-NA
for (Transaction in 1:length(Data$transactions[,1])){
    if (Data$transactions$R.Phase[Transaction]>0){
        MaxTimePhase0<-max(Data$transactions[Data$transactions$Market==Data$transactions$Market[Transaction]&Data$transactions$R.Session==Data$transactions$R.Session[Transaction]&Data$transactions$Period==Data$transactions$Period[Transaction]&Data$transactions$R.Phase==Data$transactions$R.Phase[Transaction]-1,][,"R.WithinPhaseTime"])
        Data$transactions$R.NormPrice[Transaction]<-Data$transactions[Data$transactions$Market==Data$transactions$Market[Transaction]&Data$transactions$R.Session==Data$transactions$R.Session[Transaction]&Data$transactions$Period==Data$transactions$Period[Transaction]&Data$transactions$R.Phase==Data$transactions$R.Phase[Transaction]-1&Data$transactions$R.WithinPhaseTime==MaxTimePhase0,][,"Price"]
    }
}


# For every type, loops through the unique trade times and, for each of those, calculates the average of all unique phases' latest trades
Temp.TotalTimes<-nrow(AvgPrices$Down)+nrow(AvgPrices$Start)+nrow(AvgPrices$Up)
for (Type in -1:1) {
    #Temp1<-matrix(c(AvgPrices[Type+2],rep(NA,length(AvgPrices[Type+2]))),ncol=2) # Prepares matrix to hold results
    for (Time in 1:length(AvgPrices[[Type+2]][,1])){
        Temp.Time<-Time+ifelse(Type==0,nrow(AvgPrices$Down),0)+ifelse(Type==1,nrow(AvgPrices$Down)+nrow(AvgPrices$Start),0)
        print(paste(round((Temp.Time/Temp.TotalTimes*100),2),"% --- Type ",Type,", Time ",Time,sep=""))
        TempSum<-0
        TempN<-0
        for (Session in 1:NumSessions){
            for (Period in 1:4){
                for (Market in 1:2){
                    for (Phase in if(Type==0){0} else {1:4}){
                        if (nrow(Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase&Data$transactions$R.WithinPhaseTime<=AvgPrices[[Type+2]][Time],])>0){ # Run if there was a trade fulfilling the criteria
                            TempN<-TempN+1
                            TempSum<-TempSum+tail(Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase&Data$transactions$R.WithinPhaseTime<=AvgPrices[[Type+2]][Time],],n=1)[,"Price"]-Data$transactions$R.NormPrice[Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase][1]
                        }
                    }
                }
            }
        }       
        AvgPrices[[Type+2]][Time,2]<-TempSum/TempN
        AvgPrices[[Type+2]][Time,3]<-TempN
    }
    colnames(AvgPrices[[Type+2]])<-c("R.WithinPhaseTime","R.AvgPrice","R.N") # Writes column names
}

save.image("PEAD_AfterLine180.RData")

### Plots PEAD graphs


XLIM<-c(0,180)
YLIM<-c(-max(Data$transactions$Price),max(Data$transactions$Price))
YLIM<-c(-10,10)
#LWD<-2

PowerData<-list(Down=list(),Up=list()) # Prepares variable to hold raw data for power analysis

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
                    if(nrow(Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,])>0){
                        if(Temp1==0){
                            Temp1<-Temp1+1 # Increases count of lines already drawn
                            PriceNormalization<-
                                if(Phase==0){Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][1,"Price"]}
                                else {Data$transactions[Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,][length(Data$transactions[Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,1]),"Price"]} # Sets base price equal to first price in time series in phase 0, and equal to last price in time series in phase > 0
                            plot(x=Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"R.WithinPhaseTime"],y=Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization, type="l", col="gray", lwd=LWD, xlim=XLIM, ylim=YLIM, xlab="Time (seconds)", ylab="Price (Taler)", main=paste("Type ",Type,sep="")) # Plots market 1
                            
# Assembles data for power analysis separately for negative and positive earnings announcements                         
                            if (Type==-1){
                                Temp2<-Temp2+1
                                PowerData$Down[[Temp2]]<-as.data.frame((Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization)/PriceNormalization,ncol=1) # Adds price
                                PowerData$Down[[Temp2]][,2]<-Period # Adds period
                                PowerData$Down[[Temp2]][,3]<-Phase # Adds phase
                                PowerData$Down[[Temp2]][,4]<-Market # Adds market
                                PowerData$Down[[Temp2]][,5]<-Session # Adds session
                            }
                            if (Type==1){
                                Temp2<-Temp2+1
                                PowerData$Up[[Temp2]]<-as.data.frame((Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization)/PriceNormalization,ncol=1) # Adds price
                                PowerData$Up[[Temp2]][,2]<-Period # Adds period
                                PowerData$Up[[Temp2]][,3]<-Phase # Adds phase
                                PowerData$Up[[Temp2]][,4]<-Market # Adds market
                                PowerData$Up[[Temp2]][,5]<-Session # Adds session
                                
                            }
                        } else {
                            Temp1<-Temp1+1 # Increases count of lines already drawn
                            PriceNormalization<-
                                if(Phase==0){Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][1,"Price"]}
                            else {Data$transactions[Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,][length(Data$transactions[Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase-1,1]),"Price"]} # Sets base price equal to first price in time series in phase 0, and equal to last price in time series in phase > 0
                            lines(x=Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"R.WithinPhaseTime"],y=Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization, type="l", col="gray", lwd=LWD) # Plots market 2+
                            
# Assembles data for power analysis separately for negative and positive earnings announcements
                            if (Type==-1){
                                Temp2<-Temp2+1
                                PowerData$Down[[Temp2]]<-as.data.frame((Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization)/PriceNormalization,ncol=1) # Adds price
                                PowerData$Down[[Temp2]][,2]<-Period # Adds period
                                PowerData$Down[[Temp2]][,3]<-Phase # Addds phase
                                PowerData$Down[[Temp2]][,4]<-Market # Adds market
                                PowerData$Down[[Temp2]][,5]<-Session # Adds session
                            }
                            if (Type==1){
                                Temp2<-Temp2+1
                                PowerData$Up[[Temp2]]<-as.data.frame((Data$transactions[Data$transactions$R.Type==Type&Data$transactions$Market==Market&Data$transactions$R.Session==Session&Data$transactions$Period==Period&Data$transactions$R.Phase==Phase,][,"Price"]-PriceNormalization)/PriceNormalization,ncol=1) # Adds price
                                PowerData$Up[[Temp2]][,2]<-Period # Adds period
                                PowerData$Up[[Temp2]][,3]<-Phase # Adds phase
                                PowerData$Up[[Temp2]][,4]<-Market # Adds market
                                PowerData$Up[[Temp2]][,5]<-Session # Adds session
                            }
                        }
                    }
                }
            }
        }
    }
    
    # Plots means
    lines(x=AvgPrices[[Type+2]][,1],y=AvgPrices[[Type+2]][,2]-AvgPrices[[Type+2]][1,2], type="l", col="black", lwd=LWD+2) # Plots mean prices
    
    if(Params$ShowPlots){
        dev.copy(jpeg,paste("PEADPlot_T",Type,".jpeg",sep=""), bg="white", width=2000, height=2000, res=300)
    }
    dev.off() # Turns off graphics device if even Distribution number
}

PowerDataSaved<-PowerData # Just for testing


### Plots bubble chart of asset holdings
# 
# graphics.off()
# 
# require("plotly")
# #library("plotly")
# Sys.setenv("plotly_username"="spalan")
# Sys.setenv("plotly_api_key"="RCBBKmVpdPcTe31aiMO3")
# 
# # Extracts asset holding data
# AssetData<-Data$subjects[(Data$subjects$Period==1|Data$subjects$Period==2|Data$subjects$Period==3|Data$subjects$Period==4)&Data$subjects$IsExperimenter==0,c("Assets[1]","Assets[2]")]
# colnames(AssetData)<-c("AssetA","AssetB")
# AssetData[,"Name"]<-paste(AssetData$AssetA,rep("x",length(AssetData$AssetA)),AssetData$AssetB)
# 
# # Adds count of unique asset holding combinations
# AssetData[,"Count"]<-NA
# for (i in 1:nrow(AssetData)){
#     AssetData$Count[i]<-length(AssetData[AssetData$AssetA==AssetData$AssetA[i]&AssetData$AssetB==AssetData$AssetB[i],1])
# }
# AssetDataUnique<-unique(AssetData,ordered=FALSE) # Generates matrix with unique rows
# 
# # Plots
# AssetHoldings<-plot_ly(AssetDataUnique, x = ~AssetA, y = ~AssetB, type = 'scatter', mode = 'markers',marker = list(size = ~Count*10, opacity = 0.5),showlegend=F) %>%
#     add_trace(AssetHoldings,x=c(min(AssetData[,c("AssetA","AssetB")]),max(AssetData[,c("AssetA","AssetB")])),y=c(min(AssetData[,c("AssetA","AssetB")]),max(AssetData[,c("AssetA","AssetB")])),type = 'scatter', mode = 'lines',line = list(color = '#45171D')) # Adds 45? line
# AssetHoldings
# 
# plotly_IMAGE(AssetHoldings, width = 500, height = 500, format = "png", scale = 2,out_file = "D:/Institut/#CurrentWork/PEAD/Results/BubblePlot.png")
# 
# # Saves plot to disk
# if (!require("webshot")) install.packages("webshot")
# export(plot_ly(AssetHoldings), file = "D:/Institut/#CurrentWork/PEAD/Results/BubblePlot.png")
# browseURL(tmpFile)


# PowerAnalysis

# Data preparation
PowerData<-PowerDataSaved # Just for testing
LastPrice<-matrix(NA,ncol=7,nrow=length(PowerData$Up)*2,dimnames=list(1:(length(PowerData$Up)*2),c("Type","Session","Period","Market","Phase","LastPrice","Observation")))    # Prepares variable
for (Type in 1:2) {
    for (TS in 1:length(PowerData[[Type]])){
        PowerData[[Type]][[TS]][,6]<-1:nrow(PowerData[[Type]][[TS]]) # Adds Observation number to time series
        dimnames(PowerData[[Type]][[TS]])<-list(1:nrow(PowerData[[Type]][[TS]]),c("Price","Period","Phase","Market","Session","Observation")) # Names dimensions
        #MeanShift<-mean(PowerData[[Type]][,1])-1
        PowerData[[Type]][[TS]][,"DetrendedPrice"]<-PowerData[[Type]][[TS]][,"Price"]
        #Trend<-lm(Price~Observation,data=PowerData[[Type]][[TS]])$coefficients[2] # Calculates time trend in prices
        #PowerData[[Type]][[TS]][,"DetrendedPrice"]<-PowerData[[Type]][[TS]]$Price+(nrow(PowerData[[Type]][[TS]])/2-PowerData[[Type]][[TS]]$Observation)*Trend    # Calculates and adds Detrended Price to PowerData
        LastPrice[TS+length(PowerData$Up)*(Type-1),"Session"]<-PowerData[[Type]][[TS]][nrow(PowerData[[Type]][[TS]]),"Session"]
        LastPrice[TS+length(PowerData$Up)*(Type-1),"Period"]<-PowerData[[Type]][[TS]][nrow(PowerData[[Type]][[TS]]),"Period"]
        LastPrice[TS+length(PowerData$Up)*(Type-1),"Market"]<-PowerData[[Type]][[TS]][nrow(PowerData[[Type]][[TS]]),"Market"]
        LastPrice[TS+length(PowerData$Up)*(Type-1),"Phase"]<-PowerData[[Type]][[TS]][nrow(PowerData[[Type]][[TS]]),"Phase"]
        LastPrice[TS+length(PowerData$Up)*(Type-1),"Observation"]<-PowerData[[Type]][[TS]][nrow(PowerData[[Type]][[TS]]),"Observation"]
        LastPrice[TS+length(PowerData$Up)*(Type-1),"Type"]<-Type*2-3
        LastPrice[TS+length(PowerData$Up)*(Type-1),"LastPrice"]<-PowerData[[Type]][[TS]][nrow(PowerData[[Type]][[TS]]),"Price"]
    }
}

LastPrice<-LastPrice[order(LastPrice[,"Type"], LastPrice[,"Session"], LastPrice[,"Period"], LastPrice[,"Phase"]),]    # Sorts LastPrice

# Calculates LastPriceDiff as difference in last prices between markets with positive and negative earnings news
Temp2<-c("PriceDiff","Session","Period","Phase")
LastPriceDiff<-matrix(NA,ncol=length(Temp2),nrow=NumSessions*16,dimnames=list(1:(NumSessions*16),Temp2))    # Prepares variable
for (Session in 1:NumSessions){
    for(Period in 1:4){
        for(Phase in 1:4){
            #print(paste("Session",Session,"Period",Period,"Phase",Phase))
            LastPriceDiff[(Session-1)*16+(Period-1)*4+Phase,"PriceDiff"]<-LastPrice[LastPrice[,"Session"]==Session&LastPrice[,"Period"]==Period&LastPrice[,"Phase"]==Phase&LastPrice[,"Type"]==1,"LastPrice"]-LastPrice[LastPrice[,"Session"]==Session&LastPrice[,"Period"]==Period&LastPrice[,"Phase"]==Phase&LastPrice[,"Type"]==-1,"LastPrice"]
            LastPriceDiff[(Session-1)*16+(Period-1)*4+Phase,"Session"]<-Session
            LastPriceDiff[(Session-1)*16+(Period-1)*4+Phase,"Period"]<-Period
            LastPriceDiff[(Session-1)*16+(Period-1)*4+Phase,"Phase"]<-Phase
        }
    }
}

# Subtracts mean LastPrice from LastPrice to generate LastPriceZeroMean
LastPriceZeroMean<-LastPrice    # Prepares variable
for (Type in c(-1,1)){
    LastPriceZeroMean[LastPriceZeroMean[,"Type"]==Type,"LastPrice"]<-LastPrice[LastPrice[,"Type"]==Type,"LastPrice"]-mean(LastPrice[LastPrice[,"Type"]==Type,"LastPrice"])    # Demeans markets with positive and negative earnings news separately
}

# Subtracts mean PriceDiff from LastPriceDiff to generate LastPriceDiffDemeaned
LastPriceDiffDemeaned<-LastPriceDiff    # Prepares variable
LastPriceDiffDemeaned[,"PriceDiff"]<-LastPriceDiff[,"PriceDiff"]-mean(LastPriceDiff[,"PriceDiff"])    # Demeans


# Simulation
#install.packages("pwr")
require("pwr")

set.seed(1)
EffectSize<-0.04 # Assumed effect size (difference between ending price of markets with positive and negative announcements)
SampleSize<-20 # Number of sessions
Phases<-4 # Number of announcements after which prices are observed per period
Periods<-4 # Number of periods per session
Repetitions<-1000 # Number of simulation runs
Alpha<-0.05

SimResults<-0    # Prepares variable for test results
PriceDiffs<-matrix(NA,ncol=Repetitions,nrow=SampleSize*Periods*Phases)    # Prepares variable for price difference results to be tested
for (Repetition in 1:Repetitions){
    for (Sample in 1:SampleSize){
        for (Period in 1:Periods){
            for (Phase in 1:Phases){
                PriceDiffs[(Sample-1)*16+(Period-1)*4+Phase,Repetition]<-LastPriceDiffDemeaned[LastPriceDiffDemeaned[,"Period"]==Period&LastPriceDiffDemeaned[,"Phase"]==Phase,"PriceDiff"][ceiling(runif(1,min=0,max=length(LastPriceDiffDemeaned[LastPriceDiffDemeaned[,"Period"]==Period&LastPriceDiffDemeaned[,"Phase"]==Phase,"PriceDiff"])))]+EffectSize    # Writes price differences between market with positive and negative news
            }
        }
    }
    SimResults[Repetition]<-t.test(PriceDiffs[,Repetition])[["p.value"]]    # Performs test and adds it to SimResults
}

Power<-print(length(SimResults[SimResults<Alpha])/Repetitions)    # Checks how many results are significant
pwr.t.test(d=0.04/sd(PriceDiffs),sig.level=0.05,power=0.8)
