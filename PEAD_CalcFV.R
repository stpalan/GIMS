rm(list=ls()) # Clears memory

FV<-list() #Creates empty list
Earnings.Step<-0.5 #Sets change in earnings per announcement
FV.Factor<-20 #Sets factor for transforming earnings to FVs

for(Treat in c("Base","Correl")){
    
    #Sets probabilities of earnings change in same/opposite direction
    Prob.Same<-ifelse(Treat=="Correl",0.75,0.5)
    Prob.Opp<-1-Prob.Same
    
    #Prepares dataframe
    DF<-data.frame(
        ID=integer(),
        Announcement=integer(),
        Earnings=numeric(),
        FV=numeric(),
        ID.Up=integer(),
        ID.Down=integer(),
        Prob.Up=numeric(),
        Prob.Down=numeric()
    )
    
    #Writes entry for status at period start
    DF[1,]<-c(ID=1,Announcement=0,Earnings=5,FV=NA,ID.Up=NA,ID.Down=NA,Prob.Up=0.5,Prob.Down=0.5)
    
    for(Ann in 1:5){ #Loops through announcements
        for(ID in DF$ID[DF$Announcement==Ann-1]){
            DF[DF$ID==ID,c("ID.Up","ID.Down")]<-c(max(DF$ID+1),max(DF$ID+2)) #Writes IDs of up/down nodes in next announcement
            
            #Creates up node
            DF[nrow(DF)+1,]<-c(
                ID=DF$ID.Up[DF$ID==ID],
                Announcement=Ann,
                Earnings=DF$Earnings[DF$ID==ID]+Earnings.Step,
                FV=NA,
                ID.Up=NA,
                ID.Down=NA,
                Prob.Up=Prob.Same,
                Prob.Down=Prob.Opp
                )
            
            #Creates down node
            DF[nrow(DF)+1,]<-c(
                ID=DF$ID.Down[DF$ID==ID],
                Announcement=Ann,
                Earnings=DF$Earnings[DF$ID==ID]-Earnings.Step,
                FV=NA,
                ID.Up=NA,
                ID.Down=NA,
                Prob.Up=Prob.Opp,
                Prob.Down=Prob.Same
            )
        }
    }
    
    #Calculates FVs in last announcement
    DF$FV[DF$Announcement==5]<-DF$Earnings[DF$Announcement==5]*FV.Factor
    
    for(Ann in 4:0){ #Loops through announcements
        for(ID in DF$ID[DF$Announcement==Ann]){
            DF$FV[DF$ID==ID]<-DF$FV[DF$ID==DF$ID.Up[DF$ID==ID]]*DF$Prob.Up[DF$ID==ID]+DF$FV[DF$ID==DF$ID.Down[DF$ID==ID]]*DF$Prob.Down[DF$ID==ID] #Calculates and saves FV
        }
    }
    FV[[Treat]]<-DF #Adds dataframe to FV list
}
    
rm(list=setdiff(ls(), "FV"))