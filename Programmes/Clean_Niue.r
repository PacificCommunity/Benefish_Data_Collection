##
##    Programme:  Clean_Niue.R
##
##    Objective:  What is this programme designed to do?
##
##    Author:     James Hogan, FAME - SPC, 10 September 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Collect up the downloaded files
   ##
      load('Data_Intermediate/Country_Data.rda')
      
   ##
   ##    Collect up and process the Niue files
   ##
      Niue <- Country_Data[["Niue"]]
      
      Clean_Niue <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Niue[["Estimates by the Benefish studies of annual fisheries harvestsXXTable12-2"]]
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      X$V1[1] <- "Harvest sector"
      X$V2[1] <- "Year"
      names(X) <- X[1,]
      ##
      ##    Aquaculture tonnes or pcs... Choose...
      ##       Pieces
      ##
         #X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 16000
         #X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- 37400
         #X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 65000
         
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (NZ$)", "NZ$", 
                  ifelse(X$Harvest_Sector == "AquacultureX","Pieces", "Tonnes"))
      Clean_Niue[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to Niue GDP in 2021 - Table20-5
   ##
      X <- Niue[["Fishing contribution to GDP 2021 using an alternative approachXXTable12-4"]]
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Year    <- 2021
      X$Measure <- "Fishing contribution to GDP - VAR Method"
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "NZ$")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Niue[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]


   ##
   ##    Fisheries Revenue
   ##
      X <- Niue[["Fisheries access revenue XXTable12-5"]]
      X$V1[1] <- "Year"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Value <- as.numeric(str_replace_all(X$Value, "\\%", ""))
      X <- X[!is.na(X$Value),]
      X$Year  <- ifelse(X$Year == "2017/18", 2018, 
                 ifelse(X$Year == "2018/19", 2019, 
                 ifelse(X$Year == "2019/20", 2020, 
                 ifelse(X$Year == "2020/21", 2021,2022))))
      X$Revenue_Source <- str_trim(X$variable)
      X$Measure <- "Fisheries Revenue"
      X$Unit    <- "NZ$"      
      Clean_Niue[["Fisheries Revenue"]] <- X[,c("Measure","Table", "Revenue_Source", "Year", "Unit", "Value")]



   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Niue, file = 'Data_Intermediate/Clean_Niue.rda')
##
##    And we're done
##
