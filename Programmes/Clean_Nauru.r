##
##    Programme:  Clean_Nauru.R
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
   ##    Collect up and process the Nauru files
   ##
      Nauru <- Country_Data[["Nauru"]]
      
      Clean_Nauru <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table9-4
   ##
      X <- Nauru[["Estimates by the Benefish studies of annual fisheries harvestsXXTable11-6"]]
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
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 8
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- .1
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 0
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (A$)", "A$", 
                  ifelse(X$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
      Clean_Nauru[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to Nauru GDP in 2021 - Table20-5
   ##
      X <- Nauru[["Fishing contribution to GDP in 2021 using an alternative approachXXTable11-8"]]
      X_Name <- names(Nauru[6])
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "A$")
      Clean_Nauru[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Nauru, file = 'Data_Intermediate/Clean_Nauru.rda')
##
##    And we're done
##
