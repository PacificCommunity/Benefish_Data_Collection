##
##    Programme:  Clean_Marshall_Islands.R
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
   ##    Collect up and process the Marshall_Islands files
   ##
      Marshall_Islands <- Country_Data[["Marshall_Islands"]]
      
      Clean_Marshall_Islands <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table9-4
   ##
      X <- Marshall_Islands[["Estimates by the Benefish studies of annual fisheries harvestsXXTable10-5"]]
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
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 25000
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- 10000
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 22000
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (A$)", "A$", 
                  ifelse(X$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
      Clean_Marshall_Islands[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to Marshall_Islands GDP in 2021 - Table20-5
   ##
      X <- Marshall_Islands[["Fishing contribution to GDP in 2021 using an alternative approachXXTable10-7"]]
      X_Name <- names(Marshall_Islands[6])
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "US$")
      Clean_Marshall_Islands[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      
   ##
   ##    Fishing contribution to Marshall_Islands GDP in 2021 - Table7-7
   ##
      X <- Marshall_Islands[["Fishing contribution to GDP (US$ millions)XXTable10-6"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[str_detect(X$Harvest_Sector, "Fishing contribution"),]
      X$Year    <- as.numeric(str_replace_all(X$variable, "\\D+", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "US$"
      Clean_Marshall_Islands[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
 
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Marshall_Islands, file = 'Data_Intermediate/Clean_Marshall_Islands.rda')
##
##    And we're done
##
