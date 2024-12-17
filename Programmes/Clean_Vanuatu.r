##
##    Programme:  Clean_Vanuatu.R
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
   ##    Collect up and process the Vanuatu files
   ##
      Vanuatu <- Country_Data[["Vanuatu"]]
      
      Clean_Vanuatu <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Vanuatu[["Estimates by the Benefish studies of annual fisheries harvestsXXTable19-2"]]
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
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 2500
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- 27300
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 4000
         
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$variable <- as.character(X$variable)
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (VT)", "VT", 
                  ifelse(X$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
      Clean_Vanuatu[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Vanuatu GDP in 2021 - Table20-5
   ##
      X <- Vanuatu[["Fishing contribution to GDP in 2021 using an alternative approachXXTable19-4"]]
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "VT")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Vanuatu[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Vanuatu GDP in 2021 - Table7-7
   ##
      X <- Vanuatu[["Fishing contribution to the Vanuatu GDP (current prices)XXTable19-3"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[str_detect(X$Harvest_Sector, "Fishing contribution \\(VT millions\\)"),]
      X$Year    <- as.numeric(str_replace_all(X$variable, "\\D+", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "VT"
      Clean_Vanuatu[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

      
   ##
   ##    Fish Consumption
   ##
      X <- Vanuatu[["Mean daily per capita consumption of aquatic foods in VanuatuXXTable19-7"]]
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Aquatic food group"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(str_split_fixed(X$value, " ",2)[,1], ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Type of fish consumed"
      X$Dimension_Value <- str_trim(X$`Aquatic food group`)
      X <- X[!str_detect(X$Dimension_Value, "total"),]
      X$Metric  <- X$variable
      X$Year    <- 2021
      X$Measure <- "Fishing Consumption"
      X$Unit  = "Mean daily grams per capita"
      Clean_Vanuatu[["Domestic Fish Consumption"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Metric", "Year", "Unit", "Value")]
      
      
   ##
   ##    Fish Exports
   ##
      X <- Vanuatu[["Fish exportsXXTable19-5"]]
      names(X) <- X[1,]
      names(X)[1] <- "Year"
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Aggregate Commodity"
      X$Dimension_Value <- str_trim(X$variable)
      X$Measure <- "Fish Exports"
      X$Unit    <- "VT$"
      Clean_Vanuatu[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]


   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Vanuatu, file = 'Data_Intermediate/Clean_Vanuatu.rda')
##
##    And we're done
##
