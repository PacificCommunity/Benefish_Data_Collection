##
##    Programme:  Clean_Wallis_and_Futuna.R
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
   ##    Collect up and process the Wallis_and_Futuna files
   ##
      Wallis_and_Futuna <- Country_Data[["Wallis_and_Futuna"]]
      
      Clean_Wallis_and_Futuna <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Wallis_and_Futuna[["Estimates by the Benefish studies of annual fisheries harvestsXXTable27-5"]]
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
         #X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 0
         #X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- 0
         #X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 0
         
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$variable <- as.character(X$variable)
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (XPF)", "XPF", 
                  ifelse(X$Harvest_Sector == "AquacultureX","Pieces", "Tonnes"))
      Clean_Wallis_and_Futuna[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Wallis_and_Futuna GDP in 2021 - Table20-5
   ##
      X <- Wallis_and_Futuna[["Fishing contribution to the Wallis and Futuna GDP in 2021XXTable27-6"]]
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "XPF")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Wallis_and_Futuna[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
   
   ##
   ##    Fish Consumption
   ##
      X <- Wallis_and_Futuna[["Frequency of consuming fishery products in 2020XXTable27-7"]]
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Product"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\%", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Type of fish consumed"
      X$Dimension_Value <- str_trim(X$`Product`)
      X <- X[!str_detect(X$Dimension_Value, "total"),]
      #X$Metric  <- X$variable
      X$Metric  <- "MISSING"
      X$Year    <- 2021
      X$Measure <- "Domestic Fish Consumption"
      X$Unit  = "Percentage of households that reported consuming"
      Clean_Wallis_and_Futuna[["Domestic Fish Consumption"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Metric", "Year", "Unit", "Value")]
            
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Wallis_and_Futuna, file = 'Data_Intermediate/Clean_Wallis_and_Futuna.rda')
##
##    And we're done
##
