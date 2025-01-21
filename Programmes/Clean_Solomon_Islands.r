##
##    Programme:  Clean_Solomon_Islands.R
##
##    Objective:  What is this programme designed to do?
##
##    Author:     James Hogan, FAME - SPC, 10 September 2024
##
##
   ##
   ##    Clear the memorZ
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Collect up the downloaded files
   ##
      load('Data_Intermediate/Country_Data.rda')
      
   ##
   ##    Collect up and process the Solomon_Islands files
   ##
      Solomon_Islands <- Country_Data[["Solomon_Islands"]]
      
      Clean_Solomon_Islands <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Solomon_Islands[["Estimates by the Benefish studies of annual fisheries harvestsXXTable16-6"]]
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      X$V1[1] <- "Harvest sector"
      X$V2[1] <- "Year"
      names(X) <- X[1,]
      ##
      ##    Aquaculture - split between tonnes AND pieces. Keep the value the same for each and look at allocating at the end
      ##
         Aquaculture <- X[(X$`Harvest sector` == "Aquaculture"),]
         Aquaculture <- reshape2::melt(Aquaculture,
                                       id.var = c("Harvest sector", "Year", "Nominal value ", "Measure", "Table"))   
         Aquaculture$Tonnes <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,1],
                               ifelse(str_detect(Aquaculture$value, "t"), Aquaculture$value, ""))
                               
         Aquaculture$Pieces <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,2],
                               ifelse(str_detect(Aquaculture$value, "pcs"), Aquaculture$value, ""))
         Aquaculture$Tonnes <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Tonnes, "t", ""), ",",""))
         Aquaculture$Pieces <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Pieces, "pcs", ""), ",",""))
         
         Aquaculture <- reshape2::melt(Aquaculture,
                                       id.var = c("Harvest sector", "Year", "Measure", "Table"))   
         Aquaculture <- Aquaculture[!(Aquaculture$variable %in% c("value","variable")),]
         
          
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X <- rbind(X[(X$`Harvest sector` != "Aquaculture"),], 
                 Aquaculture)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$variable <- as.character(X$variable)
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value ", "SI$", 
                ifelse(X$Harvest_Sector == "Aquaculture",as.character(X$variable), "Tonnes"))
      Clean_Solomon_Islands[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Solomon_Islands GDP in 2021 - Table20-5
   ##
      X <- Solomon_Islands[["Fishing contribution to GDP in 2021 using an alternative approachXXTable16-9"]]
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "SI$")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Solomon_Islands[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
  

   ##
   ##    Fishing contribution to Solomon_Islands GDP in 2021 - Table7-7
   ##
      X <- Solomon_Islands[["Fishing contribution to GDP in the Solomon IslandsXXTable16-7"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[str_detect(X$Harvest_Sector, "Fishing contribution"),]
      X$Year    <- as.numeric(str_replace_all(X$variable, "\\D+", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "SI$"
      Clean_Solomon_Islands[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      
   ##
   ##    Fish Consumption
   ##
      X <- Solomon_Islands[["Mean daily per capita consumption of aquatic foods in the Solomon IslandsXXTable16-16"]]
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
      X$Measure <- "Domestic Fish Consumption"
      X$Unit  = "Mean daily grams per capita"
      Clean_Solomon_Islands[["Domestic Fish Consumption"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Metric", "Year", "Unit", "Value")]
      
      
   ##
   ##    Fish Exports
   ##
      ##
      ##    Exports 
      ##
         X <- Solomon_Islands[["HS 03 Exports of the Solomon IslandsXXTable16-11"]]
         names(X) <- X[1,]
         names(X)[1] <- "Year"
    
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000
         X <- X[!is.na(X$Value),]
         X$Dimension       <- "Aggregate Commodity"
         X$Dimension_Value <-X$variable
         X$Measure <- "Fish Exports"
         X$Unit    <- "ST$"
      ##
      ##    I don't know... the other Exports?
      ##
         Y <- Solomon_Islands[["Fish exportsXXTable16-12"]]
         names(Y) <- Y[1,]
         names(Y)[1] <- "Year"
    
         Y <- reshape2::melt(Y[3:nrow(Y),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))*1000000
         Y <- Y[!is.na(Y$Value),]
         
         Y$Dimension       <- "Aggregate Commodity"
         Y$Dimension_Value <- str_trim(Y$variable)
         Y$Unit            <- str_split_fixed(str_trim(Y$variable), "XX",2)[,2]
         Y$Unit    <- "ST$"
         Y$Measure <- "Fish Exports"
         
      ##
      ##    Whatever this is
      ##
         Z <- Solomon_Islands[["Summary of the MFMR Fishery Export DatabaseXXTable16-13"]]
         names(Z) <- Z[1,]
         names(Z)[1] <- "Year"
    
         Z <- reshape2::melt(Z[3:nrow(Z),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         Z$Value <- as.numeric(str_replace_all(Z$value, ",", ""))*1000000
         Z <- Z[!is.na(Z$Value),]
         
         Z$Dimension       <- "Aggregate Commodity"
         Z$Dimension_Value <- str_trim(Z$variable)
         Z$Unit            <- str_split_fixed(str_trim(Z$variable), "XX",2)[,2]
         Z$Unit <- ifelse(str_detect(Z$variable, "Pieces"), "Pieces", 
                   ifelse(str_detect(Z$variable, "Volume"), "KGs","SI$"))
         Z$Measure <- "Fish Exports"
         
         A <- rbind.fill(X,Y)
         A <- rbind.fill(A,Z)
         #X <- X[!str_detect(X$Dimension_Value, "Total"),]

      Clean_Solomon_Islands[["Fish Exports"]] <- A[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]
        
        

   ##
   ##    Fisheries Revenue
   ##
      X <- Solomon_Islands[["Access fees received for offshore fishing in 2020XXTable16-14"]]
      X$V1[1] <- "Revenue_Source"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Revenue_Source"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Revenue_Source <- str_trim(X$Revenue_Source)
      X$Measure <- "Fisheries Revenue"
      X$Unit    <- "SI$"      
      X$Year    <- 2020
      
      
      Y <- Solomon_Islands[["Other government revenue from fisheries in 2020XXTable16-15"]]
      Y$V1[1] <- "Revenue_Source"
      names(Y) <- Y[1,]
 
      Y <- reshape2::melt(Y[2:nrow(Y),],
                          id.var = c("Measure","Table", "Revenue_Source"),
                          factorsAsStrings = FALSE)
      Y$Value <- as.numeric(str_replace_all(Y$value, "\\D+", ""))
      Y <- Y[!is.na(Y$Value),]
      Y$Year  <- str_trim(Y$variable)
      Y$Measure <- "Fisheries Revenue"
      Y$Unit    <- "SI$"
      X$Year    <- 2020
      
      X <- rbind(X, Y)
      X <- X[!str_detect(X$Revenue_Source, "Total"),]
      
      Clean_Solomon_Islands[["Fisheries Revenue"]] <- X[,c("Measure","Table", "Revenue_Source", "Year", "Unit", "Value")]

        
  
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Solomon_Islands, file = 'Data_Intermediate/Clean_Solomon_Islands.rda')
##
##    And we're done
##
