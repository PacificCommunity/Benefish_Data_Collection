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
      ##    Aquaculture - split between tonnes AND pieces. Keep the value the same for each and look at allocating at the end
      ##
         Aquaculture <- X[(X$`Harvest sector` == "Aquaculture"),]
         Aquaculture <- reshape2::melt(Aquaculture,
                                       id.var = c("Harvest sector", "Year", "Nominal value (US$)", "Measure", "Table"))   
         Aquaculture$Tonnes <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,2],
                               ifelse(str_detect(Aquaculture$value, "t"), Aquaculture$value, ""))
                               
         Aquaculture$Pieces <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,1],
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
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (US$)", "US$", 
                ifelse(X$Harvest_Sector == "Aquaculture",as.character(X$variable), "Tonnes"))
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
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Marshall_Islands[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      
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
   ##    Fish Exports
   ##
      ##
      ##    Aquarium Fish
      ##
         X <- Marshall_Islands[["Marshall Islands exports of aquarium productsXXTable10-9"]]
         names(X) <- c("Year", "Live aquarium fish", "Biff", "Invertebrates", "Biff", "Giant clams", "Biff", "Corals", "Measure", "Table")
         X <- X[,names(X) != "Biff"]
    
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
         X <- X[!is.na(X$Value),]
         X$Dimension       <- "Aggregate Commodity"
         X$Dimension_Value <- str_trim(X$variable)
         X$Measure <- "Fish Exports"
         X$Unit    <- "Pieces"
      ##
      ##    Non - Pearls
      ##
         Y <- Marshall_Islands[["Fish exportsXXTable10-8"]]
         names(Y) <- Y[1,]
         names(Y)[1] <- "Aggregate Commodity"
    
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Aggregate Commodity"),
                             factorsAsStrings = FALSE)
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))*1000000
         Y <- Y[!is.na(Y$Value),]
         Y$Dimension       <- "Aggregate Commodity"
         Y$Dimension_Value <- str_trim(Y$`Aggregate Commodity`)
         Y$Year    <-  as.numeric(str_replace_all(str_trim(Y$variable), "\\D+", ""))
         Y$Measure <- "Fish Exports"
         Y$Unit    <- "US$"
         
         X <- rbind.fill(X,Y)
         X <- X[!str_detect(X$Dimension_Value, "Total"),]

      Clean_Marshall_Islands[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]
 

   ##
   ##    Fisheries Revenue
   ##
      X <- Marshall_Islands[["Access fees received by MIMRA (US$)XXTable10-10"]]
      X$V1[1] <- "Revenue_Source"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Revenue_Source"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X <- X[!is.na(X$Value),]
      X$Year  <- str_trim(X$variable)
      X$Measure <- "Fisheries Revenue"
      X$Unit    <- "US$"
      
      
      Y <- Marshall_Islands[["Non-access revenue received by MIMRA (US$)XXTable10-11"]]
      Y$V1[1] <- "Revenue_Source"
      names(Y) <- Y[1,]
 
      Y <- reshape2::melt(Y[2:nrow(Y),],
                          id.var = c("Measure","Table", "Revenue_Source"),
                          factorsAsStrings = FALSE)
      Y$Value <- as.numeric(str_replace_all(Y$value, "\\D+", ""))
      Y <- Y[!is.na(Y$Value),]
      Y$Year  <- str_trim(Y$variable)
      Y$Measure <- "Fisheries Revenue"
      Y$Unit    <- "US$"
      
      X <- rbind(X, Y)
      X <- X[!str_detect(X$Revenue_Source, "Total"),]
      
      Clean_Marshall_Islands[["Fisheries Revenue"]] <- X[,c("Measure","Table", "Revenue_Source", "Year", "Unit", "Value")]


 
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Marshall_Islands, file = 'Data_Intermediate/Clean_Marshall_Islands.rda')
##
##    And we're done
##
