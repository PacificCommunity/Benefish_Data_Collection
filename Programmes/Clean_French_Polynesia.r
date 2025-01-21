##
##    Programme:  Clean_French_Polynesia.R
##
##    Objective:  What is this programme designed to do?
##
##    Author:     James Hogan, FAME - SPC, 10 December 2024
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
   ##    Collect up and process the French_Polynesia files
   ##
      French_Polynesia <- Country_Data[["French_Polynesia"]]
      
      Clean_French_Polynesia <- list()
      
   ##
   ##    Catches of the major fisheries in French_Polynesia - Table21-4 & Table21-1
   ##
      ##
      ##    Table 21-4
      ##
         X <- French_Polynesia[["Catches of the locally based longliners 2017\x962021 (t)XXTable21-4"]]
         X$V1[1] <- "Species"
         names(X) <- X[1,]
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Species"),
                             factorsAsStrings = FALSE)
         X$Year <- as.numeric(as.character(X$variable))
         X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
         X <- X[!is.na(X$Value),]
         X <- X[!is.na(X$Year),]
         X <- X[!str_detect(X$Species, regex("total", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- "Tonnes"
         X$Species <- str_trim(X$Species, side = "both")
         X$Method  <- 'Long Liners'
      ##
      ##    Table 21-1
      ##
         Y <- French_Polynesia[["2021 Catch and value of bonitier and poti marara fleetsXXTable21-1"]]
         Y$V1[1] <- "Species"
         names(Y) <- Y[1,]
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Species"),
                             factorsAsStrings = FALSE)
         Y$Year <- as.numeric(as.character(Y$variable))
         Y$Value <- as.numeric(str_replace_all(Y$value, "\\D+", ""))
         Y <- Y[!is.na(Y$Value),]
         Y <- Y[!str_detect(Y$Species, regex("total", ignore_case = TRUE)),]
         Y$Year <- 2021
         Y$Measure <- "Catches by Method"
         Y$Unit    <- "Tonnes"
         Y$Species <- str_trim(Y$Species, side = "both")
         Y$Method  <- 'Purse seine'

         Combined_Table <- rbind.fill(X,Y)
                     
         Clean_French_Polynesia[["Catches by Method"]] <- Combined_Table[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]

   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests
   ##
      ##
      ##    Table 8-5
      ##
         Y <- French_Polynesia[["Estimates by the Benefish studies of annual fisheries harvestsXXTable21-7"]]
            for(i in 2:nrow(Y))
            {
               Y$V1[i] <- ifelse((Y$V1[i] == "") &(Y$V1[(i-1)] != ""), Y$V1[(i-1)], Y$V1[i])
            }
         Y$V1[1] <- "Harvest_Sector"
         Y$V2[1] <- "Year"
         names(Y) <- c("Harvest_Sector", "Year", "Volume", "Nominal_Value", "Measure","Table")
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Harvest_Sector", "Year"),
                             factorsAsStrings = FALSE)
         ##
         ##    Aquaculture - split between tonnes AND pieces. Keep the value the same for each and look at allocating at the end
         ##
            Aquaculture <- Y[(Y$`Harvest_Sector` == "Aquaculture"),]
            Aquaculture$Tonnes <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,2],
                                  ifelse(str_detect(Aquaculture$value, "t"), Aquaculture$value, ""))
                                  
            Aquaculture$Pieces <- ifelse(str_detect(Aquaculture$value, "t") & str_detect(Aquaculture$value, "pcs"), str_split_fixed(Aquaculture$value, " and ", 2)[,1],
                                  ifelse(str_detect(Aquaculture$value, "pcs"), Aquaculture$value, ""))
            Aquaculture$Tonnes <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Tonnes, "t", ""), ",",""))
            Aquaculture$Pieces <- as.numeric(str_replace_all(str_replace_all(Aquaculture$Pieces, "pcs", ""), ",",""))
            
            Aquaculture <- reshape2::melt(Aquaculture,
                                          id.var = c("Harvest_Sector", "Year", "Measure", "Table"))   
            Aquaculture <- Aquaculture[!(Aquaculture$variable %in% c("variable")),]
            
         Y <- rbind(Y[(Y$`Harvest_Sector` != "Aquaculture"),], 
                    Aquaculture)                              
         Y$Year <- as.numeric(as.character(Y$Year))
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))
         Y <- Y[!is.na(Y$Value),]
         
         Y$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
         Y$Unit  = ifelse(Y$variable %in% c("value", "Nominal_Value"), "XPF", 
                     ifelse(Y$Harvest_Sector == "Aquaculture",as.character(Y$variable), "Tonnes"))
                     
         Clean_French_Polynesia[["Estimates by the Benefish studies of annual fisheries harvests"]] <- Y[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to GDP in 2021 using an alternative approach - Table21-8
   ##
      X <- French_Polynesia[["Fishing contribution to GDP in 2021 using an alternative approachXXTable21-8"]]
      X_Name <- names(French_Polynesia[6])
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
      Clean_French_Polynesia[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]

      
   ##
   ##    Fish Exports
   ##
      ##
      ##    Pearls
      ##
         X <- French_Polynesia[["Detailed information on the pearl exports of French Polynesia is given the DRM Statistical Bulletin XXTable21-10"]]
         names(X) <- X[1,]
         names(X)[1] <- "Year"
    
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
         X <- X[!is.na(X$Value),]
         X$Dimension       <- "Aggregate Commodity"
         X$Dimension_Value <- "Pearls"
         X$Measure <- "Fish Exports"
         X$Unit    <- "XPF"
      ##
      ##    Non - Pearls
      ##
         Y <- French_Polynesia[["Non-pearl fishery exports of French PolynesiaXXTable21-9"]]
         names(Y) <- Y[1,]
         names(Y)[1] <- "Aggregate Commodity"
    
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Aggregate Commodity"),
                             factorsAsStrings = FALSE)
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))*1000000
         Y <- Y[!is.na(Y$Value),]
         Y$Dimension       <- "Aggregate Commodity"
         Y$Dimension_Value <- str_trim(Y$`Aggregate Commodity`)
         Y$Year    <- str_trim(Y$variable)
         Y$Measure <- "Fish Exports"
         Y$Unit    <- "XPF"
         
         X <- rbind.fill(X,Y)
         X <- X[!str_detect(X$Dimension_Value, "Total"),]

      Clean_French_Polynesia[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]



   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_French_Polynesia, file = 'Data_Intermediate/Clean_French_Polynesia.rda')
##
##    And we're done
##
