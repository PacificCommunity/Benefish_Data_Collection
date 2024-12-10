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
         ##    Aquaculture tonnes or pcs... Choose...
         ##       Pieces
         ##
            Y$value[((Y$Harvest_Sector == "Aquaculture") & (Y$Year == 2007) & (Y$variable == "Volume"))] <- 56
            Y$value[((Y$Harvest_Sector == "Aquaculture") & (Y$Year == 2014) & (Y$variable == "Volume"))] <- 8361500
            Y$value[((Y$Harvest_Sector == "Aquaculture") & (Y$Year == 2021) & (Y$variable == "Volume"))] <- 8574012
                             
         Y$Year <- as.numeric(as.character(Y$Year))
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))
         Y <- Y[!is.na(Y$Value),]
         
         Y$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
         Y$Unit  = ifelse(Y$variable == "Nominal_Value", "XPF", 
                     ifelse(Y$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
                     
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
      Clean_French_Polynesia[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_French_Polynesia, file = 'Data_Intermediate/Clean_French_Polynesia.rda')
##
##    And we're done
##
