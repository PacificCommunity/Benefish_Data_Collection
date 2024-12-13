##
##    Programme:  Clean_Tokelau.R
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
   ##    Collect up and process the Tokelau files
   ##
      Tokelau <- Country_Data[["Tokelau"]]
      
      Clean_Tokelau <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Tokelau[["Estimates by the Benefish studies of annual fisheries harvestsXXTable26-5"]]
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
         #X$`Volume  `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 8202
         #X$`Volume  `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- 20000
         #X$`Volume  `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 3150
         
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$variable <- as.character(X$variable)
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (NZ$)", "NZ$", 
                  ifelse(X$Harvest_Sector == "AquacultureX","Pieces", "Tonnes"))
      Clean_Tokelau[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Tokelau GDP in 2021 - Table20-5
   ##
      X <- Tokelau[["Fishing contribution to Tokelau GDP in 2021XXTable26-7"]]
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
      Clean_Tokelau[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
    

   ##
   ##    Fishing contribution to Tokelau GDP in 2021 - Table7-7
   ##
      X <- Tokelau[["Agriculture and fisheries contribution to the Tokelau GDP (current prices, NZ$)XXTable26-6"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$variable <- as.character(X$variable)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, regex("total", ignore_case = TRUE)),]
      X <- X[!str_detect(X$Harvest_Sector, regex("%", ignore_case = TRUE)),]
      X$Year    <- as.numeric(paste0("20",str_split_fixed(X$variable, "/",2)[,2]))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "NZ$"
      Clean_Tokelau[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ##    Catches of the major fisheries in Tokelau - Table6-1 & Table6-2
   ##
      ##
      ##    Table 6-2
      ##
         X <- Tokelau[["Recent annual catches by the foreign fleets in the Tokelau EEZXXTable26-3"]]
         X$V1[2] <- "Year"
         names(X) <- c(X[2,1:9],X[1,10:11])
         for(i in 2:nrow(X))
         {
            X$Year[i] <- ifelse((X$Year[i] == "") &(X$Year[(i-1)] != ""), X$Year[(i-1)], X$Year[i])
         }
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Year", "Gear "),
                             factorsAsStrings = FALSE)
         X$Species <- as.character(X$variable)
         X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
         X <- X[!is.na(X$Value),]
         X <- X[!str_detect(X$Species, regex("total", ignore_case = TRUE)),]
         X <- X[!str_detect(X$Species, regex("Effort", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- "Tonnes"
         X$Species <- str_trim(X$Species, side = "both")
         X$Method  <- X$Gear

         Combined_Table <- X
                     
         Clean_Tokelau[["Catches by Method"]] <- Combined_Table[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]

   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Tokelau, file = 'Data_Intermediate/Clean_Tokelau.rda')
##
##    And we're done
##
