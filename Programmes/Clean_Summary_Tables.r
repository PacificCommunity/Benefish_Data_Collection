##
##    Programme:  Clean_Summary_Tables.R
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
   ##    Collect up and process the Summary_Tables files
   ##
      Summary_Tables <- Country_Data[["Summary_Tables"]]
      
      Clean_Summary_Tables <- list()
      
   ##
   ##    Volume of production in 2021XXTable29-1
   ##
      X <- Summary_Tables[["Estimates by the Benefish studies of annual fisheries harvestsXXTable14-5"]]
      X$V1[1] <- "PICT"
      Mapping_Table <- data.frame(Metric = paste0(X[1,],X[2,]),
                                    
      
      X$ID <- 1:nrow(X)
      
      
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
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 200
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- 160000
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 10000
         
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$variable <- as.character(X$variable)
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (K)", "K", 
                  ifelse(X$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
      Clean_Summary_Tables[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Summary_Tables GDP in 2021 - Table20-5
   ##
      X <- Summary_Tables[["Fishing contribution to GDP in 2021 using an alternative approachXXTable14-7"]]
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
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "K")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Summary_Tables[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      
      
   ##
   ##    Catches of the major fisheries in Summary_Tables - Table6-1 & Table6-2
   ##
      ##
      ##    Table 6-2
      ##
         X <- Summary_Tables[["Catch of the foreign longliners in the PNG EEZXXTable14-3"]]
         X$V1[1] <- "Year"
         names(X) <- X[1,]
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         X$Species <- as.character(X$variable)
         X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
         X <- X[!is.na(X$Value),]
         X <- X[!str_detect(X$Species, regex("total", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- "Tonnes"
         X$Species <- str_trim(X$Species, side = "both")
         X$Method  <- 'Long Liners'
      ##
      ##    Table 6-1
      ##
         Y <- Summary_Tables[["Catch of the foreign purse seiners in the PNG EEZ XXTable14-2"]]
         Y$V1[1] <- "Year"
         names(Y) <- Y[1,]
         Y <- reshape2::melt(Y[2:nrow(Y),],
                             id.var = c("Measure","Table", "Year"),
                             factorsAsStrings = FALSE)
         Y$Species <- as.character(Y$variable)
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))
         Y <- Y[!is.na(Y$Value),]
         Y <- Y[!str_detect(Y$Species, regex("total", ignore_case = TRUE)),]
         Y$Measure <- "Catches by Method"
         Y$Unit    <- "Tonnes"
         Y$Species <- str_trim(Y$Species, side = "both")
         Y$Method  <- 'Purse seine'
      ##
      ##    Locals
      ##
         Z <- Summary_Tables[["Catches of the locally based purse seine fleet (t)XXTable14-1"]]
         Z$V1[1] <- "Species"
         names(Z) <- Z[1,]
         Z <- reshape2::melt(Z[2:nrow(Z),],
                             id.var = c("Measure","Table", "Species"),
                             factorsAsStrings = FALSE)
         Z$Year <- as.character(Z$variable)
         Z$Value <- as.numeric(as.numeric(str_replace_all(Z$value, ",", "")))
         Z <- Z[!is.na(Z$Value),]
         Z <- Z[!str_detect(Z$Species, regex("total", ignore_case = TRUE)),]
         Z$Measure <- "Catches by Method"
         Z$Unit    <- "Tonnes"
         Z$Species <- str_trim(Z$Species, side = "both")
         Z$Method  <- 'Purse seine'

         Combined_Table <- rbind.fill(X,Y,Z)
         Combined_Table$Year <- as.numeric(str_replace_all(Combined_Table$Year, "\\D+", ""))
                     
         Clean_Summary_Tables[["Catches by Method"]] <- Combined_Table[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]
         Clean_Summary_Tables[["Catches by Method"]] <- aggregate(Value ~ Measure + Table + Method + Species + Year + Unit,
                                                                     data = Clean_Summary_Tables[["Catches by Method"]],
                                                                     FUN  = sum,
                                                                     na.action = NULL)


   ##
   ##    Fishing contribution to Summary_Tables GDP in 2021 - Table7-7
   ##
      X <- Summary_Tables[["Fishing contribution to PNG\x92s GDP XXTable14-6"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))*1000000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      #X <- X[str_detect(X$Harvest_Sector, "Fisheries contribution to GDP"),]
      X$Year    <- as.numeric(str_replace_all(X$variable, "\\D+", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "K"
      Clean_Summary_Tables[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Summary_Tables, file = 'Data_Intermediate/Clean_Summary_Tables.rda')
##
##    And we're done
##
