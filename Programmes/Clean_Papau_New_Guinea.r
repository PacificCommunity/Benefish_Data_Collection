##
##    Programme:  Clean_Papua_New_Guinea.R
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
   ##    Collect up and process the Papua_New_Guinea files
   ##
      Papua_New_Guinea <- Country_Data[["Papau_New_Guinea"]]
      
      Clean_Papua_New_Guinea <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Papua_New_Guinea[["Estimates by the Benefish studies of annual fisheries harvestsXXTable14-5"]]
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
                                       id.var = c("Harvest sector", "Year", "Nominal value (K)", "Measure", "Table"))   
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
      X$Unit  = ifelse(X$variable == "Nominal value (K)", "K", 
                ifelse(X$Harvest_Sector == "Aquaculture",as.character(X$variable), "Tonnes"))
      Clean_Papua_New_Guinea[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      

   ##
   ##    Fishing contribution to Papua_New_Guinea GDP in 2021 - Table20-5
   ##
      X <- Papua_New_Guinea[["Fishing contribution to GDP in 2021 using an alternative approachXXTable14-7"]]
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
      Clean_Papua_New_Guinea[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      
      
   ##
   ##    Catches of the major fisheries in Papua_New_Guinea - Table6-1 & Table6-2
   ##
      ##
      ##    Table 6-2
      ##
         X <- Papua_New_Guinea[["Catch of the foreign longliners in the PNG EEZXXTable14-3"]]
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
         Y <- Papua_New_Guinea[["Catch of the foreign purse seiners in the PNG EEZ XXTable14-2"]]
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
         Z <- Papua_New_Guinea[["Catches of the locally based purse seine fleet (t)XXTable14-1"]]
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
                     
         Clean_Papua_New_Guinea[["Catches by Method"]] <- Combined_Table[,c("Measure","Table", "Method", "Species", "Year", "Unit", "Value")]
         Clean_Papua_New_Guinea[["Catches by Method"]] <- aggregate(Value ~ Measure + Table + Method + Species + Year + Unit,
                                                                     data = Clean_Papua_New_Guinea[["Catches by Method"]],
                                                                     FUN  = sum,
                                                                     na.action = NULL)


   ##
   ##    Fishing contribution to Papua_New_Guinea GDP in 2021 - Table7-7
   ##
      X <- Papua_New_Guinea[["Fishing contribution to PNG\x92s GDP XXTable14-6"]]
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
      Clean_Papua_New_Guinea[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ##    Fish Exports
   ##
      X <- Papua_New_Guinea[["Fish exportsXXTable14-9"]]
      X[2,c(1,8,9)] <- c("Product","Measure","Table")
      names(X) <- X[2,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Product"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000000
      X$Dimension       <- "Aggregate Commodity"
      X$Dimension_Value <- str_trim(X$Product)
      X$Year <- str_trim(X$variable)
      X <- X[!is.na(X$Value),]
      X <- X[X$Year != "",]
      X$Measure <- "Fish Exports"
      X$Unit    <- "K"
      X <- X[!str_detect(X$Dimension_Value, regex("Product", ignore_case = TRUE)),]
      Clean_Papua_New_Guinea[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]




   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Papua_New_Guinea, file = 'Data_Intermediate/Clean_Papua_New_Guinea.rda')
##
##    And we're done
##
