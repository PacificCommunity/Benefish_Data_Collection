##
##    Programme:  Clean_Guam.R
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
   ##    Collect up and process the Guam files
   ##
      Guam <- Country_Data[["Guam"]]
      
      Clean_Guam <- list()
      

   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests
   ##
      ##
      ##    Table 22-4
      ##
         Y <- Guam[["Estimates by the Benefish studies of annual fisheries harvestsXXTable22-4"]]
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
         Y$Year <- as.numeric(as.character(Y$Year))
         Y$Value <- as.numeric(str_replace_all(Y$value, ",", ""))
         Y <- Y[!is.na(Y$Value),]
         
         Y$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
         Y$Unit  = ifelse(Y$variable == "Nominal_Value", "US$", 
                     ifelse(Y$Harvest_Sector == "AquacultureX","Pieces", "Tonnes"))
                     
         Clean_Guam[["Estimates by the Benefish studies of annual fisheries harvests"]] <- Y[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
   ##
   ##    Price Measures
   ##
      X <- Guam[["Price of fish received by fishersXXTable22-2"]]
      names(X) <- X[1,]
      names(X)[1] <- "Year"
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Landed Price"
      X$Dimension_Value <- str_replace_all(X$variable, " \\(US\\$\\)", "")
      X$Measure <- "Fish Prices"
      X$Unit    <- "US$"
      Clean_Guam[["Price Measures"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]

   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Guam, file = 'Data_Intermediate/Clean_Guam.rda')
##
##    And we're done
##
