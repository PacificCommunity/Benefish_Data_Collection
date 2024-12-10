##
##    Programme:  Clean_International_Waters.R
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
   ##    Collect up and process the International_Waters files
   ##
      International_Waters <- Country_Data[["International_Waters"]]
      
      Clean_International_Waters <- list()
      

   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests
   ##
      ##
      ##    Table 22-4
      ##
         X <- International_Waters[["Volume of catches in international watersXXTable28-2"]]
         X$V1[1] <- "Harvest_Sector"
         names(X) <- X[1,]
         
         X <- reshape2::melt(X[2:nrow(X),],
                             id.var = c("Measure","Table", "Harvest_Sector"),
                             factorsAsStrings = FALSE)
         X$Year <- as.numeric(as.character(X$variable))
         X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
         X <- X[!is.na(X$Value),]
         X <- X[!is.na(X$Year),]
         X <- X[!str_detect(X$Harvest_Sector, regex("total", ignore_case = TRUE)),]
         X$Measure <- "Catches by Method"
         X$Unit    <- "Tonnes"
         X$Harvest_Sector  <- str_replace_all(X$Harvest_Sector, " \\(t\\)", "")
                     
         Clean_International_Waters[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_International_Waters, file = 'Data_Intermediate/Clean_International_Waters.rda')
##
##    And we're done
##
