##
##    Programme:  Clean_American_Samoa.R
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
   ##    Collect up and process the American_Samoa files
   ##
      American_Samoa <- Country_Data[["American_Samoa"]]
      
      Clean_American_Samoa <- list()
   ##
   ##    Catches of the major fisheries in American Samoa - Table20-1
   ##
      X <- American_Samoa[[1]]
      X_Name <- names(American_Samoa[1])
      X$V1[1] <- "Year"
      names(X) <- X[1,]
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Method", "Year"),
                          factorsAsStrings = FALSE)
      X$value <- as.numeric(str_replace_all(X$value, ",",""))
      X <- X[!is.na(X$value),]
      X <- X[!str_detect(X$variable, "total"),]
      X <- X[!str_detect(X$Method, "total"),]
      X <- X[!str_detect(X$variable, "pounds"),]
      X$Measure <- "Catches by Method"
      X$Unit  = "Tonnes"
      X$Value = X$value/1000 
      Clean_American_Samoa[["Catches by Method"]] <- X[,c("Measure","Table", "Method", "Year", "Unit", "Value")]

   ##
   ##    Catches by American Samoa longline vessels - Table20-2
   ##
      X <- American_Samoa[[2]]
      X_Name <- names(American_Samoa[2])
      X$V1[2] <- "Species"
      Map <- data.frame(variable = as.character(names(X)),
                        Year = as.character(X[2,]))
      
      
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("V12","V13", "V1"),
                          factorsAsStrings = FALSE)
      X <- merge(X,
                 Map,
                 by = c("variable"))
      X$value <- as.numeric(str_replace_all(X$value, ",",""))
      X <- X[!is.na(X$value),]
      X$V1 <- str_trim(X$V1, side = "both")
      X$V1 <- str_replace_all(X$V1, "\\,  NPO \\(t\\)", "")
      X$V1 <- str_replace_all(X$V1, "\\,  SPO \\(t\\)", "")
      X$V1 <- str_replace_all(X$V1, "  ", " ")
      X$V1 <- str_replace_all(X$V1, " \\(t\\)", "")
      X <- X[!str_detect(X$V1, "total"),]
      X <- X[!str_detect(X$V1, "Total"),]
      X <- X[!str_detect(X$V1, "Species"),]
      
		Y <- with(X[X$V1 != "Number of vessels",],
              aggregate(list(Value = value),
                        list(Year = Year,
                             Species = V1,
                             Measure = V12),
                        sum,
                        na.rm = TRUE))    
      Y$Unit  = "Tonnes"
      Y$Table  = "20-2a"
                        
		Z <- with(X[X$V1 == "Number of vessels",],
              aggregate(list(Value = value),
                        list(Year = Year,
                             Measure = V12,
                             Table = V13),
                        sum,
                        na.rm = TRUE))      
      Z$Unit  = "Number"
      Z$Table  = "20-2b"
      
      Clean_American_Samoa[["Catch Volume - Longline"]]      <- Y[,c("Measure","Table", "Species", "Year", "Unit", "Value")]
      Clean_American_Samoa[["Number of vessels - Longline"]] <- Z[,c("Measure","Table",            "Year", "Unit", "Value")]


   ##
   ##    Annual fisheries and aquaculture harvest - Table20-3
   ##
      X <- American_Samoa[[3]]
      X_Name <- names(American_Samoa[3])
      names(X) <- X[1,]
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",",""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Measure <- "Annual fisheries and aquaculture harvest"
      X$Year    <- 2021
      X$Unit  = ifelse(X$variable == "Volume (t)", "Tonnes", "US$")
      Clean_American_Samoa[["Annual fisheries and aquaculture harvest"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table20-4
   ##
      X <- American_Samoa[[4]]
      X_Name <- names(American_Samoa[4])
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",",""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Volume (t)", "Tonnes", "US$")
      Clean_American_Samoa[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]

   ##
   ##    Fishing contribution to American Samoa GDP in 2021 - Table20-5
   ##
      X <- American_Samoa[[5]]
      X_Name <- names(American_Samoa[5])
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",",""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Year    <- 2021
      X$Measure <- "Fishing contribution to GDP - VAR Method"
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "US$")
      Clean_American_Samoa[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_American_Samoa, file = 'Data_Intermediate/Clean_American_Samoa.rda')
##
##    And we're done
##





 [2,] "Cook_Islands"                  
 [3,] "Federated_States_of_Micronesia"
 [4,] "Fiji"                          
 [5,] "French_Polynesia"              
 [6,] "Guam"                          
 [7,] "International_Waters"          
 [8,] "Kiribati"                      
 [9,] "Marshall_Islands"              
[10,] "Nauru"                         
[11,] "New_Caledonia"                 
[12,] "Niue"                          
[13,] "Northern_Marianas_Islands"     
[14,] "Palau"                         
[15,] "Papau_New_Guinea"              
[16,] "Pitcairn_Islands"              
[17,] "Samoa"                         
[18,] "Solomon_Islands"               
[19,] "Summary_Tables"                
[20,] "Tokelau"                       
[21,] "Tonga"                         
[22,] "Tuvalu"                        
[23,] "Vanuatu"                       
[24,] "Wallis_and_Futuna"  




   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
