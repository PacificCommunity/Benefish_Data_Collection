##
##    Programme:  Clean_Gillet_Data.r
##
##    Objective:  This programme cleans up the spreadsheets which were read into R in the programme,
##                Read_Spreadsheets.
##
##                The main content of the spreadsheets is fishery harvest measures, although there are
##                also some information on revenue, exports, employment, and GDP which might have value.
##
##                I'm on the fence about them if we can get the data more authoratively from other sources.
##
##    Next Steps: Can we also read in the Benefish 1-3 data and make a time series?
##
##    Author:     James Hogan, FAME - The Pacific Community (SPC)
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Collect up the downloaded files
   ##
      Contents <- as.data.frame(list.files(path = "Data_Intermediate/", pattern = "*.rda"))
      names(Contents) <- "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n=2)[,1]
      
      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Data_Intermediate/", File))
                           X <- get(str_split_fixed(File, "\\.", n=2)[,1])
                           rm(list = c(as.character(File)))
                           return(X)
                        })
      names(All_Data) <- Contents$Dframe
                        
   ##
   ##    Now clean them
   ##
      Cleaned <- list()
      for(i in Contents$Dframe)
      {
         if(i == "RAWDATA_Sheet1XX000_Am_Sam_graphs")
         {##
          ##  1
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,1:2]
            Fisheries_Harvest_Value  <- All_Data[[i]][,13:14]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "US$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
                       
            X$Fishery <- str_trim(X$Fishery, side = "both")

            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X$Freq <- "A"
            X$Time_Period <- 2021
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            Cleaned[["Fisheries_HarvestXXAmerican_Samoa"]] <- X
            
         } else if(i == "RAWDATA_Sheet1XX000_Cooks_graphs")
         {##
          ##  2
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][1:5,1:2]
            Fisheries_Harvest_Pieces <- All_Data[[i]][6,1:2]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(1,3)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "NZ$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 81500
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X$Freq <- "A"
            X$Time_Period <- 2021
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            Cleaned[["Fisheries_HarvestXXCook_Islands"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Fiji_graphs")
         {##
          ##  3
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(8,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][17,c(8,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(8,9)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "F$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 20000
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture "] <- 351
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXFiji"]] <- X
            
         } else if(i == "RAWDATA_Sheet1XX000_FPoly_graphs")
         {##
          ##  4
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][6,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(7,8)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "XPF"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 8571012
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture "] <- 1542
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXFrench_Polynesia"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_FSM_graphs")
         {##
          ##  5
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(12,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][16,c(12,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(12,13)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "US$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 65000
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture "] <- 0
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXFederated_States_of_Micronesia"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Guam_graphs")
         {##
          ##  6
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(8,9)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "US$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")
           
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXGuam"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Kiribati_graphs")
         {##
          ##  7
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(11,12)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "AU$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXKiribati"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Marshall_Islands_Graphs")
         {##
          ##  8
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][1:7,c(1,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][6,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][1:7,c(1,3)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "US$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 2.3
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture "] <- 22000
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXMarshall_Islands"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Nauru")
         {##
          ##  9
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][1:6,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][9:15,c(1,2)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "AU$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXNauru"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_New_Caledonia_graphs")
         {##
          ##  10
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][1:6,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][1:6,c(1,3)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "XPF"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXNew_Caledonia"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Niue")
         {##
          ##  11
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][1:6, c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][9:15,c(1,2)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "NZ$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXNiue"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Northern_Marianas_graphs")
         {##
          ##  12
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][9:14,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][9:14,c(7,8)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "US$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXNorthern_Marianas_Islands"]] <- X
         } else if(i == "RAWDATA_Sheet1XX000_Palau_graphs")
         {##
          ##  13
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][6,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(1,3)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "US$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 4419
            
            Fisheries_Harvest_Volume$Fishery[Fisheries_Harvest_Volume$Fishery == "Aquaculture (pcs and mt)"] <- "Aquaculture"
            Fisheries_Harvest_Value$Fishery[Fisheries_Harvest_Value$Fishery   == "Aquaculture (pcs and mt)"] <- "Aquaculture"
            Fisheries_Harvest_Pieces$Fishery[Fisheries_Harvest_Pieces$Fishery == "Aquaculture (pcs and mt)"] <- "Aquaculture"
            
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture"] <- 11
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXPalau"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Pitcairn_graphs")
         {##
          ##  14
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(12,13)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "NZ$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXPitcairn_Islands"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_PNG_graphs")
         {##
          ##  15
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][5:10,c(8,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][10,c(8,9)]
            Fisheries_Harvest_Value  <- All_Data[[i]][5:10,c(8,9)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "K"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 850
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture"] <- 10000
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXPapau_New_Guinea"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Solomons_graphs")
         {##
          ##  16
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(1,8)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "SI$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXSolomon_Islands"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Tokelau_graphs")
         {##
          ##  17
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(1,3)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "NZ$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")
            
            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXTokelau"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Tonga_graphs")
         {##
          ##  18
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(6,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][6,c(6,7)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(6,7)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "T$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 35000
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture"] <- 0

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXTonga"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Samoa_graphs")
         {##
          ##  19
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][2:7,c(17,14)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][7,c(17,18)]
            Fisheries_Harvest_Value  <- All_Data[[i]][2:7,c(17,18)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "ST$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 10000
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture "] <- 6.5

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXSamoa"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Tuvalu_graphs")
         {##
          ##  20
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][,c(1,3)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "AU$"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXTuvalu"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Vanuatu_graphs")
         {##
          ##  21
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][1:6,c(1,2)]
            Fisheries_Harvest_Pieces <- All_Data[[i]][6,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][1:6,c(1,3)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Pieces$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Pieces$Unit <- "Pieces"
            Fisheries_Harvest_Value$Unit  <- "VT"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Pieces) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            ##
            ##    Hardcoded difference between pdf and excel data
            ##
            Fisheries_Harvest_Pieces$Obs_Value[1] <- 4000
            Fisheries_Harvest_Volume$Obs_Value[Fisheries_Harvest_Volume$Fishery == "Aquaculture"] <- 8

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Pieces, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXVanuatu"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Wallis_graphs")
         {##
          ##  22
          ##
            Fisheries_Harvest_Volume <- All_Data[[i]][1:6,c(1,2)]
            Fisheries_Harvest_Value  <- All_Data[[i]][1:6,c(1,10)]
            
            Fisheries_Harvest_Volume$Measure <- "Volume"
            Fisheries_Harvest_Value$Measure  <- "Value"

            Fisheries_Harvest_Volume$Unit <- "Tonnes"
            Fisheries_Harvest_Value$Unit  <- "XPF"

            names(Fisheries_Harvest_Volume) <- c("Fishery", "Obs_Value", "Measure", "Unit")
            names(Fisheries_Harvest_Value)  <- c("Fishery", "Obs_Value", "Measure", "Unit")

            X <- rbind(Fisheries_Harvest_Volume, 
                       Fisheries_Harvest_Value)
            X$Fishery   <- str_trim(str_replace_all(X$Fishery, " \\(pcs\\)",""), side = "both")
            
            X$Obs_Value <- as.numeric(str_replace_all(X$Obs_Value, ",", ""))
            X <- X[((X$Obs_Value != "") & !is.na(X$Obs_Value)),]
            
            X$Freq <- "A"
            X$Time_Period <- 2021
            
            Cleaned[["Fisheries_HarvestXXWallis_and_Futuna"]] <- X
         }   
      }
      ##
      ##    Make it into a dataframe
      ##
      Fisheries_Harvest <- do.call(rbind, lapply(names(Cleaned), function(x){
                                                 Dataframe <- Cleaned[[x]]
                                                 Dataframe$PICT <- str_split_fixed(x,"XX",2)[,2]
                                                 return(Dataframe)}))
      
      unique(Fisheries_Harvest$Fishery)
      Fisheries_Harvest$Fishery[Fisheries_Harvest$Fishery == "Offshore Locally based"] <- "Offshore Locally-based"
      
      unique(Fisheries_Harvest$Unit)
      Fisheries_Harvest$Unit[Fisheries_Harvest$Unit == "Offshore Locally based"] <- "Offshore Locally-based"
      
      
      ##
      ## Next bits
      ##    "RAWDATA_Sheet1XXCorrection_to_pie_charts_on_production_by_fishery": is a whole series of totals without context
      ##    "RAWDATA_Feuil1XXFigure_32-6": is percentage change columen in "Table 32-3: Changes in access fees 2007–2021"
      ##    "RAWDATA_Sheet1XXCorrection_to_pie_charts_on_production_by_fishery": is another series of totals without context
      ##    "RAWDATA_Sheet1XXEmployment_1": is a sex employment proportional split for each country
      ##
      
      ##
      ##    The "could be usefuls" - but might also be aggregates of the above
      ##       "RAWDATA_Sheet1XXEmployment_graphs"      
      ##       "RAWDATA_Sheet1XXGraphs_for_export_chapter"
      ##       "RAWDATA_Sheet1XXGraphs_for_GDP_chapters"
      ##       "RAWDATA_Sheet1XXGraphs_for_govt_revenue_chapter"
      ##       "RAWDATA_Sheet1XXGraphs_production_chapter"
      ##       "RAWDATA_Sheet1XXMore_graphs_gdp_chapter"
      
   save(Fisheries_Harvest, file = "Data_Intermediate/Fisheries_Harvest.rda") 
   save(All_Data, file = "Data_Intermediate/All_Data.rda") 

 
##
##    And we're done
##
