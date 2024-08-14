##
##    Programme:  Clean_Gillet_Data.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
            
            Cleaned[["Fisheries_HarvestXXPalau"]] <- X
         }  else if(i == "RAWDATA_Sheet1XX000_Pitcairn_graphs")
         {
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
         {
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
         {
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
         {
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
         {
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
         {
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
         }   
  
      
      }
      Cleaned
      
      
      All_Data[[1]]
      
 [1] "RAWDATA_Feuil1XXFigure_32-6"                                                                                                         
[16]                                     "RAWDATA_Sheet1XX000_Revised_New_Cal_graphs"                                                         
[19]                                                                                                 
[22] "RAWDATA_Sheet1XX000_Tuvalu_graphs"                                 "RAWDATA_Sheet1XX000_Vanuatu_graphs"                                "RAWDATA_Sheet1XX000_Wallis_graphs"                                
[25] "RAWDATA_Sheet1XXCorrection_to_pie_charts_on_production_by_fishery" "RAWDATA_Sheet1XXEmployment_1"                                      "RAWDATA_Sheet1XXEmployment_graphs"                                
[28] "RAWDATA_Sheet1XXGraphs_for_export_chapter"                         "RAWDATA_Sheet1XXGraphs_for_fish_consumption_chapter"               "RAWDATA_Sheet1XXGraphs_for_GDP_chapters"                          
[31] "RAWDATA_Sheet1XXGraphs_for_govt_revenue_chapter"                   "RAWDATA_Sheet1XXGraphs_production_chapter"                         "RAWDATA_Sheet1XXMore_graphs_gdp_chapter"                          
[34] "RAWDATA_Sheet1XXMore_graphs_production_chapter"                   
> 
      
      
##
##    And we're done
##
