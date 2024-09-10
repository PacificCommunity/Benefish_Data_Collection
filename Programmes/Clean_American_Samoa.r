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
      Contents <- as.data.frame(list.files(path = "Data_Intermediate/", pattern = "*Manually_Collected_Data_Version2.rda"))
      names(Contents) <- "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n=2)[,1]
      
      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Data_Intermediate/", File))
                           X <- get(str_split_fixed(File, "\\.", n=2)[,1])
                           rm(list = c(as.character(File)))
                           return(X)
                        })
      names(All_Data) <- Contents$Dframe
        
      Member_Country <- str_replace_all(str_split_fixed(Contents$Dframe, "XX",2)[,1], "RAWDATA_", "")
      t(t(Member_Country))
        
 [1,] "American_Samoa"                
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
