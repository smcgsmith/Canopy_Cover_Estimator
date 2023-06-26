# Load required functions
working_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste0(working_directory,"/Source_Files/Image_Manager.R"))

estimate_canopy_cover(current_directory = working_directory, 
                      output_directory = "/Output_Files")
