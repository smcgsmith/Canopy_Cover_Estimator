# Define the conversion function
convert_image <- function(file) {
  
  # Remove the file extension
  file_name <- tools::file_path_sans_ext(file)

  # Set the new file name with the ".jpg" extension
  new_file <- paste0(file_name, ".jpg")

  # Convert the image to JPEG format
  image <- image_read(file)
  image_convert(image, format = "jpeg") %>% image_write(new_file)

  # Return the new file name
  new_file
  
  # # Read the image
  # image <- image_read(circle_crop(file, border_size = 1000, border_colour = "white"))
  # 
  # name <- unlist(strsplit(tools::file_path_sans_ext(file), "Files_To_Process"))[2]
  # 
  # image_write(image, path = paste0(image_directory,name,".jpeg"))
  # 
  # return(name)
}

# Define the function to calculate percent canopy cover
calculate_canopy_cover <- function(file, crop_width = 1000, crop_height = 1000) {
  library(imagefx)
  library(tidyverse)
  
  # Read the image using imager
  image <- imager::load.image(file)
  
  # crop_width <- 1000
  # crop_height <- 1000
  # half_width <- dim(image)[1] / 2
  # half_height <- dim(image)[2] / 2
  # xleft <- half_width - crop_width / 2
  # ybottom <- half_height - crop_height / 2
  # xright <- half_width + crop_width / 2
  # ytop <- half_height + crop_height / 2
  # 
  # image <- crop.image(img = image, xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop)
  # 
  # Crop the image using resize() to make sure photos are same size
  # cropped_image <- resize(image, size_x = crop_width, size_y = crop_height)
  
  # Convert the image to grayscale
  gray_image <- grayscale(image)
  
  # Threshold the gray image to create a binary mask
  binary_image <- gray_image < 0.25

  # Calculate the percent canopy cover
  percent_canopy_cover <- sum(binary_image) / length(binary_image) * 100
  
  name <- unlist(strsplit(tools::file_path_sans_ext(file), "Files_To_Process/"))[2]
  
  # Return a data frame with the file name and percent canopy cover
  data.frame(File = name, Percent_Canopy_Cover = percent_canopy_cover) %>%
    separate(File, into = c("group", "direction", "replicate", "site"), sep = "_")
}

estimate_canopy_cover <- function(current_directory = ".", output_directory = "/Output_Files") {
  
  # Load required packages
  library(imager)
  library(magick)
  library(doParallel)
  
  # Find the file path: these are presets
  path0 <- current_directory
  image_directory <- paste0(path0,"/Files_To_Process")
  function_directory <- paste0(path0,"/Source_Files")
  dir.create(paste0(path0,output_directory))
  output_directory <- paste0(path0,"/Output_Files")
  
  # Load required functions
  # source(paste0(function_directory,"Image_Manager.R"))
  
  # Get a list of all the image files in the directory
  image_files <- list.files(path = image_directory, full.names = TRUE)
  
  # Set the number of cores to be used for parallel processing
  num_cores <- detectCores()
  
  # Initialize parallel backend
  registerDoParallel(cores = num_cores)
  
  # Apply the conversion function to each image file in parallel
  new_files <- foreach(file = image_files, .packages = c("magick", "tools")) %dopar% {
    convert_image(file)
  }
  
  new_files <- unlist(new_files)
  
  # Apply the function to each image file in parallel
  results <- foreach(file = new_files, .combine = rbind) %dopar% {
    calculate_canopy_cover(file)
  }
  
  # Stop the parallel backend
  stopImplicitCluster()
  
  # Convert results to a data frame
  results_df <- as.data.frame(results)
  
  # Save the results as a CSV file
  output_file <- paste0(output_directory, "/Percent_Canopy_Cover.csv")
  write.csv(results_df, file = output_file, row.names = FALSE)
  
  # Print the result
  cat("Percent canopy cover estimates have been saved to:", output_file, "\n")
}
