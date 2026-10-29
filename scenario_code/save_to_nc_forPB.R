save_to_nc_forPB <- function(data, output_file, varname) {
  
  # Define dimensions
  nrows <- ncdim_def("lat","deg North",1:nrow(data$mean))
  ncols <- ncdim_def("lon","deg East",1:ncol(data$mean))
  
  # Step 2: Define the variables (mean, max, min, sd)
  varMean <- ncvar_def("mean", data$units, list(nrows, ncols), -1, longname = paste0("Mean of ",data$varname), prec = "double")
  varSD <- ncvar_def("sd", data$units, list(nrows, ncols), -1, longname = paste0("Standard deviation of ",data$varname), prec = "double")
  varMin <- ncvar_def("min", data$units, list(nrows, ncols), -1, longname = paste0("Min of ",data$varname), prec = "double")
  varMax <- ncvar_def("max", data$units, list(nrows, ncols), -1, longname = paste0("Max of ",data$varname), prec = "double")
  
  ncnew <- nc_create(output_file, list(varMean, varSD,varMin,varMax))

  # Step 5: Store data in the variables
  ncvar_put(ncnew, varMean, data$mean)
  ncvar_put(ncnew, varSD, data$sd)
  ncvar_put(ncnew, varMin, data$min)
  ncvar_put(ncnew, varMax, data$max)
  
  # Step 6: Add metadata (optional)
  ncatt_put(ncnew, 0, "source", "Generated from RCO-SCOBI RCP8.5 SSP2 data")

  # Close the NetCDF file
  # Step 7: Close the NetCDF file
  nc_close(ncnew)
}