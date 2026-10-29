process_data_forPB <- function(file_path4="", varname="VAR004", depth=1:2, time_period = c(1:12),naids=NA) {
  current_years <- lapply(file_path4, function(file_path) {
    ncfile <- nc_open(file_path)
    # Select the data based on depth and months
    timenc <- ncvar_get(ncfile, 'time')
    origin_date <- as.Date('1899-12-30')
    time_dates <- origin_date + timenc
    current_year <- as.numeric(format(time_dates, "%Y"))[1]
    nc_close(ncfile)

    return(current_year)
  })
  current_years <- unlist(current_years)
  print(c(min(current_years),max(current_years)))
  
  ncfile <- nc_open(file_path4[1])
  unit <- ncatt_get(ncfile,varname)$units
  var_long_name <- ncatt_get(ncfile,varname)$long_name 
  nc_close(ncfile)
  
  
  data_list <- lapply(file_path4, function(file_path){
    ncfile <- nc_open(file_path)
    # Select the data based on depth and months
    timenc <- ncvar_get(ncfile, 'time') 
    origin_date <- as.Date('1899-12-30')
    time_dates <- origin_date + timenc
    
    if (varname %in% c("VAR165","VAR051","VAR050","VAR001")){
      var_data <- ncvar_get(ncfile, varname)
    }else{
      if(length(depth) == 1){
        var_data <- ncvar_get(ncfile, varname, start = c(1,1,1,1),count=c(-1,-1,1,-1))
        if(varname=='VAR011'){
          var_data2 <- ncvar_get(ncfile, 'VAR012',start = c(1,1,1,1),count=c(-1,-1,1,-1))
          var_data3 <- ncvar_get(ncfile, 'VAR013',start = c(1,1,1,1),count=c(-1,-1,1,-1))
          # print(dim(var_data3))
          var_data <- (var_data+var_data2+var_data3)/10**3
          
          var_data[naids] <- NA
        }
        
      }else{
        
        var_data <- ncvar_get(ncfile, varname)
        var_data <- Bottom_values(var_data,depth)
      }
    }
    nc_close(ncfile)
    
    
    annual_mean_val <- apply(var_data, c(1, 2), mean, na.rm = TRUE)
    # df_list <- list(AllData = var_data, AnnualData = annual_mean_val)
    return( annual_mean_val)
    rm(var_data)
    
    
  })
  # print(dim(data_list))
  var_data <- do.call(abind,c(data_list,along=3))
  print(dim(var_data))
  
  
  
  # Calculate statistics
  # Mean value over full time period
  mean_val <- apply(var_data, c(1, 2), mean, na.rm = TRUE)
  # Standard deviation over annual means
  sd_val <- apply(var_data, c(1, 2), sd, na.rm = TRUE)
  
  max_val <- apply(var_data, c(1, 2), function(x) quantile(x, 0.95, na.rm = TRUE))
  # Min
  min_val <- apply(var_data, c(1, 2), function(x) quantile(x, 0.05, na.rm = TRUE))
  
  return(list(mean = mean_val,sd = sd_val, min = min_val, max = max_val, years=current_years,months = time_period,depth=1, units = unit, varname =var_long_name))

}