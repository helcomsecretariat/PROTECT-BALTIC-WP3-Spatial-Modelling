Bottom_values <- function(var_data,dind){
  
  depth_indices_long <- rep(dind,dim(var_data)[4])
  
  new.var_data <- aperm(var_data, c(1,2,4,3))
  
  arr_3d_flat_new <- array(new.var_data, dim = c(nrow(new.var_data) * ncol(new.var_data) * dim(new.var_data)[3], dim(new.var_data)[4]))
  # Vectorized extraction using array indexing
  result_values_new <- arr_3d_flat_new[cbind(1:nrow(arr_3d_flat_new),depth_indices_long)]
  results_new <- array(result_values_new, dim(new.var_data)[1:3])
  
  return(results_new)
}
