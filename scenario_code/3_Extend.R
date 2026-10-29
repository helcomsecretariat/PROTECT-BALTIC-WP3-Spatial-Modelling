library(terra)

r <- rast("Data/EnvLayers/BioSEA/Crop_Temp_BioSEA_2010-2024_5-8_depth_0-3_mean.tif")

e <- ext(180273.5, 350273.5, 6605216, 6734930)
rc <- crop(r, e)


dx <- res(rc)[1]

# mask: only extend 2 pixels inland
sea_mask <- !is.na(rc)
dist <- distance(ifel(sea_mask, 1, NA))
mask_2px <- is.na(rc) & dist <= 2 * dx

r_out <- rc

r_vals   <- values(r_out)

mask_vals <- values(mask_2px)


for (step in 1:2) {
  
  # --- find edge: valid cells touching NA ---
  edge <- !is.na(r_out) & focal(is.na(r_out), w=matrix(1,3,3), fun=max, na.rm=TRUE) == 1
  
  # --- compute gradients on current field ---
  gx <- focal(r_out, w=matrix(c(-1,0,1,-2,0,2,-1,0,1),3,3), fun=sum, na.rm=TRUE) / (8*dx)
  gy <- focal(r_out, w=matrix(c(-1,-2,-1,0,0,0,1,2,1),3,3), fun=sum, na.rm=TRUE) / (8*dx)
  
  gx_vals  <- values(gx)
  gy_vals  <- values(gy)
  
  # --- propagate outward ---
  r_new <- r_out
  r_new_vals <- values(r_out)
  
  for (cell in which(values(edge) == 1)) {
    
    # val <- r_out[cell]
    val   <- r_vals[cell]
    
    
    if (is.na(val)) next
    
    # gradx <- gx[cell]
    # grady <- gy[cell]
    gradx <- gx_vals[cell]
    grady <- gy_vals[cell]
    
    # neighbors (8 directions)
    adj <- adjacent(r_out, cells=cell, directions=8, pairs=FALSE, include=FALSE)
    adj <- as.vector(adj)
    adj <- adj[!is.na(adj)]  # remove invalid neighbors
    
    for (nb in adj) {
      if (is.na(r_vals[nb]) && mask_vals[nb]) {
        
        # direction vector (approx)
        xy_c <- xyFromCell(r_out, cell)
        xy_n <- xyFromCell(r_out, nb)
        
        dxn <- xy_n[1] - xy_c[1]
        dyn <- xy_n[2] - xy_c[2]
        
        # normalize to pixel step
        dxn <- dxn / dx
        dyn <- dyn / dx
        
        # gradient projection
        T_est <- val + gradx * dxn + grady * dyn
        
        # r_new[nb] <- T_est
        r_new_vals[nb] <- T_est
      }
    }
  }
  
  # r_out <- r_new
  values(r_out) <- r_new_vals
}

# apply only within 2 pixels
r_final <- rc
r_final[mask_2px] <- r_out[mask_2px]

par(mfrow=c(1,2))
plot(rc, main="Original")
plot(r_final, main="Gradient extrapolated (2 px)")