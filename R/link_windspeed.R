
# Add windspeed data from raster file to DHS data

link_windspeed <- function(r_stack, comb_dat, lags = integer(0), method = "simple") {
  
  # Keep only windspeed layers
  ws_stack <- r_stack[[grep("^windspeed_", names(r_stack))]]
  ws_years <- as.integer(sub("^windspeed_", "", names(ws_stack)))
  
  # Indices of rows with coords
  idx <- which(!is.na(comb_dat$LATNUM) & !is.na(comb_dat$LONGNUM))
  
  # Create vector to store values
  n <- nrow(comb_dat)
  winds_out <- numeric(n)
  
  if (length(idx) > 0) {
    
    # Make DHS cluster SpatVector
    pts <- terra::vect(
      data.frame(x = comb_dat$LONGNUM[idx], y = comb_dat$LATNUM[idx]),
      geom = c("x","y"), crs = "EPSG:4326"
    )
    pts <- terra::project(pts, terra::crs(ws_stack))
    
    # Extract winspeeds at DHS clusters
    mat <- terra::extract(ws_stack, pts, method = method, ID = FALSE)
    mat <- as.matrix(mat) # Convert to matrix
    
    # Get windspeed in the current year --------
    
    # Set the default windspeed value to 0
    vals0 <- rep(0, length(idx)) 
    
    # Column index for current year
    col_idx0 <- match(comb_dat$year[idx], ws_years)  
    
    # Get rows that have valid matches
    sel0 <- !is.na(col_idx0)
    rpos0 <- which(sel0)
    
    # Add windspeed values
    if (length(rpos0) > 0) {
      vals0[rpos0] <- mat[cbind(rpos0, col_idx0[sel0])]
    }
    vals0[is.na(vals0)] <- 0
    winds_out[idx] <- vals0
    
    # Add windspeed variable
    comb_dat$windspeed <- winds_out
    
    # Add lagged windspeed values --------
    
    if (length(lags) > 0) {
      for (k in lags) {
        
        # Set the default windspeed value to 0
        out <- numeric(n)
        
        # Column index for the year minus the lag
        col_idx <- match(comb_dat$year[idx] - k, ws_years)
        
        # Identify rows with valid year matches
        sel <- !is.na(col_idx)
        rpos <- which(sel)
        
        # Set default values to 0 for the current index subset
        vals <- rep(0, length(idx))
        
        # Add lagged windspeed values from matrix where matches exist
        if (length(rpos) > 0) {
          vals[rpos] <- mat[cbind(rpos, col_idx[sel])]
        }
        vals[is.na(vals)] <- 0
        out[idx] <- vals
        
        # Add lagged windspeed column to dataset
        comb_dat[[paste0("windspeed_lag", k)]] <- out
      }
    }
  }
  
  return(comb_dat)
}
