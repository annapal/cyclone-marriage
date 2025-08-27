
link_windspeed <- function(r_stack, comb_dat, method = "simple") {
  # keep only windspeed layers
  ws_stack <- r_stack[[grep("^windspeed_", names(r_stack))]]
  ws_years <- as.integer(sub("^windspeed_", "", names(ws_stack)))
  
  # indices of rows with coords
  idx <- which(!is.na(comb_dat$LATNUM) & !is.na(comb_dat$LONGNUM))
  winds_out <- numeric(nrow(comb_dat))  # default 0s
  
  if (length(idx) > 0) {
    # make points
    pts <- vect(data.frame(x = comb_dat$LONGNUM[idx],
                           y = comb_dat$LATNUM[idx]),
                geom = c("x","y"), crs = "EPSG:4326")
    pts <- project(pts, crs(ws_stack))
    
    # extract all years at once
    mat <- extract(ws_stack, pts, method = method, ID = FALSE)
    mat <- as.matrix(mat)
    
    # match each row's year to correct column
    col_idx <- match(comb_dat$year[idx], ws_years)
    sel <- !is.na(col_idx)
    
    vals <- rep(0, length(idx))
    vals[sel] <- mat[cbind(which(sel), col_idx[sel])]
    vals[is.na(vals)] <- 0
    
    winds_out[idx] <- vals
  }
  
  comb_dat$windspeed <- winds_out
  comb_dat
}