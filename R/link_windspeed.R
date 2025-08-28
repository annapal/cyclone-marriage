
link_windspeed <- function(r_stack, comb_dat, lags = integer(0), method = "simple") {
  # keep only windspeed layers
  ws_stack <- r_stack[[grep("^windspeed_", names(r_stack))]]
  ws_years <- as.integer(sub("^windspeed_", "", names(ws_stack)))
  
  # indices of rows with coords
  idx <- which(!is.na(comb_dat$LATNUM) & !is.na(comb_dat$LONGNUM))
  n <- nrow(comb_dat)
  winds_out <- numeric(n)  # default 0s
  
  if (length(idx) > 0) {
    # make points
    pts <- vect(
      data.frame(x = comb_dat$LONGNUM[idx], y = comb_dat$LATNUM[idx]),
      geom = c("x","y"), crs = "EPSG:4326"
    )
    pts <- project(pts, crs(ws_stack))
    
    # extract all years at once
    mat <- extract(ws_stack, pts, method = method, ID = FALSE)
    mat <- as.matrix(mat)  # rows == length(idx), cols == length(ws_years)
    
    # ------- current-year windspeed -------
    col_idx0 <- match(comb_dat$year[idx], ws_years)  # column for current year
    sel0 <- !is.na(col_idx0)
    vals0 <- rep(0, length(idx))
    rpos0 <- which(sel0)
    if (length(rpos0) > 0) {
      vals0[rpos0] <- mat[cbind(rpos0, col_idx0[sel0])]
    }
    vals0[is.na(vals0)] <- 0
    winds_out[idx] <- vals0
    
    # ------- helper: lag vector for any k -------
    make_ws_lag <- function(k) {
      out <- numeric(n)                 # default 0s for all rows
      col_idx <- match(comb_dat$year[idx] - k, ws_years)
      sel <- !is.na(col_idx)
      vals <- rep(0, length(idx))
      rpos <- which(sel)
      if (length(rpos) > 0) {
        vals[rpos] <- mat[cbind(rpos, col_idx[sel])]
      }
      vals[is.na(vals)] <- 0
      out[idx] <- vals
      out
    }
    
    # add lag columns if requested (positive integers)
    lags <- lags[is.finite(lags) & lags > 0]
    if (length(lags) > 0) {
      for (k in lags) {
        comb_dat[[paste0("windspeed_lag", k)]] <- make_ws_lag(k)
      }
    }
  }
  
  comb_dat$windspeed <- winds_out
  comb_dat
}