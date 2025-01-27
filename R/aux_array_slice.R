.slice_array <- function(arr,
                         dim_sizes,
                         n_dims) {

  other_dims <- setdiff(x = 1:n_dims, y = c(1,2))
  other_indices <- lapply(X = dim_sizes[other_dims],
                          FUN = seq_len)

  other_grid <- expand.grid(other_indices)

  ls_mat <- list()
  for (i in 1:nrow(x = other_grid)) {
    indices <- as.list(x = other_grid[i,])
    full_indices <- vector(mode = "list", length = n_dims)
    full_indices[c(1,2)] <- list(quote(expr = ))
    full_indices[other_dims] <- indices
    ls_mat[[i]] <- do.call(what = "[",
                           args = c(list(arr), full_indices))
  }
  ls_dt <- lapply(X = ls_mat,
                  FUN = function(m){data.table::as.data.table(x = rbind(m))})
  return(ls_dt)
}
