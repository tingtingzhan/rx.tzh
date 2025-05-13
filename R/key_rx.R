

#' @title Index to Factor, selected using Regular Expression
#' 
#' @description ..
#' 
#' @param pattern,envir see function [select_rx]
#' 
#' @param ... additional parameters of function [id2key]
#' 
#' @details
#' Function [key_rx] is the inverse of function [splitKey].
#' 
#' @examples 
#' npk |> 
#'  within.data.frame(expr = {
#'   N = N |> as.character() |> as.numeric()
#'   P = P |> as.character() |> as.numeric()
#'   K = K |> as.character() |> as.numeric()
#'   tmp = key_rx(pattern = 'N|P|K')
#'  })
#' @export
key_rx <- function(pattern, envir = parent.frame(), ...) {
  x <- select_rx(pattern = pattern, envir = envir)
  if (!length(x)) return(invisible())
  x1 <- x |>
    lapply(FUN = as.logical) |>
    do.call(what = cbind)
  if (!grepl(pattern = '\\|', x = pattern)) {
    colnames(x1) <- gsub(pattern = pattern, replacement = '', x = colnames(x1))
  } # else do nothing
  x1 |> id2key(...)
}











#' @title Indices to Factor
#' 
#' @description ..
#' 
#' @param x \link[base]{logical} \link[base]{matrix}
#' 
#' @param collapse \link[base]{character} scalar, symbol to
#' collapse all keywords
#' 
#' @param none \link[base]{character} scalar, 
#' label for all-negative, default `'[none]'`
#' 
#' @param ... ..
#' 
#' @details ..
#' 
#' @returns 
#' Function [id2key] returns a \link[base]{character} \link[base]{vector}.
#' Note that function [id2key] does *not* return a \link[base]{factor}, 
#' as the levels will be very messy and may create problems when
#' \link[base]{rbind}.
#' 
#' @examples 
#' (x = cbind(
#'  a = c(1, 1, 1, 0, NA, 1, NA),
#'  b = c(NA, 0, 1, 0, NA, NA, NA),
#'  c = c(1, 0, 0, 0, 0, NA, NA)
#' ))
#' storage.mode(x) = 'logical'
#' id2key(x)
#' @keywords internal
#' @export
id2key <- function(x, collapse = ';', none = '[none]', ...) {
  
  if (!is.matrix(x) || typeof(x) != 'logical') stop('input must be logical-matrix')
  nm <- dimnames(x)[[2L]]
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm)) || anyDuplicated(nm)) stop('illegal colname names')
  dm <- dim(x)
  
  x1 <- x
  x1[is.na(x1)] <- FALSE # using .Internal(which()) in lapply is slow
  ret <- vapply(seq_len(dm[1L]), FUN = \(i) {
    nm[x1[i,]] |> paste(collapse = collapse)
  }, FUN.VALUE = '')
  ret[!nzchar(ret)] <- NA_character_ # 0-char in `ret` may be FALSE+missing, or all-FALSE (i.e., confirmed empty)
  
  if (length(id0 <- which(.rowSums(x, m = dm[1L], n = dm[2L], na.rm = FALSE) == 0L))) { # confirmed empty
    if (!is.character(none) || length(none) != 1L || anyNA(none) || !nzchar(none)) stop('label for all-negative must be len-1 character')
    #if (any(none == c(nm, ret), na.rm = TRUE)) # ACTUALLY ALLOW THIS!!!! stop('All-negative label ', sQuote(none), ' clash!')
    ret[id0] <- none
  }
  
  return(ret)
  
}



