
#' @title Operation on Objects Specified by \link[base]{regex}
#' 
#' @description ..
#' 
#' @param pattern \link[base]{character} scalar, regular expression
#' 
#' @param envir an \link[base]{environment}, 
#' a \link[base]{list}, 
#' a \link[base]{data.frame}. 
#' May also be a \link[base]{matrix} for function [select_rx].
#' 
#' @param ... additional parameters of \link[base]{grep}, most importantly `invert`
#' 
#' @returns  
#' 
#' Function [select_rx()] selects
#' 
#' \itemize{
#' \item {columns from a \link[base]{data.frame} by `pattern` matching against its \link[base]{names},
#' and returns a \link[base]{data.frame}.}
#' \item {elements from a \link[base]{list} by `pattern` matching against its \link[base]{names},
#' and returns a \link[base]{list}.}
#' \item {columns from a \link[base]{matrix} by `pattern` matching against its \link[base]{colnames},
#' and returns a \link[base]{matrix}.}
#' \item {objects in an \link[base]{environment} by `pattern` matching against the names of variables within,
#' and returns a **\link[base]{list}** 
#' instead of an \link[base]{environment}.}
#' }
#'  
#' 
#' @examples
#' iris |> select_rx(pattern = '\\.Width$') |> head()
#' iris |> select_rx(pattern = '\\.Length$', invert = TRUE) |> head()
#' VADeaths |> select_rx(pattern = '^Rural')
#' iris |> head() |> with(expr = select_rx(pattern = '\\.Width$'))
#' @keywords internal
#' @name op_rx
#' @export
select_rx <- function(pattern, envir = parent.frame(), ...) {
  
  nms <- env_rx(pattern = pattern, envir = envir, ...)
  
  if (is.environment(envir)) return(mget(x = nms, envir = envir))
  
  if (is.matrix(envir)) return(envir[, nms, drop = FALSE]) # if (!length(nms)) then length(return) == 0

  if (is.recursive(envir)) {
    # 'list', 'data.frame'
    # 'tibble'
    return(envir[nms]) # if (!length(nms)) then length(return) == 0
  }
  
}




#' @rdname op_rx
#' 
# @param envir \link[base]{environment}, \link[base]{list} or \link[base]{data.frame}.
# \link[base]{matrix} is not acceptable, 
# since `FUN` may have a different \link[base]{typeof} of return
#' 
# @param invert \link[base]{logical} scalar, see \link[base]{grep}
#' 
#' @param FUN \link[base]{function}
#' 
#' @param MoreArgs (optional) \link[base]{list}, additional parameters of `FUN`
#' 
#' @returns 
#' Function [apply_rx()] returns the updated `envir`, whether it is 
#' \link[base]{environment}, \link[base]{list} or \link[base]{data.frame}.
#' 
#' @examples 
#' attenu |> 
#'  head() |> 
#'  apply_rx(pattern = '^acc', FUN = stats:::format_perc, MoreArgs = list(digits = 2))
#' 
#' attenu |> 
#'  head() |> 
#'  within.data.frame(expr = {
#'   apply_rx('^acc', FUN = stats:::format_perc, MoreArgs = list(digits = 2))
#'  }) # same
#' @export
apply_rx <- function(pattern, envir = parent.frame(), ..., FUN, MoreArgs = NULL) {
  
  nms <- env_rx(pattern = pattern, envir = envir, ...)
  
  if (!length(nms)) {
    
    warning('Pattern ', sQuote(pattern), ' in ', sQuote(deparse1(substitute(envir))), ' not found')
    
  } else if (is.environment(envir)) {
    
    xs <- mget(x = nms, envir = envir, inherits = FALSE)
    vals <- do.call(what = lapply, args = c(list(X = xs, FUN = FUN), MoreArgs))
    mapply(FUN = assign, x = nms, value = vals, MoreArgs = list(envir = envir))
    
  } else if (is.recursive(envir)) {
    
    envir[nms] <- do.call(what = lapply, args = c(list(X = envir[nms], FUN = FUN), MoreArgs))
    
  } else stop(sQuote(class(envir)), ' `envir` not supported')
  
  return(envir)
  
}











env_rx <- function(pattern, envir = parent.frame(), ...) {
  
  if (!is.character(pattern) || length(pattern) != 1L || anyNA(pattern) || !nzchar(pattern)) stop('regular expression in `pattern` must be len-1 character')
  
  if (inherits(envir, what = 'data.table')) {
    stop(sQuote(class(envir)[1L]), ' not supported, yet')
  }
  
  if (is.environment(envir)) {
    # most frequently used; via ?base::within and ?base::with
    return(ls(envir = envir, all.names = TRUE, pattern = pattern, sorted = FALSE))
  }
  
  if (is.matrix(envir)) {
    return(grep(pattern = pattern, x = dimnames(envir)[[2L]], value = TRUE, ...))
  }
  
  if (is.recursive(envir)) { # ?base::is.recursive must be **after** ?base::is.environment
    # 'list', 'data.frame'
    # 'tibble'
    return(grep(pattern = pattern, x = names(envir), value = TRUE, ...))
  }
  
  stop(sQuote(class(envir)), ' `envir` not supported')
  
}




