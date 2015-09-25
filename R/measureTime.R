#' Measure Runtime of a Feature Computation
#'
#' Simple wrapper around \code{proc.time}.
#' @param expr [\code{expression}]\cr
#'   Expression of which the time should be measured.
#' @param prefix [\code{character(1)}]\cr
#'   Name of the corresponding feature set. Used as a prefix for the runtime.
#' @param envir [\code{environment}]\cr
#'   Environment in which expr should be evaluated.
#' @return Returns the value(s) of the evaluated \code{expr} and adds two
#' additional attributes: the number of function evaluations
#' \code{costs_fun_evals} and the runtime \code{costs_runtime}, which was
#' required for evaluating the expression.
#' @export
measureTime = function(expr, prefix, envir = parent.frame()) {
  start = proc.time()
  feats = eval(expr, envir = envir)
  runtime = (proc.time() - start)[3]
  names(runtime) = NULL
  if (all(!grepl("costs_fun_evals", names(feats))))
    feats[[paste(prefix, "costs_fun_evals", sep = ".")]] = 0L
  feats[[paste(prefix, "costs_runtime", sep = ".")]] = runtime   
  return(feats)
}
