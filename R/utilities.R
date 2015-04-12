control_parameter = function(control, name, default) {
  v = control[[name]]
  if (is.null(v))
    return(default)
  else
    return(v)
}

## wraps a evaluation counter around the function
initializeCounter = function(fn) {
  force(fn)
  count = 0L
  structure(function(x, ...) {
    count <<- count + if (is.matrix(x)) 
      ncol(x)
    else 1L
    fn(x, ...)
  })
}

## shows the number of function evaluations / calls
showEvals = function(fn) {
  environment(fn)$count
}

## resets the evaluation counter to 0 and returns the number of calls
resetCounter = function (fn) {
  counts = environment(fn)$count
  last_count = environment(fn)$count
  environment(fn)$count = 0L
  counts
}

## helper, which returns the minimum of a vector
selectMin = function(x, tie.breaker = "sample") {
  i = which(x == min(x))
  if (length(i) > 1L) {
    if (tie.breaker == "first") {
      return(i[1L])
    } else if (tie.breaker == "last") {
      return(i[length(i)])
    } else if (tie.breaker == "sample") {
      return(sample(i, 1L))
    }
  }
  return(i)
}

## helper, which returns the maximum of a vector
selectMax = function(x, tie.breaker = "sample") {
  i = which(x == max(x))
  if (length(i) > 1L) {
    if (tie.breaker == "first") {
      return(i[1L])
    } else if (tie.breaker == "last") {
      return(i[length(i)])
    } else if (tie.breaker == "sample") {
      return(sample(i, 1L))
    }
  }
  return(i)
}

normalizeVector = function(x) {
  x / sqrt(crossprod(x, x))
}

extractFeatures = function(feat.object) {
  as.matrix(subset(feat.object$env$init, select = feat.object$feature.names))
}

extractObjective = function(feat.object) {
  feat.object$env$init[, feat.object$objective.name]
}

extractInit = function(feat.object) {
  feat.object$env$init
}


# enable returning tuples without the need for a temporary value
# it is used like this:
#list[QR,,QRaux]  <- qr(c(1,1:3,3:1))
#list[,Green,Blue]  <- col2rgb("aquamarine")
# by Gabor Grothendieck, obtained from: https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}




#imitates matlab full(sparse(i,j,s)) function for the special case of square matrices.
# creates an mxm (square) matrix A, where m = max(c(i,j))
# and a[i[k], j[k]] = s[k]
fullsparse <- function (i, j, s) {
  if (length(i) != length(j) 
      || length(j) != length(s)
      || length(i) != length(s))  {
    warning(paste('lengths of vectors must be equal', length(i), length(j), length(s)))
  }
  A <- matrix(0,nrow=max(c(i,j)), ncol=max(c(i,j)))
  for (elem in 1:length(i)) {
    A[ i[elem], j[elem] ] = s[elem]
  }
  return (A)
}

# returns the number of non-zero elements of vector
nnz <- function(vector) {
  length( which(vector != 0) )
}
