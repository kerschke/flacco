# Bundle functions that are common to the initialisation of GCM and Barrier Tree features
# (and stricly only used there).
# gcm_init, which is calling create and canform. Canform, in turn, calls commclasses.

gcm_init <- function(feat.object) {
  # option 1 => min
  # check whether matrices are already cached (otherwise, calculate them)
  if (is.null(feat.object$env$gcm.representatives$min) ||
        is.null(feat.object$env$gcm.canonicalForm$min)) {
    #compute mappings and probability
    sparse1 <- create(feat.object, "min")
    # cache values within $env (to avoid that R creates a copy if feat.object is modified directly)
    feat.object$env$gcm.representatives$min = sparse1$fe
    
    #Compute canonical form of matrix A
    feat.object$env$gcm.canonicalForm$min   = canform(
      fullsparse(sparse1$fromcell, sparse1$tocell, sparse1$probabilities) ) # (A1)
  }
  
  # option 2 => mean
  # check whether matrices are already cached (otherwise, calculate them)
  if (is.null(feat.object$env$gcm.representatives$mean) ||
        is.null(feat.object$env$gcm.canonicalForm$mean)) {
    #compute mappings and probability
    sparse2 <- create(feat.object, "mean")
    # cache values within $env (to avoid that R creates a copy if feat.object is modified directly)
    feat.object$env$gcm.representatives$mean = sparse2$fe
    
    #Compute canonical form of matrix A
    feat.object$env$gcm.canonicalForm$mean   = canform( 
      fullsparse(sparse2$fromcell, sparse2$tocell, sparse2$probabilities) ) #(A2)
  }
  
  # option 3 => near
  # check whether matrices are already cached (otherwise, calculate them)
  if (is.null(feat.object$env$gcm.representatives$near) ||
        is.null(feat.object$env$gcm.canonicalForm$near)) {
    #compute mappings and probability
    sparse3 <- create(feat.object, "near")
    # cache values within $env (to avoid that R creates a copy if feat.object is modified directly)
    feat.object$env$gcm.representatives$near = sparse3$fe
    
    #Compute canonical form of matrix A
    feat.object$env$gcm.canonicalForm$near   = canform( 
      fullsparse(sparse3$fromcell, sparse3$tocell, sparse3$probabilities) ) #(A2)
  }
}

# return list( fromcell = fromcell, tocell = tocell, probabilities = probabilities, fe = fe);
create <- function (feat.object, approach) {
  assertClass(feat.object, "FeatureObject")
  assertChoice(approach, c("min", "mean", "near"))
  
  var = extractFeatures(feat.object)
  fun = extractObjective(feat.object)
  

  if (approach == "min") { 
    fe <- sapply(seq_len( prod(feat.object$blocks) ), function (i) {
      if (length(which(feat.object$init.grid$cell.ID == i)) == 0) {
        return(Inf)
      }
      min(feat.object$init.grid$y[feat.object$init.grid$cell.ID == i])
    })
    
  } else if(approach == "mean") {
    fe <- sapply(seq_len( prod(feat.object$blocks) ), function (i) {
      if (length(which(feat.object$init.grid$cell.ID == i)) == 0) {
        return(Inf)
      }
      mean(feat.object$init.grid$y[feat.object$init.grid$cell.ID == i])
    })
    
  } else { # approach == near
    nearest = findNearestPrototype(feat.object)
    fe = vector()
    fe[nearest$represented.cell] = nearest[[feat.object$objective.name]]
    
  }
  
  fromcell <- c() # fromcell
  tocell <- c() # tocell
  
  probability <- na.omit( unlist(
    lapply(seq_len( prod(feat.object$blocks) ), FUN=function(i) { # length of valuesPerCell is prod(divisions) by definition.
      if ( fe[i] == Inf ) {
        return (NA)
      }
      
      
      noBetterCells <- TRUE 
      coordinate <- celltoz(i, feat.object$blocks)
      
      #Find all neighbours of a cell
      neighbours <- findAllNeighbours( rep(1, length(feat.object$blocks)) * feat.object$cell.size[1], coordinate, feat.object$cell.size )
      
      #Discard those neighbours that are outside the bounds or the cell itself
      discard <- apply(neighbours, 1, FUN= function(neighbour) {
        isNeighbourInvalid(coordinate, neighbour, feat.object$blocks)
      })
      
      neighbours <- neighbours[!discard,]
      
      
      #Select the neighbours that are better according to the objective function
      apply(neighbours, 1, function (row) {
        neighbourIndex <- ztocell(row, feat.object$blocks)            
        if (fe[neighbourIndex] <= fe[i]) {
          fromcell <<- c(fromcell, i)
          tocell <<- c(tocell, neighbourIndex)
          noBetterCells <<- FALSE
        }
      })
      
      #if all the neighbours have the same function evaluation as the current cell
      #include the starting cells
      #TODO the comment above does NOT describe what's done here!!
      neighbourFe <- fe[ztocell( neighbours[1,], feat.object$blocks)]
      currentFe <- fe[i] # deviation! bug in original code. [ orig:  q2 = fe(cells); ]
      if (!noBetterCells && neighbourFe == currentFe) {
        fromcell <<- c(fromcell, i)
        tocell <<- c(tocell, i)
      }
      
      #compute the set of neighbours that are better than the current
      #cells (gneighbours)
      gneighbours <- tocell[which(fromcell == i)]
      
      
      #Compute the probability to move to those cells
      if (length(gneighbours) > 0) {
        pi <- rep(1/length(gneighbours), length(gneighbours))  #%(fe(gneighbours(j)) - fe(i))/ev; %
      }
      
      #If there are no better cells, then stay with probability 1
      if (noBetterCells) {
        fromcell <<- c(fromcell, i) # isn't that done in the section above already?!
        tocell <<- c(tocell, i)
        pi <- 1
      }
      return(pi)
    })
    , use.names=FALSE))
  
  return (list( fromcell = fromcell, tocell = tocell,
                probabilities = probability, fe = fe))
}

# (origin: canform.m)
# Obtains the canonical form canonicalMatrix of a stochastic matric stochasicMatrix
# permutation contains the permutation of indices
# sumV is the number of closed classes
canform <- function(stochasticMatrix) { # [ orig: function [Q p sv]=canform(P) ]
  
  res <- commclasses(stochasticMatrix)
  commClass = res$commClass
  closedClass = res$closedClass
  
  
  sumV = sum(closedClass)
  closedIndices = which(closedClass == 1) # indices in  comprise union of closed classes [formerly u]
  openIndices   = which(closedClass == 0) # [formerly w]
  
  #compute set of representatives of closed classes
  representatives <- c()
  while ( length(closedIndices) > 0 ) {
    representatives <- c(representatives, closedIndices[1])
    # remove closedIndices[1] from closedClass
    closedClass   <- closedClass & (commClass[closedIndices[1],] == 0)
    closedIndices <- which( closedClass==1 )
  }
  #representatives is now the set of representatives of closed classes
  
  #Each closed class has a unique representative in representative.
  permutation <- c()
  for (i in 1:length(representatives)) {
    a <- which( commClass[representatives[i], ] )
    permutation <- c(permutation, a)
  }
  permutation <- c(permutation, openIndices)
  #We have now a permutation p of indices, permutation, that
  #gives the new stochastic matrix Q.
  canonicalMatrix <- stochasticMatrix[permutation, permutation]
  
  return (list(
    canonicalForm = canonicalMatrix, indexPermutation = permutation, closedClassIndex = sumV
  ))
}

# (origin: commclasses.m)
#Input - P is a stochastic matrix
#Output - C is a logical matrix
# - commClass[i,j] is TRUE if and only if j is in the
#   communicating class of i.
# - closedClass is a logical  vector. closedClass[i]=1 iff
#   the class commClass[i] is closed
commclasses <- function(stochasticMatrix) { # [ orig: function [C,v]=commclasses(P) ]
  m <- nrow(stochasticMatrix)
  classes <- matrix(FALSE, m,m);
  
  for (i in 1:m) {
    matrixRows <- c(i)
    classesRow <- rep(0, m) # formerly: b
    classesRow[i] <- 1
    oldVal <- 1
    newVal <- 0
    # calculate which rows are in communicating class of i
    # (not really. actual classes are checked by logical combination of T and t(T) later on.)
    while (oldVal != newVal) {
      oldVal <- sum( which(classesRow > 0) )
      #n <- ncol(a) (not needed)
      sums <- if (length(matrixRows) == 1) 
        stochasticMatrix[matrixRows,] else 
          colSums(stochasticMatrix[matrixRows,])
      
      d <- which( sums > 0 )
      classesRow[d] <- 1
      
      newVal <- sum( which(classesRow > 0) )
      matrixRows <- d
    }
    classes[i,] <- (classesRow == TRUE)
    
  }
  
  commClass   <- classes & t(classes)
  closedClass <- ( colSums(  t(commClass)==t(classes)  ) == m )
  
  return(list(commClass = commClass, closedClass = closedClass))
}