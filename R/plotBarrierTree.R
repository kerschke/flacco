plotBarrierTree3d = function(z2, pred2, w2, root2, fe, divisions, levels,
  custom.colors) {
  orig.margins = par("mar")
  on.exit ( par(mar=orig.margins) )
  par(mar=c(0,1,0,0))
  
  # prepare colour palette
  palette = c("lightgrey", topo.colors(levels))
  # overwrite with user-defined colours, if any
  if (length(custom.colors) > 0) {
    # how many?
    overwrite = min(length(palette), length(custom.colors))
    palette[1:overwrite] = custom.colors[1:overwrite]
  }
  
  Fe = matrix(nrow=divisions[1], ncol = divisions[2])
  
  sapply(1:length(fe), FUN = function(i) {
    coord <- celltoz(i, divisions)
    if (fe[i] != Inf) {
      Fe[coord[1], coord[2]] <<- fe[i]
    } else {
      Fe[coord[1], coord[2]] <<- NA
    }
  })

  persp3d = persp(1:divisions[2], 1:divisions[1], t(Fe),
    theta=330, phi=15, border="grey",
    xlab=expression(x[2]), ylab=expression(x[1]),
    zlab=expression(y),
    col=palette[1])
  
  # draw root
  rootCoord = celltoz( root2, divisions )
  rootPoint = trans3d(
    rootCoord[2], rootCoord[1],
    fe[ root2 ], persp3d)
  points(rootPoint)
  text(rootPoint, labels = paste(root2, "(root)"), pos=1)
  
  # initialise BFS
  nextLevel = which(pred2 == root2) #children(root)
  level = 0
  drawn = 0
  while (length(nextLevel) > 0) {
    # change to next level (BFS)
    currentLevel = nextLevel
    nextLevel = c()
    level = level + 1
    
    # draw nodes of current level (BFS)
    for (i in currentLevel) {
      thisCoord = celltoz( z2[i], divisions ) # -> thisCoord[1] == cellX
      thisFe = fe[ z2[i] ]
      predCoord = celltoz( pred2[i], divisions )
      predFe = fe[ pred2[i] ]
      
      this3dPoint = trans3d(
        thisCoord[2], thisCoord[1],
        thisFe, persp3d)
      
      pred3dPoint = trans3d(
        predCoord[2], predCoord[1],
        predFe, persp3d)
      
      points(this3dPoint, col=palette[1+level])
      text(this3dPoint,
           labels = z2[i], pos=1, col=palette[1+level]) # label point
      
      lineBreak3dPoint = trans3d(
        thisCoord[2], thisCoord[1],
        predFe, persp3d)
      
      lines( rbind( # upwards
        this3dPoint, lineBreak3dPoint 
      ),
      lwd=2, col=palette[1+level] ) 
      lines( rbind( # sideways
        pred3dPoint, lineBreak3dPoint 
      ),
      lwd=2, col=palette[1+level] ) 
      
      # append children of this node to the next level (BFS)
      nextLevel = c(nextLevel,  which(pred2 == z2[i]) ) # append children(i)
      drawn = drawn + 1
    }
  }
  assert(drawn == length(z2)) # otherwise, an error has caused that some nodes weren't drawn...
}

plotBarrierTreePCA = function(z2, pred2, w2, root2, fe, divisions) {
  #Fe = matrix(nrow=divisions[1], ncol = divisions[2])
  
  coords = sapply(1:length(fe), FUN = function(i) {
    celltoz(i, divisions)
  })
  cell2fe = data.frame(cellX = coords[1, ], cellY = coords[2, ], fe = fe)
  
  principals = princomp(~ cellX + cellY, data = cell2fe)
  #print(principals$loadings[,1]) # loadings of first PC
  
  cell2fe$pca = apply(cell2fe, 1, function (row) {
    print (row)
    sum( c( row[1], row[2] ) 
         * principals$loadings[, 1] )
    }
  )
  View(cell2fe)
}
