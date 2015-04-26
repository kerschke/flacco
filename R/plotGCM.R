# ( origins: plotgcm.m, plot2.m )

# [orig: function [pcells, nattr, punc] = plotgcm(Fm, R, N, h, lb, ub, fn, iterx, fe) {]
plotgcm = function(Fm, indexPermutation, feat.object, plot.gcm.colors) {
  orig.margins = par("mar")
  on.exit ( par(mar=orig.margins) )
  par(mar=c(3,3,3,3))
  
  divisions = feat.object$blocks
  assert(length(divisions) == 2)
  
  attractors = seq_len(ncol(Fm))
  colors = matrix(nrow=divisions[1], ncol=divisions[2])
  
  # To how many basins of attraction do cells belong?
  belongsTo = rowSums(Fm != 0)
  
  # cells belonging to more than one basin of attraction => col #1
  colors[ indexPermutation[belongsTo > 1] ] = 1
  
  # cells belonging to exactly one basin of attraction (but which one?)
  #  => col #2+i
  sapply( which(belongsTo == 1), FUN = function(index) {
    # The one column which has a value > 0 indicates the attractor
    # (=> column ID = attractor ID)
    # Assign the first column to colour #3, second to #4, and so on.
    colors[ indexPermutation[index] ] <<- 2 + which( Fm[index, ] > 0 )
  })
  
  # attractors (those cells which belong to themselves) => col #2
  colors[ indexPermutation[attractors] ] = 2
  
  arrows = NULL  # will be a matrix with 4 rows and one column per arrow
  for(toID in attractors) {
    basincoord = celltoz( indexPermutation[toID], divisions )
    
    attracted = which(Fm[, toID] != 0)
    # remove itself (would result in vector of length 0, which raises warnings)
    attracted = attracted[-(attracted == toID)]
    if(length(attracted) == 0) {
      next;
    }
    
    arrows.toID = sapply (attracted, FUN=function(fromID) {
      fromcoord = celltoz( indexPermutation[fromID], divisions )
      
      components = normalizeVector( basincoord-fromcoord )
      components.weighted = components * Fm[fromID, toID]
      
      return(
        c(fromcoord,
          components.weighted)
        )
    })
    
    if (is.null(arrows)) {
      arrows = arrows.toID
    } else {
      arrows = cbind(arrows, arrows.toID)
    }
  }
  rownames(arrows) = c("from.x", "from.y", "component.x", "component.y")
  
  # prepare colour palette
  palette = c("#cccccc","#333333", topo.colors(length(attractors)))
  # overwrite with user-defined colours, if any
  if (length(plot.gcm.colors) > 0) {
    # how many?
    overwrite = min(length(palette), length(plot.gcm.colors))
    palette[1:overwrite] = plot.gcm.colors[1:overwrite]
  }
  
  # cell information
  image(seq_len(divisions[2]), seq_len(divisions[1]), t(colors), useRaster = TRUE, 
        col=palette,
        breaks=seq(0.5, length(attractors) + 2.5), # force `image` to consider `color` as a matrix of discrete values (otherwise, will try to use the full range of colors given)
        xlab="", ylab="", asp=1,
        xlim=c(0.5, divisions[2] + 0.5),
        ylim=c(0.5, divisions[1] + 0.5 ) )
  
  # grid
  abline(v=seq(0.5,divisions[2] + 0.5), col="#333333",
         xlim=c(0.5, divisions[2] + 0.5),
         ylim=c(0.5, divisions[1] + 0.5 ))
  abline(h=seq(0.5,divisions[1] + 0.5), col="#333333",
         xlim=c(0.5, divisions[2] + 0.5),
         ylim=c(0.5, divisions[1] + 0.5 ))
  
  # attraction
  apply(arrows, 2, FUN = function(arr) {
    arr.length = sqrt( arr[3]^2 + arr[4]^2 )
    shape::Arrows(arr[2],                arr[1], 
                  arr[2] + arr[4] * 0.9, arr[1] + arr[3] * 0.9,
                  arr.length = arr.length * 0.1, 
                  arr.width  = arr.length * 0.1,
                  arr.type = "triangle")
  })
  
  # additional axes that represent values of original feature dimensions
  mtext(side = 1, "Cell coordinate, dimension 2", line = 2, cex=par("cex"))
  mtext(side = 2, "Cell coordinate, dimension 1", line = 2, cex=par("cex"))
  axis(3, seq_len(divisions[2]), unique(feat.object$cell.centers[[2]]) ) # above; feature x2
  mtext(side = 3, expression(x[2]), line = 2, cex=par("cex")) 
  axis(4, seq_len(divisions[1]), unique(feat.object$cell.centers[[1]]) ) # right; feature x1
  mtext(side = 4, expression(x[1]), line = 2, cex=par("cex"))
}
