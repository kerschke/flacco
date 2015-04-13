plot_ic <- function (epsilon, calculate.partial.ic, H, M, Hmax, epsilonS, Mzero, epsilon05) {

  plot(log10(epsilon), H, type="l", xlab=expression(log[10](epsilon)))
  
  #Hmax
  points(log10(epsilon[which.max(H)]), Hmax, pch=1)
  abline(v=log10(epsilon[which.max(H)]), lty = 3)
  
  #epsilonS
  points(epsilonS, 0, pch=0)
  
  if (calculate.partial.ic) {
    lines(log10(epsilon), M, lty=2)
    
    #Mzero
    points(log10(epsilon[2]), Mzero, pch=5) # (because log10(0) = -Inf)
    
    #epsilon05
    points(epsilon05, 0, pch=6)
    abline(v=epsilon05, lty = 3)
    abline(h=M[ max( which(M > 0.5*Mzero) ) ], lty = 3)
  }
  
  # -------------------------
  # draw legend
  
  #lines
  legend.descr  = c(expression(H(epsilon)))
  legend.points = c(NA)
  legend.lines  = c(1)
  
  if (calculate.partial.ic) {
    legend.descr  = c(legend.descr,  expression(M(epsilon)))
    legend.points = c(legend.points, NA)
    legend.lines  = c(legend.lines,  2)
  }
  
  #points
  legend.descr  = c(legend.descr,  expression(H[max]), expression(epsilon[s]))
  legend.points = c(legend.points, 1, 0)
  legend.lines  = c(legend.lines,  NA, NA)
  
  if (calculate.partial.ic) {
    legend.descr  = c(legend.descr,  expression(M[0]), expression(epsilon[0.5]))
    legend.points = c(legend.points, 5, 6)
    legend.lines  = c(legend.lines,  NA, NA)
  }
  
  
  legend("topright",
         legend = legend.descr,
         pch    = legend.points,
         lty    = legend.lines)
}