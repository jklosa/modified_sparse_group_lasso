#!/usr/bin/Rscript

initial_dir = getwd()
setwd("C:/Users/klosa/Desktop/Seattle_Documentation/5.2-Modified sparse-group lasso")

pdf("modified_sg_lasso_figures_R.pdf", width=12, height=8)

#******************************************************************************
#*  Analysis tools.                                                           *
#******************************************************************************
beta_sim = as.matrix(read.table("beta_sim.txt", header = FALSE, sep = " ", dec = "."))
beta     = as.matrix(read.table("modified_sg_lasso_output_R_beta.txt", header = FALSE, dec = "."))

num_intervals = dim(beta)[1]
p = dim(beta)[2]

for (i in 1:(num_intervals-1)) {
  if (i == 1) {
    max_val = max(beta_sim, beta)
    min_val = min(beta_sim, beta)
    plot(seq(1,p,1), beta_sim, xlim = c(1,p+0.6), ylim=c(min_val, max_val), type='h', col='blue',
         xlab='index of feature', ylab=expression(paste(beta)), axes=FALSE)
    box()
    points(seq(1,p,1), beta_sim, pch=16, col='blue')
    
    abline(h=0, lty=2)
    
    axis(1, at=seq(1,p,1), labels=TRUE, tick=TRUE)
    axis(2, labels=TRUE, tick=TRUE)
    legend('topright', 
           c('simulated', 'modified sparse-group lasso (iterates)', 'modified sparse-group lasso (final)'), 
           col=c('blue', 'gray60', 'black'), 
           pch=c(16, 16, 16))
  }
  
  shift = (num_intervals - i)/(num_intervals-1)*0.2 + (i - 1)/(num_intervals-1)*0.8
  points(seq(1,p,1)+shift, beta[i,], pch=16, col='gray60')
}
shift = 0.8
points(seq(1,p,1)+shift, beta[num_intervals,], type='h', col='black')
points(seq(1,p,1)+shift, beta[num_intervals,], pch=16, col='black')
dev.off()

setwd(initial_dir)
