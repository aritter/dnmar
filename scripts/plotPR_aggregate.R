#png('PR.png', width=1500, height=2000)
#pdf('PR_aggregate.pdf', width=20, height=15)
#pdf('PR_aggregate.pdf', width=7, height=7)
pdf('PR_aggregate.pdf', width=5, height=5)
#files = list.files('../scala/experiments/dnmar_sent')
files = list.files('../scala/experiments/DNMAR')
#files = list.files('../scala/experiments_test/DNMAR_agg_normalized')
#par(mfcol=c(3,4))
#par(mfcol=c(6,4))
for(f in files) {
  #if(f != 'log' && f != 'sentential' && f != 'aggregate') {
  if(f != 'log' && f == 'aggregate') {
    print(f)
    #p1 = read.csv(paste('../scala/experiments/dnmar_sent/',f,sep=""), sep="\t", header=FALSE)
    file = paste('../scala/experiments/DNMAR/',f,sep="")
    #file = paste('../scala/experiments_test/DNMAR_agg_normalized/',f,sep="")
    print(file)
    if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
      p1 = read.csv(file, sep="\t", header=FALSE)
    } else {
      p1 = data.frame(p=numeric(0), r=numeric(0))
    }
    #p2 = read.csv(paste('../scala/experiments/multir_sent/',f,sep=""), sep="\t", header=FALSE)
    file = paste('../scala/experiments/MultiR/',f,sep="")
    if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
      p2 = read.csv(file, sep="\t", header=FALSE)
    } else {
      p2 = data.frame(p=numeric(0), r=numeric(0))
    }
    file = paste('../scala/experiments/DNMAR_fb/',f,sep="")
    if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
      p3 = read.csv(file, sep="\t", header=FALSE)
    } else {
      p3 = data.frame(p=numeric(0), r=numeric(0))
    }    

    if(dim(p1)[1] > 20 && (sum(p1[,1] > 0) || sum(p2[,1] > 0))) {
      #plot(p1[,2], p1[,1], xlim=c(0,0.9), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
      if(f == 'aggregate') {
        plot(p1[,2], p1[,1], xlim=c(0,0.35), ylim=c(0,1), xlab='recall', ylab='precision', type='l', lwd=2)
      } else {
        plot(p1[,2], p1[,1], xlim=c(0,1), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
      }
      #points(p2[,2], p2[,1], col='red', pch=2)
      #points(p3[,2], p3[,1], col='brown', pch=3)
      lines(p2[,2], p2[,1], lty=2, lwd=2, col='red')
      lines(p3[,2], p3[,1], lty=3, lwd=2, col='brown')
    }
  }
}

legend('topright', c('MultiR', 'DNMAR', 'DNMAR*'), lty=c(2,1,3), col=c('red', 'black', 'brown'), lwd=c(2,2,2))
#legend('topright', c('MultiR', 'DNMAR', 'DNMAR*'), pch=c(2,1,3), col=c('red', 'black', 'brown'))
dev.off()
