pdf('PR_noSA.pdf', width=9, height=12)

#files = list.files('../scala/experiments/dnmar_sent')
files = list.files('../scala/experiments/DNMAR')
#par(mfcol=c(3,4))
par(mfcol=c(4,3))
#par(mfcol=c(6,4))
for(f in files) {
  #if(f != 'log' && f != 'sentential' && f != 'aggregate') {
  if(f != 'log' & f != 'sentential' & f != 'aggregate') {
    print(f)
    #p1 = read.csv(paste('../scala/experiments/dnmar_sent/',f,sep=""), sep="\t", header=FALSE)
    file = paste('../scala/experiments/DNMAR/',f,sep="")
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

    file = paste('../scala/experiments/MultiR_XU/',f,sep="")
    if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
        p4 = read.csv(file, sep="\t", header=FALSE)
    } else {
        p4 = data.frame(p=numeric(0), r=numeric(0))
    }
    
    if(dim(p1)[1] > 20 && (sum(p1[,1] > 0) || sum(p2[,1] > 0))) {
      #plot(p1[,2], p1[,1], xlim=c(0,0.9), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
      if(f == 'aggregate') {
        plot(p1[,2], p1[,1], xlim=c(0,0.3), ylim=c(0.15,1), main=sub('sentential__', '', f), xlab='recall', ylab='precision')
      } else {
        #plot(p1[,2], p1[,1], xlim=c(0,1), ylim=c(0,1), main=sub('sentential__', '', f), xlab='recall', ylab='precision')
        plot(p1[,2], p1[,1], xlim=c(0,1), ylim=c(0,1), main=sub('sentential__', '', f), xlab='recall', ylab='precision', type='l', lwd=2)
      }
      #points(p2[,2], p2[,1], col='red', pch=2)
      #points(p3[,2], p3[,1], col='brown', pch=3)
      lines(p2[,2], p2[,1], lty=2, lwd=2, col='red')
      lines(p3[,2], p3[,1], lty=3, lwd=2, col='brown')
      lines(p4[,2], p4[,1], lty=4, lwd=2, col='blue')
    }
  }
}
dev.off()
