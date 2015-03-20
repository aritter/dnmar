#png('PR.png', width=1500, height=2000)
pdf('PR.pdf', width=20, height=15)
#files = list.files('../scala/experiments/dnmar_sent')
files = list.files('../scala/experiments/DNMAR')
par(mfcol=c(3,4))
#par(mfcol=c(6,4))
for(f in files) {
  #if(f != 'log' && f != 'sentential' && f != 'aggregate') {
  if(f != 'log') {
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

    if(dim(p1)[1] > 20 && (sum(p1[,1] > 0) || sum(p2[,1] > 0))) {
      #plot(p1[,2], p1[,1], xlim=c(0,0.9), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
      if(f == 'aggregate') {
        plot(p1[,2], p1[,1], xlim=c(0,0.3), ylim=c(0.15,1), main=f, xlab='recall', ylab='precision')
      } else {
        plot(p1[,2], p1[,1], xlim=c(0,1), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
      }
      points(p2[,2], p2[,1], col='red', pch=2)
      points(p3[,2], p3[,1], col='green', pch=3)
    }
  }
}
dev.off()
