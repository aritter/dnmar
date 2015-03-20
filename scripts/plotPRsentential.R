AUC <- function(p, r) {
  sum = 0.0
  for(i in 1:(length(p)-1)) {
    sum = sum + (r[i+1] - r[i]) * p[i]
    #print((r[i+1] - r[i]) * p[i])
  }
  sum
}

#pdf('PR_sentential_miml.pdf', width=7, height=7)
#pdf('PR_sentential_miml.pdf', width=5, height=5)
pdf('PR_sentential.pdf', width=5, height=5)

f = 'sentential'
file = paste('../scala/experiments/DNMAR/',f,sep="")
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

file = paste('../scala/experiments_test/MIML/',f,sep="")
p5 = read.csv(file, sep="\t", header=FALSE)

file = paste('../scala/experiments/DNMAR_XU/',f,sep="")
if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
  p6 = read.csv(file, sep="\t", header=FALSE)
} else {
  p6 = data.frame(p=numeric(0), r=numeric(0))
}

if(dim(p1)[1] > 20 && (sum(p1[,1] > 0) || sum(p2[,1] > 0))) {
                                        #plot(p1[,2], p1[,1], xlim=c(0,0.9), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
  if(f == 'aggregate') {
    plot(p1[,2], p1[,1], xlim=c(0,0.3), ylim=c(0.15,1), xlab='recall', ylab='precision')
  } else {
    plot(p1[,2], p1[,1], xlim=c(0,1), ylim=c(0,1), xlab='recall', ylab='precision', type='l', lwd=2)
  }

  #points(p2[,2], p2[,1], col='red', pch=2)
  #points(p3[,2], p3[,1], col='brown', pch=3)
  #points(p4[,2], p4[,1], col='blue', pch=4)
  lines(p2[,2], p2[,1], lty=2, lwd=2, col='red')
  lines(p3[,2], p3[,1], lty=3, lwd=2, col='brown')
  lines(p4[,2], p4[,1], lty=4, lwd=2, col='blue')
  #lines(p5[,2], p5[,1], lty=5, lwd=2, col='brown')
  #lines(p6[,2], p6[,1], lty=5, lwd=2, col='magenta')
}

#legend('bottomleft', c('MultiR', 'DNMAR', 'DNMAR*', 'MIML'), pch=c(2,1,3,4), col=c('red', 'black', 'brown', 'blue'))
#legend('bottomleft', c('MultiR', 'DNMAR', 'DNMAR*'), pch=c(2,1,3,4), col=c('red', 'black', 'brown'))
#legend('bottomleft', c('MultiR', 'DNMAR', 'DNMAR*'), lty=c(2,1,3), lwd=c(3,3,3))
#legend('bottomleft', c('MultiR', 'Xu13', 'DNMAR', 'MIML', 'DNMAR*'), lty=c(2,4,1,5,3), col=c('red', 'blue', 'black', 'brown', 'brown'), lwd=c(2,2,2,2,2))
legend('bottomleft', c('MultiR', 'Xu13', 'DNMAR', 'DNMAR*'), lty=c(2,4,1,3), col=c('red', 'blue', 'black', 'brown'), lwd=c(2,2,2,2))

print('mu')
muauc = AUC(p2[,1], p2[,2])
print(muauc)

print('dn')
dnauc = AUC(p1[,1], p1[,2])
print(dnauc)
print((dnauc - muauc) / muauc)

print('dnfb')
dnfbauc = AUC(p3[,1], p3[,2])
print(dnfbauc)
print((dnfbauc - muauc) / muauc)

print('xu')
xuauc = AUC(p4[,1], p4[,2])
print(xuauc)
print((dnfbauc - xuauc) / xuauc)

print('miml')
mimlauc = AUC(p5[,1], p5[,2])
print(mimlauc)
print((dnfbauc - mimlauc) / mimlauc)

dev.off()
