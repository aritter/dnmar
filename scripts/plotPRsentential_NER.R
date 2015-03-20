nAUC <- function(p, r) {
  sum = 0.0
  for(i in 1:(length(p)-1)) {
    sum = sum + (r[i+1] - r[i]) * p[i]
    #print((r[i+1] - r[i]) * p[i])
  }
  sum
}

pdf('PR_sentential_NER.pdf', width=5, height=5)

f = 'sentential'
file = paste('../scala/experiments/NER_DNMAR/',f,sep="")
if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
  p1 = read.csv(file, sep="\t", header=FALSE, quote="")
} else {
  p1 = data.frame(p=numeric(0), r=numeric(0))
}
samp = sort(unique(c(seq(100), sample(seq(dim(p1)[1]), 1000))))
p1 = p1[samp,]
                                        #p2 = read.csv(paste('../scala/experiments/multir_sent/',f,sep=""), sep="\t", header=FALSE)
file = paste('../scala/experiments/NER_MultiR/',f,sep="")
if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
  p2 = read.csv(file, sep="\t", header=FALSE, quote="")
} else {
  p2 = data.frame(p=numeric(0), r=numeric(0))
}
samp = sort(unique(c(seq(100), sample(seq(dim(p2)[1]), 1000))))
p2 = p2[samp,]

file = paste('../scala/experiments/NER_DNMAR_fb/',f,sep="")
if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
  p3 = read.csv(file, sep="\t", header=FALSE, quote="")
} else {
  p3 = data.frame(p=numeric(0), r=numeric(0))
}
samp = sort(unique(c(seq(100), sample(seq(dim(p3)[1]), 1000))))
p3 = p3[samp,]

if(dim(p1)[1] > 20 && (sum(p1[,1] > 0) || sum(p2[,1] > 0))) {
                                        #plot(p1[,2], p1[,1], xlim=c(0,0.9), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
  if(f == 'aggregate') {
    plot(p1[,2], p1[,1], xlim=c(0,0.3), ylim=c(0.15,1), xlab='recall', ylab='precision')
  } else {
    #plot(p1[,2], p1[,1], xlim=c(0,1), ylim=c(0,1), xlab='recall', ylab='precision')
    plot(p1[,2], p1[,1], xlim=c(0,1), ylim=c(0,1), xlab='recall', ylab='precision', type='l', lwd=2)
  }
  #points(p2[,2], p2[,1], col='red', pch=2)
  #points(p3[,2], p3[,1], col='brown', pch=3)
  lines(p2[,2], p2[,1], lty=2, lwd=2, col='red')
}

#legend('bottomleft', c('NER_MultiR', 'NER_DNMAR', 'NER_DNMAR*'), pch=c(2,1,3), col=c('red', 'black', 'brown'))
#legend('topright', c('NER_MultiR', 'NER_DNMAR', 'NER_DNMAR*'), pch=c(2,1,3), col=c('red', 'black', 'brown'))
#legend('topright', c('NER_MultiR', 'NER_DNMAR'), pch=c(2,1), col=c('red', 'black'))
legend('topright', c('NER_MultiR', 'NER_DNMAR'), lty=c(2,1), lwd=c(3,3), col=c('red', 'black'))

dev.off()

print('mu')
muauc = AUC(p2[,1], p2[,2])
print(muauc)
print('dn')
dnauc = AUC(p1[,1], p1[,2])
print(dnauc)
print((dnauc - muauc) / muauc)
