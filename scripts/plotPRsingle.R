p1 = read.csv('../scala/experiments/dnmar_random20/sentential', sep="\t", header=FALSE)
p2 = read.csv('../scala/experiments/multir/sentential', sep="\t", header=FALSE)
plot(p1[,2], p1[,1], xlim=c(0,0.8), ylim=c(0,1), main='test')
points(p2[,2], p2[,1], col='red')
