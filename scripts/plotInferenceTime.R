d = read.csv('../scala/experiments/compareInfer2/infer.out', sep="\t", header=TRUE)
dBig = d[d$nVars > 5,]

png(file='inference.png', width=1000, height=500)
par(mfcol=c(1,2))
plot(dBig$nVars, jitter(dBig$time20rs), log="y", col='green', ylim=c(0.00001,10000), pch=1, xlab='num variables', ylab='seconds')
#plot(dBig$nVars, jitter(dBig$time20rs), col='green', ylim=c(0.00001,1000))
points(dBig$nVars, jitter(dBig$time1kBeam), col='red', pch=2)
points(dBig$nVars, jitter(dBig$timeExact), col='blue', pch=3)
points(dBig$nVars, jitter(dBig$timeBNB), col='black', pch=4)
legend('topright', c('Random Restart', 'A* Beam', 'A* Exact', 'Branch and Bound'), pch=c(1,2,3,4), col=c('green', 'red', 'blue', 'black'))

barplot(c(sum(dBig$score20rs != dBig$scoreExact), sum(dBig$score1kBeam != dBig$scoreExact)),
        names=c('Random Restart', 'A* Beam'),
        main=paste("number of search errors in ", dim(dBig)[1], " examples"))
dev.off()
