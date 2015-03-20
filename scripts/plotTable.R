#png('PR.png', width=1500, height=2000)
#pdf('PR.pdf', width=20, height=15)
#files = list.files('../scala/experiments/dnmar_sent')
files = list.files('../scala/experiments/DNMAR')
#par(mfcol=c(3,4))
#par(mfcol=c(6,4))

table = c()
for(f in files) {
  #if(f != 'log' && f != 'sentential' && f != 'aggregate') {
  if(f != 'log' & f != 'sentential' & f != 'aggregate') {
    #print(f)
    file = paste('../scala/experiments/DNMAR/',f,sep="")
    #print(file)
    if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
      p1 = read.csv(file, sep="\t", header=FALSE)
      p1 = p1[p1[,2] > 0,]
    } else {
      p1 = data.frame(p=numeric(0), r=numeric(0))
      p1 = p1[p1[,2] > 0,]
    }
    file = paste('../scala/experiments/MultiR/',f,sep="")
    if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
      p2 = read.csv(file, sep="\t", header=FALSE)
      p2 = p2[p2[,2] > 0,]
    } else {
      p2 = data.frame(p=numeric(0), r=numeric(0))
      p2 = p2[p2[,2] > 0,]
    }
    file = paste('../scala/experiments/DNMAR_fb/',f,sep="")
    if(file.exists(file) && !file.info(file)$isdir && file.info(file)$size > 0) {
      p3 = read.csv(file, sep="\t", header=FALSE)
      p3 = p3[p3[,2] > 0,]
    } else {
      p3 = data.frame(p=numeric(0), r=numeric(0))
      p3 = p3[p3[,2] > 0,]
    }    

    if(dim(p1)[1] > 20 && (sum(p1[,1] > 0) || sum(p2[,1] > 0))) {
      beta = 1.0
      
      f1 = (1 + beta^2) * p1[,1] * p1[,2] / (beta^2 * p1[,1] + p1[,2])
      f2 = (1 + beta^2) * p2[,1] * p2[,2] / (beta^2 * p2[,1] + p2[,2])
      f3 = (1 + beta^2) * p3[,1] * p3[,2] / (beta^2 * p3[,1] + p3[,2])

      #print(p1[,1:2])
      #print(f1)

      #barplot(max(f2, rm.na=TRUE), max(f1), max(f3))
      print(paste(f, max(f2), max(f1), max(f3), sep=" & "))
    }
  }
}
