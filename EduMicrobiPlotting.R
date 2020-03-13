getwd()
myfile = read.csv(file.choose(), header=T, sep=",", dec=".")
#myfile = read.csv("EduMicrobialEnvironmentSE.csv", header=T, sep=",")

View(myfile)
str(myfile)
table(myfile$Group2)
table(myfile$oxotol)
table(myfile$temprange)

#"UGLY" PLOTS
barplot(table(myfile$Group2))

barplot(table(myfile$temprange))

hist(myfile$temp_opt.C.)
plot(density(myfile$temp_opt.C.))

boxplot(myfile$temp_opt.C.~myfile$oxotol)

plot(x=myfile$temp_opt.C.,y=myfile$Q)
plot(x=myfile$temp_opt.C.,y=log10(myfile$Genome_size))
plot(x=(myfile$bG+myfile$bC),y=myfile$A)


#NICE PLOTS
par(mar=(c(bottom=4,left=10,top=4, right=4)))
barplot(table(myfile$Group2),main="Taxonomic distribution",
        xlab="Domain",ylab="Occurence",
        col=rainbow(2), cex.lab=2, cex.axis=1.5, cex.names=2)

par(mar=(c(bottom=4,left=5,top=4, right=4)))
barplot(table(myfile$temprange),main="Temperature range",
        xlab="",ylab="Occurence",
        col=c("red","green","blue", "orange"),
        cex.lab=2, cex.axis=1.5, cex.names=2)

barplot(table(myfile$oxotol),main="Oxygen tolerance",
        xlab="",ylab="Occurence",
        col=rainbow(length(names(table(myfile$oxotol)))),
        cex.lab=2, cex.axis=1.5, cex.names=1.5)


par(mar=(c(bottom=4,left=5,top=4, right=4)))
hist(myfile$temp_opt.C.,main="Histogram of temperature optimum",
        xlab="Temperature(°C)",ylab="Occurence",
        col=c("red"),
        cex.lab=2, cex.axis=1.5, ylim=c(0,200))

par(mar=(c(bottom=4,left=5,top=4, right=4)))
hist(myfile$pH_opt,main="pH optimum",
     xlab="ph",ylab="Occurence",
     col=c("green"),
     cex.lab=2, cex.axis=1.5, ylim=c(0,200))

par(mar=(c(bottom=5,left=5,top=4, right=4)))
hist(myfile$pH_opt,main="Salt conc. optimum",
     xlab="NaCl conc.[(w/V)%]",ylab="Occurence",
     col=c("blue"),
     cex.lab=2, cex.axis=1.5, ylim=c(0,200))


boxplot(myfile$temp_opt.C.~myfile$temprange,
        ylab="Temperature(°C)",
        col=c("red","green","blue", "orange"),
        cex.lab=2, cex.axis=1.5, cex.names=2)


boxplot(myfile$temp_opt.C.~myfile$Group2,
        xlab="Domain",ylab="Temperature(°C)",
        col=rainbow(2), cex.lab=2, cex.axis=1.5, cex.names=2)

boxplot(myfile$temp_opt.C.~myfile$oxotol,
        xlab="Domain",ylab="Temperature(°C)",
        col=rainbow(length(names(table(myfile$oxotol)))),
        cex.lab=2, cex.axis=1.5, cex.names=2)

par(mar=(c(bottom=5,left=5,top=5, right=5)))
plot(x=myfile$temp_opt.C.,y=log10(myfile$Genome_size),
     ylab="log10(Genome size)", xlab="Temperature(°C)",
     col="purple", cex.lab=2, cex.axis=1.5,pch=20,
     ylim=c(5.5,7.5), xlim=c(0,100))

plot(x=myfile$temp_opt.C.,y=myfile$bG+myfile$bC,
     ylab="GC-content", xlab="Temperature(°C)",
     col="red", cex.lab=2, cex.axis=1.5,pch=1,
     ylim=c(0,1), xlim=c(0,100))

plot(x=myfile$bG+myfile$bC,y=myfile$A,
     ylab="Alanine content of proteins", xlab="GC-content of the genome",
     col="red", cex.lab=2, cex.axis=1.5,pch=1,
     ylim=c(0,0.2), xlim=c(0.2,0.8))

####4DV4NC3D UB3RK1NG PL0TT1NG SK1LLZ EX.####
AA_ALL=myfile[,19:39]
Temp_ALL=myfile$temp_opt.C.
GC_ALL=myfile$bG+myfile$bC
colorsch=c("#0F820F", "#C8C8C8","#A9A9A9","#0F820F", "#E60A0A",
           "#FA9600", "#0F820F","#145AFF","#145AFF", "#E60A0A",
           "#FA9600", "#DC9682","#00DCDC","#00DCDC", "#3232AA",
           "#3232AA", "#E6E600","#8282D2","#E6E600", "#B45AB4","#FF1493")

aalist=c("L","A","G","V",
         "E","S","I","K",
         "R","D","T","P",
         "N","Q","F","Y",
         "M","H","C","W","TER")

aalist=c("Leu","Ala","Gly","Val",
         "Glu","Ser","Ile","Lys",
         "Arg","Asp","Thr","Pro",
         "Asn","Gln","Phe","Tyr",
         "Met","His","Cys","Trp")  

par(mfrow = c(4,5), mar=c(4,5,1,1),bg="white",new=FALSE)
for (n in 1:20){#par(mar = c(4, 4, 4, 4) + 0.1) #PLOT
  plot(Temp_ALL,AA_ALL[,n],xlab = "Temperature (°C)", ylab = "Proportion",
       ylim = c(0,0.15),xlim=c(0,100),cex.lab=1.5,cex.axis=1.5,pch=".")
  legend("topright", legend = aalist[n], lwd = 0,cex=1.5,
         col = colorsch[n], ncol=1,
         pt.bg = colorsch[n], pch = 21, bty = "n")}

par(mfrow = c(4,5), mar=c(4,5,1,1),bg="white",new=FALSE)
for (n in 1:20){#par(mar = c(4, 4, 4, 4) + 0.1) #PLOT
  plot(GC_ALL,AA_ALL[,n],xlab = "GC-content", ylab = "Proportion",
       ylim = c(0,0.15),xlim=c(0,1),cex.lab=1.5,cex.axis=1.5,pch=".")
  legend("topright", legend = aalist[n], lwd = 0,cex=1.5,
         col = colorsch[n], ncol=1,
         pt.bg = colorsch[n], pch = 21, bty = "n")}

par(mfrow = c(1,1), mar=c(4,5,1,1),bg="white",new=FALSE)

