library(sp)
library(raster)
library(tidyverse)

setwd("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 3/Cherry Sweet/Geoperfil")

L1<-read.table("C_T060206002.xyz", header=T, sep=";")
A<-L1[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L1)
L1<-L1[-c(1:4),] 
L1<-as.data.frame(L1)
L1<-L1[-c((A+1):B[1,1]),]
L1<-strsplit(L1, "\\s+")
L1<-matrix(unlist(L1), ncol=5, byrow=TRUE)
L1<-L1[,-1]
colnames(L1) <- c("x", "z", "R", "C")
as.numeric(L1$R)
Lx<-length(levels(factor(L1[, "x"])))
Lz<-length(levels(factor(L1[, "z"])))
MATRIX_L1<-matrix(as.numeric(L1[,"R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)



L2<-read.table("C_T060206003.xyz", header=T, sep=";")
A<-L2[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L2)
L2<-L2[-c(1:4),] 
L2<-as.data.frame(L2)
L2<-L2[-c((A+1):B[1,1]),]
L2<-strsplit(L2, "\\s+")
L2<-matrix(unlist(L2), ncol=5, byrow=TRUE)
L2<-L2[,-1]
colnames(L2) <- c("x", "z", "R", "C")
as.numeric(L2)
Lx<-length(levels(factor(L2[, "x"])))
Lz<-length(levels(factor(L2[, "z"])))
MATRIX_L2<-matrix(as.numeric(L2[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L3<-read.table("C_T060206004.xyz", header=T, sep=";")
A<-L3[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L3)
L3<-L3[-c(1:4),] 
L3<-as.data.frame(L3)
L3<-L3[-c((A+1):B[1,1]),]
L3<-strsplit(L3, "\\s+")
L3<-matrix(unlist(L3), ncol=5, byrow=TRUE)
L3<-L3[,-1]
colnames(L3) <- c("x", "z", "R", "C")
as.numeric(L3)
Lx<-length(levels(factor(L3[, "x"])))
Lz<-length(levels(factor(L3[, "z"])))
MATRIX_L3<-matrix(as.numeric(L3[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L4<-read.table("C_T060206005.xyz", header=T, sep=";")
A<-L4[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L4)
L4<-L4[-c(1:4),] 
L4<-as.data.frame(L4)
L4<-L4[-c((A+1):B[1,1]),]
L4<-strsplit(L4, "\\s+")
L4<-matrix(unlist(L4), ncol=5, byrow=TRUE)
L4<-L4[,-1]
colnames(L4) <- c("x", "z", "R", "C")
as.numeric(L4)
Lx<-length(levels(factor(L4[, "x"])))
Lz<-length(levels(factor(L4[, "z"])))
MATRIX_L4<-matrix(as.numeric(L4[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L5<-read.table("C_T060206005.xyz", header=T, sep=";")
A<-L5[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L5)
L5<-L5[-c(1:4),] 
L5<-as.data.frame(L5)
L5<-L5[-c((A+1):B[1,1]),]
L5<-strsplit(L5, "\\s+")
L5<-matrix(unlist(L5), ncol=5, byrow=TRUE)
L5<-L5[,-1]
colnames(L5) <- c("x", "z", "R", "C")
as.numeric(L5)
Lx<-length(levels(factor(L5[, "x"])))
Lz<-length(levels(factor(L5[, "z"])))
MATRIX_L5<-matrix(as.numeric(L5[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)

L6<-read.table("C_T060206006.xyz", header=T, sep=";")
A<-L6[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L6)
L6<-L6[-c(1:4),] 
L6<-as.data.frame(L6)
L6<-L6[-c((A+1):B[1,1]),]
L6<-strsplit(L6, "\\s+")
L6<-matrix(unlist(L6), ncol=5, byrow=TRUE)
L6<-L6[,-1]
colnames(L6) <- c("x", "z", "R", "C")
as.numeric(L6)
Lx<-length(levels(factor(L6[, "x"])))
Lz<-length(levels(factor(L6[, "z"])))
MATRIX_L6<-matrix(as.numeric(L6[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L7<-read.table("C_T060206015.xyz", header=T, sep=";")
A<-L7[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L7)
L7<-L7[-c(1:4),] 
L7<-as.data.frame(L7)
L7<-L7[-c((A+1):B[1,1]),]
L7<-strsplit(L7, "\\s+")
L7<-matrix(unlist(L7), ncol=5, byrow=TRUE)
L7<-L7[,-1]
colnames(L7) <- c("x", "z", "R", "C")
as.numeric(L7)
Lx<-length(levels(factor(L7[, "x"])))
Lz<-length(levels(factor(L7[, "z"])))
MATRIX_L7<-matrix(as.numeric(L7[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L8<-read.table("C_T060206016.xyz", header=T, sep=";")
A<-L8[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L8)
L8<-L8[-c(1:4),] 
L8<-as.data.frame(L8)
L8<-L8[-c((A+1):B[1,1]),]
L8<-strsplit(L8, "\\s+")
L8<-matrix(unlist(L8), ncol=5, byrow=TRUE)
L8<-L8[,-1]
colnames(L8) <- c("x", "z", "R", "C")
as.numeric(L8)
Lx<-length(levels(factor(L8[, "x"])))
Lz<-length(levels(factor(L8[, "z"])))
MATRIX_L8<-matrix(as.numeric(L8[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L9<-read.table("C_T060206017.xyz", header=T, sep=";")
A<-L9[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L9)
L9<-L9[-c(1:4),] 
L9<-as.data.frame(L9)
L9<-L9[-c((A+1):B[1,1]),]
L9<-strsplit(L9, "\\s+")
L9<-matrix(unlist(L9), ncol=5, byrow=TRUE)
L9<-L9[,-1]
colnames(L9) <- c("x", "z", "R", "C")
as.numeric(L9)
Lx<-length(levels(factor(L9[, "x"])))
Lz<-length(levels(factor(L9[, "z"])))
MATRIX_L9<-matrix(as.numeric(L9[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L10<-read.table("C_T060206018.xyz", header=T, sep=";")
A<-L10[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L10)
L10<-L10[-c(1:4),] 
L10<-as.data.frame(L10)
L10<-L10[-c((A+1):B[1,1]),]
L10<-strsplit(L10, "\\s+")
L10<-matrix(unlist(L10), ncol=5, byrow=TRUE)
L10<-L10[,-1]
colnames(L10) <- c("x", "z", "R", "C")
as.numeric(L10)
Lx<-length(levels(factor(L10[, "x"])))
Lz<-length(levels(factor(L10[, "z"])))
MATRIX_L10<-matrix(as.numeric(L10[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L11<-read.table("C_T060206019.xyz", header=T, sep=";")
A<-L11[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L11)
L11<-L11[-c(1:4),] 
L11<-as.data.frame(L11)
L11<-L11[-c((A+1):B[1,1]),]
L11<-strsplit(L11, "\\s+")
L11<-matrix(unlist(L11), ncol=5, byrow=TRUE)
L11<-L11[,-1]
colnames(L11) <- c("x", "z", "R", "C")
as.numeric(L11)
Lx<-length(levels(factor(L11[, "x"])))
Lz<-length(levels(factor(L11[, "z"])))
MATRIX_L11<-matrix(as.numeric(L11[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L12<-read.table("C_T060206020.xyz", header=T, sep=";")
A<-L12[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L12)
L12<-L12[-c(1:4),] 
L12<-as.data.frame(L12)
L12<-L12[-c((A+1):B[1,1]),]
L12<-strsplit(L12, "\\s+")
L12<-matrix(unlist(L12), ncol=5, byrow=TRUE)
L12<-L12[,-1]
colnames(L12) <- c("x", "z", "R", "C")
as.numeric(L12)
Lx<-length(levels(factor(L12[, "x"])))
Lz<-length(levels(factor(L12[, "z"])))
MATRIX_L12<-matrix(as.numeric(L12[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L13<-read.table("C_T060206021.xyz", header=T, sep=";")
A<-L13[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L13)
L13<-L13[-c(1:4),] 
L13<-as.data.frame(L13)
L13<-L13[-c((A+1):B[1,1]),]
L13<-strsplit(L13, "\\s+")
L13<-matrix(unlist(L13), ncol=5, byrow=TRUE)
L13<-L13[,-1]
colnames(L13) <- c("x", "z", "R", "C")
as.numeric(L13)
Lx<-length(levels(factor(L13[, "x"])))
Lz<-length(levels(factor(L13[, "z"])))
MATRIX_L13<-matrix(as.numeric(L13[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)


L14<-read.table("C_T060206022.xyz", header=T, sep=";")
A<-L14[1,1]
A<-substring(A, 21)
A<-as.numeric(A)
B<-count(L14)
L14<-L14[-c(1:4),] 
L14<-as.data.frame(L14)
L14<-L14[-c((A+1):B[1,1]),]
L14<-strsplit(L14, "\\s+")
L14<-matrix(unlist(L14), ncol=5, byrow=TRUE)
L14<-L14[,-1]
colnames(L14) <- c("x", "z", "R", "C")
as.numeric(L14)
Lx<-length(levels(factor(L14[, "x"])))
Lz<-length(levels(factor(L14[, "z"])))
MATRIX_L14<-matrix(as.numeric(L14[, "R"])) %>% matrix(ncol=Lx, nrow=Lz, byrow=TRUE)






##cREACI?N DE RASTER

raster_L1<-raster(MATRIX_L1)
extent(raster_L1)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L2<-raster(MATRIX_L2)
extent(raster_L2)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L3<-raster(MATRIX_L3)
extent(raster_L3)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L4<-raster(MATRIX_L4)
extent(raster_L4)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L5<-raster(MATRIX_L5)
extent(raster_L5)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L6<-raster(MATRIX_L6)
extent(raster_L6)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L7<-raster(MATRIX_L7)
extent(raster_L7)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L8<-raster(MATRIX_L8)
extent(raster_L8)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L9<-raster(MATRIX_L9)
extent(raster_L9)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L10<-raster(MATRIX_L10)
extent(raster_L10)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L11<-raster(MATRIX_L11)
extent(raster_L11)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L12<-raster(MATRIX_L12)
extent(raster_L12)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L13<-raster(MATRIX_L13)
extent(raster_L13)<-extent(0.05, 7.75, -1.643, -0.028)


raster_L14<-raster(MATRIX_L14)
extent(raster_L14)<-extent(0.05, 7.75, -1.643, -0.028)


jpeg(filename="Gráfico1.jpg",width=8000,height=5000,units="px",res=700,pointsize=12,quality=75)

m<-matrix(1:4,4,1)
layout(m,widths=c(1),heights=c(1,1,1))
par(mar=c(2,5,0,2))

plot(raster_L1, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028))
legend("bottomright",c("A"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)


plot(raster_L2, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("B"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

plot(raster_L3, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")), xlab=expression("Distancia m"), 
     xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("C"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

plot(raster_L4, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")), xlab=expression("Distancia m"), 
     xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("D"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

dev.off()

jpeg(filename="Gráfico2.jpg",width=8000,height=5000,units="px",res=700,pointsize=12,quality=75)

m<-matrix(1:4,4,1)
layout(m,widths=c(1),heights=c(1,1,1))
par(mar=c(2,5,0,2))

plot(raster_L5, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028))
legend("bottomright",c("A"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)


plot(raster_L6, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("B"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

plot(raster_L7, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")), xlab=expression("Distancia m"), 
     xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("C"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

plot(raster_L8, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")), xlab=expression("Distancia m"), 
     xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("D"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

dev.off()

jpeg(filename="Gráfico3.jpg",width=8000,height=5000,units="px",res=700,pointsize=12,quality=75)

m<-matrix(1:4,4,1)
layout(m,widths=c(1),heights=c(1,1,1))
par(mar=c(2,5,0,2))

plot(raster_L9, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028))
legend("bottomright",c("A"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)


plot(raster_L10, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("B"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

plot(raster_L11, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")), xlab=expression("Distancia m"), 
     xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("C"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

plot(raster_L12, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")), xlab=expression("Distancia m"), 
     xlim=(c(0,8)), ylim=c(-1.643, -.028),las=1)
legend("bottomright",c("D"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

dev.off()

jpeg(filename="Gráfico4.jpg",width=8000,height=5000,units="px",res=700,pointsize=12,quality=75)

m<-matrix(1:4,4,1)
layout(m,widths=c(1),heights=c(1,1,1))
par(mar=c(2,5,0,2))

plot(raster_L13, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028))
legend("bottomright",c("A"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)

plot(raster_L14, col=topo.colors(50), interpolate=TRUE, ylab=expression(bold("Profundidad (m)")),xlim=(c(0,8)), ylim=c(-1.643, -.028))
legend("bottomright",c("B"),bty="n")
abline(h=-0.2, lwd=2, col="red", lty=2)
abline(h=-0.4, lwd=2, col="black", lty=2)
abline(h=-0.6, lwd=2, col="orange", lty=2)
abline(h=-0.8, lwd=2, col="purple", lty=2)
abline(h=-1, lwd=2, col="brown", lty=2)


plot(0,type="n",axes=F,xaxs="r",yaxs="i",ylab="")

plot(0,type="n",axes=F,xaxs="r",yaxs="i",ylab="")

dev.off()
