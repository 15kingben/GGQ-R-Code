#
# this plots Figure 1 in the manuscript submitted to JASA. i.e., The histogram of the optimal policies. 
rm(list=ls())
load("~/hhome/Rev2-GGQ-75-5k-S1-50-r2-2-gam6.RData")
#source('~/Dropbox/Susan/My Code on Diab./Simulation/Scenario 1/7YCat/OP-Search-7YCat.R', chdir = TRUE)
new.dat2$OP<-ff$OP;   new.dat2$OP[new.dat2$OP>0]<-1
dat3<-new.dat2[new.dat2$catA1c>1&new.dat2$catA1c<6,] # GGQ OP dataset
head(dat3)




load("~/hhome/Rev2-OPBellToDat-S1-5000-7YCat-gam1369.RData") 
new.dat.Bell<-new.dat1<-new.dat.nao[new.dat.nao$quarter>1,]
new.dat.Bell$OPBell6[new.dat.Bell$OPBell6>0]<-1
dat4<-new.dat.Bell[new.dat.Bell$catA1c>1& new.dat.Bell$catA1c<6,] # Bell OP dataset
head(dat4)


######################n=2000###########
load("~/hhome/Rev2-OPBellToDat-S1-2000-7YCat-gam1369.RData") 
new.dat.nao<-new.dat.nao[cs<2]
new.dat.Bell<-new.dat1<-new.dat.nao[new.dat.nao$quarter>1,]
new.dat.Bell$OPBell6[new.dat.Bell$OPBell6>0]<-1
dat5<-new.dat.Bell[new.dat.Bell$catA1c>1& new.dat.Bell$catA1c<6,] # Bell OP dataset

head(dat5)

load("~/hhome/Rev2-GGQ-75-2k-S1-50-r2-2-gam6.RData")
new.dat2$OP<-ff$OP;   new.dat2$OP[new.dat2$OP>0]<-1
dat6<-new.dat2[new.dat2$catA1c>1&new.dat2$catA1c<6,] # GGQ OP dataset
head(dat6)


#############TRUE############################
load("~/hhome/TransProbs-S1-200K-seed501-Rev2-6.RData")
source('~/Dropbox/Susan/My Code on Diab./Simulation/Scenario 1/7YCat/JASA R2 codes/Source_plot_optregimes.R', chdir = TRUE)
dat.true<-data.frame(cbind( sto[,-4],opt.a11,opt.a21)[1:70,])

dat.true<-dat.true[dat.true$A1c2>1&dat.true$A1c2<6&dat.true$step2<5,]

dat.true<-dat.true[-c(5,13,21,29),]
dat.true$op.txt<-as.numeric(dat.true$opt.a21>0 )
dat.true$op.txt <- factor(dat.true$op.txt, levels=c("0", "1"), labels=c("Continue", "Augment"))
dat.true$NAT<-dat.true$step2-1
dat.true$D<-dat.true$D2-1

#pt<-ggplot(dat.true, aes(factor(A1c2), fill=op.txt)) + geom_bar(position = 'fill') + facet_grid(step~D, labeller = label_both )+ scale_fill_manual(values=c("black", "gray"))+labs(title="TRUE")+ylab("Percentage") +xlab("Cat. A1c")+theme(plot.title=element_text(size=rel(0.8)))  

###################################################



library(gridExtra);library(ggplot2)
pdf('Test2gam1.pdf')
png(file="mygraphic.png",width=1500,height=1200)
dat6$NAT<-dat6$step.d
dat6$op.txt<-as.factor(dat6$OP)
#dat6$op.txt[dat6$op.txt==1]<-"Augment"
#dat6$op.txt[dat6$op.txt==0]<-"Continue"
dat6$op.txt <- factor(dat6$OP, levels=c("0", "1"), labels=c("Continue", "Augment"))



p1<-ggplot(dat6[dat6$step.d<4,], aes(factor(catA1c), fill=op.txt)) + geom_bar(position = 'fill') + facet_grid(NAT~D, labeller = label_both )+ scale_fill_manual(values=c("black", "gray"))+labs(title="GGQ(n=2000)")+ylab("Percentage") +xlab("Cat. A1c")+theme_bw()+theme(plot.title=element_text(size=rel(1.2)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.key = element_blank(),strip.background = element_rect(colour="white", fill="white"))  
dat5$NAT<-dat5$step.d
dat5$op.txt<-as.factor(dat5$OPBell6)
dat5$op.txt <- factor(dat5$OPBell6, levels=c("0", "1"), labels=c("Continue", "Augment"))
p2<-ggplot(dat5[dat5$step.d<4,], aes(factor(catA1c), fill=op.txt)) + geom_bar(position = 'fill') + facet_grid(NAT~D, labeller = label_both )+ scale_fill_manual(values=c("black", "gray")) +ylab("Percentage") +xlab("Cat. A1c")+labs(title="Classical(n=2000)")+theme_bw()+theme(plot.title=element_text(size=rel(1.2)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.key = element_blank(),strip.background = element_rect(colour="white", fill="white"))  

dat3$NAT<-dat3$step.d
dat3$op.txt<-as.factor(dat3$OP)
dat3$op.txt <- factor(dat3$OP, levels=c("0", "1"), labels=c("Continue", "Augment"))
p3<-ggplot(dat3[dat3$step.d<4,], aes(factor(catA1c), fill=op.txt)) + geom_bar(position = 'fill') + facet_grid(NAT~D, labeller = label_both )+ scale_fill_manual(values=c("black", "gray"))+ylab("Percentage") +xlab("Cat. A1c") +labs(title="GGQ(n=5000)")+theme_bw()+theme(plot.title=element_text(size=rel(1.2)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.key = element_blank(),strip.background = element_rect(colour="white", fill="white"))  

dat4$NAT<-dat4$step.d
dat4$op.txt<-as.factor(dat4$OPBell6)
dat4$op.txt <- factor(dat4$OPBell6, levels=c("0", "1"), labels=c("Continue", "Augment"))
p4<-ggplot(dat4[dat4$step.d<4,], aes(factor(catA1c), fill=op.txt)) + geom_bar(position = 'fill') + facet_grid(NAT~D, labeller = label_both )+ scale_fill_manual(values=c("black", "gray")) +ylab("Percentage") +xlab("Cat. A1c")+labs(title="Classical(n=5000)")+theme_bw()+theme(plot.title=element_text(size=rel(1.2)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.key = element_blank(),strip.background = element_rect(colour="white", fill="white"))  
dat.true$op.txt<-as.numeric(dat.true$opt.a21>0 )
dat.true$op.txt <- factor(dat.true$op.txt, levels=c("0", "1"), labels=c("Continue", "Augment"))
dat.true$NAT<-dat.true$step2-1
dat.true$D<-dat.true$D2-1

D<-factor(dat.true$D,levels=c(0,1),labels=c("D=0","D=1"))
pt<-ggplot(dat.true, aes(factor(A1c2), fill=op.txt)) + geom_bar(position = 'fill') + facet_grid(NAT~D, labeller = label_both )+ scale_fill_manual(values=c("black", "gray"))+labs(title="TRUE")+ylab("Percentage") +xlab("Cat. A1c")+theme_bw()+theme(plot.title=element_text(size=rel(1.2)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.key = element_blank(),strip.background = element_rect(colour="white", fill="white"),axis.text=element_text(size=11),axis.title=element_text(size=14))  
#grid.arrange(p2, ncol=2)#, nrow=2)
#dev.off()

#myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(p1, p2,pt,p3,p4, ncol=3, nrow=2)
dev.off()
#dev.new()
####################################
library(ggplot2)
ST1<-ST<-c("(0,0,2)","(1,0,2)","(2,0,2)","(3,0,2)","(1,1,2)","(2,1,2)","(3,1,2)", "(0,0,3)","(1,0,3)","(2,0,3)","(3,0,3)", "(1,1,3)","(2,1,3)","(3,1,3)","(0,0,4)","(1,0,4)","(2,0,4)","(3,0,4)","(1,1,4)","(2,1,4)","(3,1,4)","(0,0,5)","(1,0,5)","(2,0,5)","(3,0,5)","(1,1,5)","(2,1,5)","(3,1,5)")


#pdf('val.pdf')
#plot(ValGGQ,ylim=c(-0.3,1.6),pch=16,ylab="Value", xlab="States")
#points(ValBell,col="2",pch=8)
#dev.off()

#barplot(ValBell,names.arg=ST,type="o")
#points(ValBell,col="2")
#qplot(ValBell)


V<-ValBell

V<-c(ValBell, ValGGQ)
ST<-c(ST1,ST1)
Method<-rep(1:2,each=28)
Method[Method==1]<-"Classical"
Method[Method==2]<-"GGQ"


dt<-data.frame(V,ST,Method)

pdf('val.pdf')
#qplot(V,ST,data=dt)+geom_point(aes(shape=factor(Method)))+scale_shape(solid=FALSE)
# n=2000
load("MCVal-S1-gam6-2000-GGQ-7YCat.RData")
ValGGQ<-Rstate
load("MCVal-S1-gam6-2000-Bell-7YCat.RData")
ValBell<-Rstate
V<-c(ValBell, ValGGQ)
ST<-c(ST1,ST1)
Method<-rep(1:2,each=28)
Method[Method==1]<-"Classical"
Method[Method==2]<-"GGQ"
dt1<-data.frame(V,ST,Method)

p4<-qplot(V,ST,data=dt1,shape=Method)+ylab("States (NAT,D,Cat. A1c)") +xlab("Value")+labs(title="n=2000")+theme(plot.title=element_text(size=rel(0.8)))

#n=10,000
load("MCVal-S1-gam6-10000-GGQ-7YCat.RData")
ValGGQ<-Rstate
load("MCVal-S1-gam6-10000-Bell-7YCat.RData")
ValBell<-Rstate
V<-c(ValBell, ValGGQ)
ST<-c(ST1,ST1)
Method<-rep(1:2,each=28)
Method[Method==1]<-"Classical"
Method[Method==2]<-"GGQ"
dt2<-data.frame(V,ST,Method)

p5<-qplot(V,ST,data=dt2,shape=Method)+ylab("States (NAT,D,Cat. A1c)") +xlab("Value")+labs(title="n=10000")+theme(plot.title=element_text(size=rel(0.8)))
grid.arrange(p4, p5, ncol=2)
dev.off()

head(mpg)

load("MCVal-S1-gam6-2000-GGQ-7YCat.RData")
ValGGQ2<-Rstate
load("MCVal-S1-gam6-2000-Bell-7YCat.RData")
ValBell2<-Rstate
load("MCVal-S1-gam6-10000-GGQ-7YCat.RData")
ValGGQ10<-Rstate
load("MCVal-S1-gam6-10000-Bell-7YCat.RData")
ValBell10<-Rstate

NAT<-c(0,1,2,3,1,2,3,0,1,2,3,1,2,3,0,1,2,3,1,2,3,0,1,2,3,1,2,3)
D<-c(0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,0,0,1,1,1)
Cat.A1c<-c(2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5)

library(rgl)
library(rsm)


par(mfrow=c(1,2))



dat<-data.frame(cbind(NAT,D, Cat.A1c, ValGGQ2, ValBell2, ValGGQ10, ValBell10))
dat0<-dat[dat$D==0,]

z<-matrix(dat0$ValGGQ2-dat0$ValBell2,nrow=4)
x<-c(0,1,2,3)
y<-c(2,3,4,5)
zlim=range(y)
zlen<-zlim[2]-zlim[1]+3
#zlen<-5
colorlut<-terrain.colors(28)
col<-colorlut[1*z-zlim[1]+1]

labels<-c("X","Y","Z")
open3d()
plot3d(dat0$NAT,dat0$Cat.A1c,dat0$ValGGQ2-dat0$ValBell2,xlab="NAT",ylab="A1c",zlab="Diff",main="n=2000")
#surface3d(x,y,z,color="green")
surface3d(x,y,z,color=col,back="fill")

zz<-dat0$ValGGQ2-dat0$ValBell2
lm1<-lm(zz~poly(NAT,Cat.A1c,degree=3),data=dat0)
rsm2<-rsm(zz~SO(NAT,Cat.A1c),data=dat1)

#persp(lm1,NAT~Cat.A1c, col = "lightblue")
persp(rsm2,~Cat.A1c+NAT,col = rainbow(50), contours = "colors")
#lines(lm1,NAT~Cat.A1c)


#persp(x,y,z,col = rainbow(50), contours = "colors")



#persp(lm1,NAT~Cat.A1c)$`Cat.A1c ~ NAT`$transf->res
#trans3d(x,y,z,pmat=res)


dat1<-dat[dat$D==1,]
z<-matrix(dat1$ValGGQ2-dat1$ValBell2,nrow=3)
x<-c(1,2,3)
y<-c(2,3,4,5)
zlim=range(y)
zlen<-zlim[2]-zlim[1]+2
colorlut<-terrain.colors(zlen)
col<-colorlut[500*z-zlim[1]+1]
surface3d(x,y,z,color="gray",texmipmap=TRUE)

zz<-dat1$ValGGQ2-dat1$ValBell2
lm2<-lm(zz~poly(NAT,Cat.A1c,degree=2),data=dat1)
##persp(lm2,NAT~Cat.A1c)


rsm2<-rsm(zz~SO(NAT,Cat.A1c),data=dat1)
persp(rsm2,~Cat.A1c+NAT,col = rainbow(50), contours = "colors")


zz<-dat$ValGGQ2-dat$ValBell2
rsm2<-rsm(zz~D+SO(NAT,Cat.A1c),data=dat)
xs<-canonical(rsm2)$xs
#persp(rsm2,~NAT+Cat.A1c,at=xs)



#########
dat<-data.frame(cbind(NAT,D,Cat.A1c, ValGGQ2, ValBell2, ValGGQ10, ValBell10))
dat0<-dat[dat$D==0,]

z<-matrix(dat0$ValGGQ10-dat0$ValBell10,nrow=4)
x<-c(0,1,2,3)
y<-c(2,3,4,5)
zlim=range(y)
zlen<-zlim[2]-zlim[1]+1
colorlut<-terrain.colors(zlen)
col<-colorlut[z-zlim[1]+5]

labels<-c("X","Y","Z")
plot3d(dat0$NAT,dat0$Cat.A1c,dat0$ValGGQ2-dat0$ValBell2,xlab="NAT",ylab="A1c",zlab="Diff",main="n=10000")
surface3d(x,y,z,color=col, back="lines")



zz<-dat0$ValGGQ10-dat0$ValBell10
rsm2<-rsm(zz~SO(NAT,Cat.A1c),data=dat0)

#persp(lm1,NAT~Cat.A1c, col = "lightblue")
persp(rsm2,~Cat.A1c+NAT,col = rainbow(50), contours = "colors")



dat1<-dat[dat$D==1,]
z<-matrix(dat1$ValGGQ10-dat1$ValBell10,nrow=3)
x<-c(1,2,3)
y<-c(2,3,4,5)
zlim=range(y)
zlen<-zlim[2]-zlim[1]+1
colorlut<-terrain.colors(zlen)
col<-colorlut[z-zlim[1]+10]
surface3d(x,y,z,color=col)


zz<-dat1$ValGGQ10-dat1$ValBell10
lm2<-lm(zz~poly(NAT,Cat.A1c,degree=2),data=dat1)
#persp(lm2,NAT~Cat.A1c)

rsm2<-rsm(zz~SO(NAT,Cat.A1c),data=dat1)

sliced<-c(0,1)
persp(rsm2,~Cat.A1c+NAT,col = rainbow(50), contours = "colors")


par(mfrow=c(3,3))

heli.rsm<-rsm(ave ~ block + SO(x1,x2,x3,x4), data = heli)
xs <- canonical(heli.rsm)$xs

persp (heli.rsm, ~ x1 + x2 + x3 + x4, at = xs, col = rainbow(50), contours = "colors")




data(volcano)
z <- 2 * volcano
x <- 10 * (1:nrow(z))
y <- 10 * (1:ncol(z))
# Exaggerate the relief
# 10 meter spacing (S to N)
# 10 meter spacing (E to W)
    zlim <- range(y)
    zlen <- zlim[2] - zlim[1] + 1
    colorlut <- terrain.colors(zlen) # height color lookup table
    col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
    open3d()
    surface3d(x, y, z, color=col, back="lines")




open3d()
      x <- sort(rnorm(1000))
      y <- rnorm(1000)
      z <- rnorm(1000) + atan2(x,y)
      plot3d(x, y, z, col=rainbow(1000))





    
    