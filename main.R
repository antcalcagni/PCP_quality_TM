
# Set environment and load data -------------------------------------------
rm(list=ls()); graphics.off(); options(warn = -1)
#setwd("")
source("utils/utils.R")

load("elaborated_results/measures_LDA_aggregated.rds")
Design <- read.table(file = "design.dat",header = TRUE,sep = ";")
DD <- NROW(Design)
k0neigh <- matrix(c(2,6,8,10,16,2,18,20,22,40),ncol = 5,byrow = TRUE); rownames(k0neigh) <- c("K=8","K=20")




# Descriptive analyses ----------------------------------------------------

### Figure 1
cls <- c("#4682B4", "#CD5C5C", "#DAA520")  # Steel Blue, Indian Red, Goldenrod

dd <- c(as.numeric(rownames(Design[Design$n==1e3 & Design$K==8 & Design$lambda==1e3 & Design$V==5e3,])),
        as.numeric(rownames(Design[Design$n==3e3 & Design$K==8 & Design$lambda==1e3 & Design$V==5e3,])),
        as.numeric(rownames(Design[Design$n==1e3 & Design$K==20 & Design$lambda==1e3 & Design$V==5e3,])),
        as.numeric(rownames(Design[Design$n==3e3 & Design$K==20 & Design$lambda==1e3 & Design$V==5e3,])))

#x11();
tikzDevice::tikz(file='/home/antonio/MEGA/Lavoro_sync/Conferences/2025_IES Bressanone/contribution/fig1.tex',width=8.5,height=6.5)
par(mfrow=c(3,4),mar=c(6,5,3,2));
lwdx <- 1.5
k <- 0; 
for(d in dd){
  k <- k+1
  ylims <- c(min(res[[d]]$avg[,"cl",c(2,3,4)])-0.025,max(res[[d]]$avg[,"cl",c(2,3,4)])+0.025)
  if(Design$K[d]==8){xsup <-k0neigh[1,]}else{xsup <-k0neigh[2,]}
  tit <- paste0("n=",Design$n[d])
  ytit <- ifelse(k==1,"CL","")
  if(k==1){yax <- round(seq(ylims[1],ylims[2],length=5),2)}else{rep("",5)}
  
  plot(1:5,rep(0,5),col="white",bty="n",type="b",ylim=ylims,xlab="",ylab = yylab,pch=20,main="",axes = FALSE); title(tit,cex.main=1.25)
  lines(1:5,res[[d]]$avg[,"cl","fLSA"],col=cls[1],type="b",pch=20,lwd=lwdx) #fLSA
  lines(1:5,res[[d]]$avg[,"cl","LDA"],col=cls[2],type="b",pch=20,lwd=lwdx) #LDA
  lines(1:5,res[[d]]$avg[,"cl","CTM"],col=cls[3],type="b",pch=20,lwd=lwdx) #CTM
  
  axis(side = 1,at = 1:5,labels = rep(NA,5)); #x-axis
  axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5));  #y-axis
  mtext(text = yax,side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 0.85);
  mtext(text = ytit,side = 2,at = median(round(seq(ylims[1],ylims[2],length=5),2)),line = 3,cex = 1.1,font = 2);
  
  abline(v = 3,lty=2,col="grey",lwd=2)
}

k <- 0; 
for(d in dd){
  k <- k+1
  ylims <- c(min(res[[d]]$avg[,"rl",c(2,3,4)])-0.025,max(res[[d]]$avg[,"rl",c(2,3,4)])+0.025)
  if(Design$K[d]==8){xsup <-k0neigh[1,]}else{xsup <-k0neigh[2,]}
  ytit <- ifelse(k==1,"RL","")
  tit <- ""
  if(k==1){yax <- round(seq(ylims[1],ylims[2],length=5),2)}else{rep("",5)}
  
  plot(1:5,rep(0,5),col="white",bty="n",type="b",ylim=ylims,xlab="",ylab = yylab,pch=20,main="",axes = FALSE); title(tit)
  lines(1:5,res[[d]]$avg[,"rl","fLSA"],col=cls[1],type="b",pch=20,lwd=lwdx) #fLSA
  lines(1:5,res[[d]]$avg[,"rl","LDA"],col=cls[2],type="b",pch=20,lwd=lwdx) #LDA
  lines(1:5,res[[d]]$avg[,"rl","CTM"],col=cls[3],type="b",pch=20,lwd=lwdx) #CTM
  
  axis(side = 1,at = 1:5,labels = rep(NA,5)); #x-axis
  axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5));  #y-axis
  mtext(text = yax,side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 0.85);
  mtext(text = ytit,side = 2,at = median(round(seq(ylims[1],ylims[2],length=5),2)),line = 3,cex = 1.1,font = 2);
  
  abline(v = 3,lty=2,col="grey",lwd=2)
}

k <- 0; 
for(d in dd){
  k <- k+1
  ylims <- c(min(log(res[[d]]$avg[,"stb1",c(2,3,4)]))-0.025,max(log(res[[d]]$avg[,"stb1",c(2,3,4)]))+0.025)
  ylims[2] <- -2.30
  if(Design$K[d]==8){xsup <-k0neigh[1,]}else{xsup <-k0neigh[2,]}
  ytit <- ifelse(k==1,"log-HO","")
  tit <- ""
  if(k==1){yax <- round(seq(ylims[1],ylims[2],length=5),2)}else{rep("",5)}
  
  plot(1:5,rep(0,5),col="white",bty="n",type="b",ylim=ylims,xlab="",ylab = yylab,pch=20,main="",axes = FALSE); title(tit)
  lines(1:5,log(res[[d]]$avg[,"stb1","fLSA"]),col=cls[1],type="b",pch=20,lwd=lwdx) #fLSA
  lines(1:5,log(res[[d]]$avg[,"stb1","LDA"]),col=cls[2],type="b",pch=20,lwd=lwdx) #LDA
  lines(1:5,log(res[[d]]$avg[,"stb1","CTM"]),col=cls[3],type="b",pch=20,lwd=lwdx) #CTM
  
  axis(side = 1,at = 1:5,labels = rep(NA,5)); #x-axis
  mtext(text = paste0("",xsup),side = 1,at = 1:5,line = 1.5,cex = 0.8);
  axis(side = 2,at = round(seq(ylims[1],ylims[2],length=3),2),labels = rep(NA,5));  #y-axis
  mtext(text = yax,side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 0.65);
  mtext(text = ytit,side = 2,at = median(round(seq(ylims[1],ylims[2],length=5),2)),line = 3,cex = 1.1,font = 2);
  
  abline(v = 3,lty=2,col="grey",lwd=2)
}
add_legend("bottom",fill = cls[c(1,2,3)],legend = c("fLSA","LDA","CTM"),border = FALSE,bty = "n",ncol = 3,cex=1.65)
dev.off()



# GLMs analysis -----------------------------------------------------------

library(glmmTMB)
dd <- as.numeric(rownames(Design[Design$lambda==1e3 & Design$V==5e3,]))

## CL index
out <- data.frame()
for(d in dd){
  y <- as.numeric(res[[d]]$avg[,"cl",c(2,3,4)])
  out <- rbind(out,
               data.frame(y,n=rep(Design[d,"n"],length(y)),V=rep(Design[d,"V"],length(y)),K=rep(Design[d,"K"],length(y)),alg=rep(c("fLSA","LDA","CTM"),each=5)))
}
str(out)
out$n <- as.factor(out$n)
#out$V <- as.factor(out$V)
out$K <- as.factor(out$K)
out$alg <- as.factor(out$alg); out$alg = relevel(out$alg,ref = "LDA")

mod0 <- glmmTMB::glmmTMB(formula = y~n*K*alg+(1|alg),data = out,family = list(family="beta",link="logit"),contrasts = "contr.sum",dispformula = ~1,)
mod1 <- glmmTMB::glmmTMB(formula = y~n*K*alg+(1|alg),data = out,family = list(family="beta",link="logit"),contrasts = "contr.sum",dispformula = ~alg)
anova(mod0,mod1)
A1 <- car::Anova(mod1,type="III")
summary(mod1)
A2 <- summary(mod1)$coefficients$disp
X <- as.data.frame(ggeffects::ggpredict(mod1,terms = as.formula(~n*K*alg)))


## RL index
out <- data.frame()
for(d in dd){
  y <- as.numeric(res[[d]]$avg[,"rl",c(2,3,4)])
  out <- rbind(out,
               data.frame(y,n=rep(Design[d,"n"],length(y)),V=rep(Design[d,"V"],length(y)),K=rep(Design[d,"K"],length(y)),alg=rep(c("fLSA","LDA","CTM"),each=5)))
}
str(out)
out$n <- as.factor(out$n)
#out$V <- as.factor(out$V)
out$K <- as.factor(out$K)
out$alg <- as.factor(out$alg); out$alg = relevel(out$alg,ref = "LDA")

mod0 <- glmmTMB::glmmTMB(formula = y~n*K*alg+(1|alg),data = out,family = list(family="beta",link="logit"),contrasts = "contr.sum",dispformula = ~1)
mod1 <- glmmTMB::glmmTMB(formula = y~n*K*alg+(1|alg),data = out,family = list(family="beta",link="logit"),contrasts = "contr.sum",dispformula = ~alg)
anova(mod0,mod1)
B1 <- car::Anova(mod1,type="III")
summary(mod1)
B2 <- summary(mod1)$coefficients$disp

cls <- c("#B03060","#668B8B")

### Figure 2 (Supplementary)
tikzDevice::tikz(file='/home/antonio/MEGA/Lavoro_sync/Conferences/2025_IES Bressanone/contribution/fig2.tex',width=8,height=3.5)
#x11()
par(mfrow=c(1,4));
ylims <- c(min(X$predicted),max(X$predicted))
plot(1:3,rep(0,3),col="white",bty="n",ylim=c(ylims[1],ylims[2]),axes = FALSE,xlab="",ylab=""); title(main="CL | K=8",adj=0,cex.main=1.3)
points(1:3,X$predicted[X$x==1000 & X$group==8],pch=16,type="b",col=cls[1],lwd=1.5)
points(1:3,X$predicted[X$x==3000 & X$group==8],pch=16,type="b",col=cls[2],wld=1.5)

axis(side = 1,at = 1:3,labels = rep(NA,3)); mtext(text = c("LDA","CTM","fLSA"),side = 1,at = 1:3,line = 1.5,cex = 1.25);
axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5)); mtext(text = round(seq(ylims[1],ylims[2],length=5),2),side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 1);

plot(1:3,rep(0,3),col="white",bty="n",ylim=c(ylims[1],ylims[2]),axes = FALSE,xlab="",ylab=""); title(main="CL | K=20",adj=0,cex.main=1.3)
points(1:3,X$predicted[X$x==1000 & X$group==20],pch=16,type="b",col=cls[1],lwd=1.5)
points(1:3,X$predicted[X$x==3000 & X$group==20],pch=16,type="b",col=cls[2],wld=1.5)

axis(side = 1,at = 1:3,labels = rep(NA,3)); mtext(text = c("LDA","CTM","fLSA"),side = 1,at = 1:3,line = 1.5,cex = 1.25);
axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5)); mtext(text = round(seq(ylims[1],ylims[2],length=5),2),side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 1);

X <- as.data.frame(ggeffects::ggpredict(mod1,terms = as.formula(~n*K*alg)))
ylims <- c(min(X$predicted),max(X$predicted))
plot(1:3,rep(0,3),col="white",bty="n",ylim=c(ylims[1],ylims[2]),axes = FALSE,xlab="",ylab=""); title(main="RL | K=8",adj=0,cex.main=1.3)
points(1:3,X$predicted[X$x==1000 & X$group==8],pch=16,type="b",col=cls[1],lwd=1.5)
points(1:3,X$predicted[X$x==3000 & X$group==8],pch=16,type="b",col=cls[2],wld=1.5)

axis(side = 1,at = 1:3,labels = rep(NA,3)); mtext(text = c("LDA","CTM","fLSA"),side = 1,at = 1:3,line = 1.5,cex = 1.25);
axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5)); mtext(text = round(seq(ylims[1],ylims[2],length=5),2),side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 1);

plot(1:3,rep(0,3),col="white",bty="n",ylim=c(ylims[1],ylims[2]),axes = FALSE,xlab="",ylab=""); title(main="RL | K=20",adj=0,cex.main=1.3)
points(1:3,X$predicted[X$x==1000 & X$group==20],pch=16,type="b",col=cls[1],lwd=1.5)
points(1:3,X$predicted[X$x==3000 & X$group==20],pch=16,type="b",col=cls[2],wld=1.5)

axis(side = 1,at = 1:3,labels = rep(NA,3)); mtext(text = c("LDA","CTM","fLSA"),side = 1,at = 1:3,line = 1.5,cex = 1.25);
axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5)); mtext(text = round(seq(ylims[1],ylims[2],length=5),2),side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 1);

add_legend("topright",fill = cls,legend = c("n=1000","n=3000"),border = FALSE,bty = "n",ncol = 1,cex=1.3)
dev.off()


## HO index
out <- data.frame()
for(d in dd){
  y <- as.numeric(res[[d]]$avg[,"stb1",c(2,3,4)])
  out <- rbind(out,
               data.frame(y,n=rep(Design[d,"n"],length(y)),V=rep(Design[d,"V"],length(y)),K=rep(Design[d,"K"],length(y)),alg=rep(c("fLSA","LDA","CTM"),each=5)))
}
str(out)
out$n <- as.factor(out$n)
#out$V <- as.factor(out$V)
out$K <- as.factor(out$K)
out$alg <- as.factor(out$alg); out$alg = relevel(out$alg,ref = "LDA")

mod0 <- glmmTMB::glmmTMB(formula = log(y)~n*K*alg+(1|alg),data = out,family = list(family="gaussian",link="identity"),contrasts = "contr.sum",dispformula = ~1)
mod1 <- glmmTMB::glmmTMB(formula = log(y)~n*K*alg+(1|alg),data = out,family = list(family="gaussian",link="identity"),contrasts = "contr.sum",dispformula = ~alg)
anova(mod0,mod1)
C1 <- car::Anova(mod1,type="III")
summary(mod1)
C2 <- summary(mod1)$coefficients$disp


X <- as.data.frame(effects::allEffects(mod = mod1))[[1]]; colnames(X)[c(1,2,4)] = c("x","group","predicted")
cls <- c("#B03060","#668B8B","#698B22","#FFA500")


### Figure 3 (Supplementary)
#x11();
tikzDevice::tikz(file='/home/antonio/MEGA/Lavoro_sync/Conferences/2025_IES Bressanone/contribution/fig3.tex',width=5,height=5.5)
par(mfrow=c(2,2));
ylims <- c(min(X$predicted),max(X$predicted))
plot(1:3,rep(0,3),col="white",bty="n",ylim=c(ylims[1],ylims[2]),axes = FALSE,xlab="",ylab=""); title(main="K=8",adj=0)
points(1:3,X$predicted[X$x==1000 & X$group==8],pch=16,type="b",col=cls[1],lwd=1.5)
points(1:3,X$predicted[X$x==3000 & X$group==8],pch=16,type="b",col=cls[2],wld=1.5)

axis(side = 1,at = 1:3,labels = rep(NA,3)); mtext(text = c("LDA","CTM","fLSA"),side = 1,at = 1:3,line = 1.5,cex = 1.25);
axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5)); mtext(text = round(seq(ylims[1],ylims[2],length=5),2),side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 1);

plot(1:3,rep(0,3),col="white",bty="n",ylim=c(ylims[1],ylims[2]),axes = FALSE,xlab="",ylab=""); title(main="K=20",adj=0)
points(1:3,X$predicted[X$x==1000 & X$group==20],pch=16,type="b",col=cls[1],lwd=1.5)
points(1:3,X$predicted[X$x==3000 & X$group==20],pch=16,type="b",col=cls[2],wld=1.5)

axis(side = 1,at = 1:3,labels = rep(NA,3)); mtext(text = c("LDA","CTM","fLSA"),side = 1,at = 1:3,line = 1.5,cex = 1.25);
axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5)); mtext(text = round(seq(ylims[1],ylims[2],length=5),2),side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 1);

Z <- aggregate(predicted~group*alg,data=X,FUN = mean)
ylims <- c(min(Z$predicted),max(Z$predicted))
plot(1:3,rep(0,3),col="white",bty="n",ylim=c(ylims[1],ylims[2]),axes = FALSE,xlab="",ylab=""); title(main="K x alg",adj=0)
points(1:3,Z$predicted[Z$group==8],pch=16,type="b",col=cls[3],lwd=1.5)
points(1:3,Z$predicted[Z$group==20],pch=16,type="b",col=cls[4],wld=1.5)

axis(side = 1,at = 1:3,labels = rep(NA,3)); mtext(text = c("LDA","CTM","fLSA"),side = 1,at = 1:3,line = 1.5,cex = 1.25);
axis(side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),labels = rep(NA,5)); mtext(text = round(seq(ylims[1],ylims[2],length=5),2),side = 2,at = round(seq(ylims[1],ylims[2],length=5),2),line = 1,cex = 1)

add_legend("right",fill = c(cls[1],cls[3],cls[2],cls[4]),legend = c("n=1000","K=2","n=3000","K=8"),border = FALSE,bty = "n",ncol = 2,cex=1.35)
dev.off()


### Table 1
A <- cbind(rownames(A1),paste0(round(A1[,1],3)," (",A1[,2],")"),paste0(round(B1[,1],3)," (",B1[,2],")"),paste0(round(C1[,1],3)," (",C1[,2],")"))
Xtab_tex = xtable::xtable(A)
attributes(Xtab_tex)$row.names <- rep("",NROW(A))
attributes(Xtab_tex)$caption <- ""
attributes(Xtab_tex)$label <- "tab1"
attributes(Xtab_tex)$align <- rep("c",length(attributes(Xtab_tex)$align))
print.xtable(Xtab_tex,table.placement = "h!",sanitize.text.function = function(x){x})


### Table 2
A <- round(cbind(c(A2[1,1],A2[1,1]+A2[2,1],A2[1,1]+A2[3,1]),c(B2[1,1],B2[1,1]+B2[2,1],B2[1,1]+B2[3,1]),c(C2[1,1],C2[1,1]+C2[2,1],C2[1,1]+C2[3,1])),3)
Xtab_tex = xtable::xtable(A)
attributes(Xtab_tex)$row.names <- rep("",NROW(A))
attributes(Xtab_tex)$caption <- ""
attributes(Xtab_tex)$label <- "tab2"
attributes(Xtab_tex)$align <- rep("c",length(attributes(Xtab_tex)$align))
print.xtable(Xtab_tex,table.placement = "h!",sanitize.text.function = function(x){x})
