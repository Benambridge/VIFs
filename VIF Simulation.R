#devtools::install_github("debruine/faux")
#install.packages("rlang")
library(faux) # for data simulation
library(ggplot2) # for plots
library(car) # for VIFs

### OK here is the correlation matrix

## IV1 + IV2 = IVCor
## IV1 + DV = Out1
## IV2 + DV = Out2


#####   IV1   IV2   DV
## IV1  1     IVCor Out1        
## IV2  IVCor 1     Out2   
## DV   Out1  Out2  1

Output=NULL
seed=1
exact=T #(Set to T or F depending on whether you want it to give the exact correlations specified, or just draw randomly from a population with those correlations)

### First when there is no correlation between the IVs (Sometimes gives odd results so just delete it later)
cmat <- c(1, IVCor, Out1,
          IVCor, 1, Out2,
          Out1, Out2, 1)
  run=0
  IVCor=0
  Out1=0.4
  Out2=0.5
  Data <- rnorm_multi(100, 3, 0, 1, cmat, varnames = c("IV1", "IV2", "DV"), empirical = exact)
  Model = lm(DV~ IV1+IV2, data=Data)
  Output=as.data.frame(summary(Model)$coef)
  Output$IVCorrelation=IVCor
  Output$Zero_Order_IV1_DV=Out1
  Output$Zero_Order_IV2_DV=Out2
  Output$Effect=c("(Intercept)", "IV1", "IV2")
  Output$Run=run
  VIF=vif(Model)
  Output$VIFs=c(0,VIF[1],VIF[2])  
  
### Now loop this over different values of correlation between the IVs
for (i in c(0.05, 0.1, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)) {
IVCor=i
Out1=0.4
Out2=0.5

cmat <- c(1, IVCor, Out1,
          IVCor, 1, Out2,
          Out1, Out2, 1)

Data <- rnorm_multi(100, 3, 0, 1, cmat, varnames = c("IV1", "IV2", "DV"), empirical = exact)

Model = lm(DV~ IV1+IV2, data=Data)
temp=as.data.frame(summary(Model)$coef)
temp$IVCorrelation=IVCor
temp$Zero_Order_IV1_DV=Out1
temp$Zero_Order_IV2_DV=Out2
temp$Effect=c("(Intercept)", "IV1", "IV2")
temp$Run=run
VIF=vif(Model)
temp$VIFs=c(0,VIF[1],VIF[2])  

run=run+1
Output=rbind(Output, temp)
}

summary(Model)


colnames(Output)=c("Estimate", "SE", "t", "p", "IV_Correlation", "Zero_Order_IV1_DV", "Zero_Order_IV2_DV", "Effect", "Run", "VIF")
Output=subset(Output, Effect!="(Intercept)")
Output=subset(Output, Run>0)


Output$VIF=round(Output$VIF, digits = 2)


Output

ggplot(Output, aes(x=IV_Correlation, y=p, group=Effect, colour=Effect))  +geom_point() +ylab("p value") +xlab("Correlation between IVs") +geom_hline(yintercept=0) + scale_y_continuous(trans='log') +geom_hline(yintercept=0.05) + ylab("p value") +geom_label(label=Output$VIF)

ggplot(Output, aes(x=IV_Correlation, y=Estimate, group=Effect, colour=Effect)) + geom_errorbar(aes(ymin=Estimate-SE, ymax=Estimate+SE), width=0.02) +geom_label(label=Output$VIF)  +ylab("Estimate (SE)") +xlab("Correlation between IVs") +geom_hline(yintercept=0) +ggtitle("IV1 and IV2 correlated with DV at r=0.4 and r=0.5 respectively")

