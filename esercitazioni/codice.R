library(ggplot2)
library(mirt)
library(difR)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

###############################

# simulate data

n <- 250
nitem <- 50
percDif <- 50

(ndif <- round(nitem*percDif/100)) # item con dif
d <- data.frame(id=rep(1:(n*2),each=nitem),
                gender=rep(c("m","f"),each=n*nitem),
                ability=rep(rnorm(n*2,0,1),each=nitem),
                item=paste("item",rep(1:nitem,times=n*2),sep=""),
                level=rep(rnorm(nitem,0,1),times=n*2),
                dif=NA)
#d$dif <- rep(c(rep(0,nitem-ndif),rgamma(ndif,1.5)),times=n*2)
d$dif <- rep(c(rep(0,nitem-ndif),rnorm(ndif,0.5,0.5)),times=n*2)
d$logit <- d$ability - d$level + d$dif * ifelse(d$gender=="m",0.5,-0.5)
d$prob <- logit2prob(d$logit)
d$resp <- rbinom(nrow(d),1,d$prob)

d2 <- reshape(d[,!colnames(d)%in%c("logit","prob","level","dif")],direction="wide",varying=levels(as.factor(d$item)),
              v.names="resp",timevar="item",idvar="id")
temp = d2[, !colnames(d2) %in% c("id", "gender", "ability")]

temp = temp[unique(d$item)]
d2 = cbind( d2[, colnames(d2) %in% c("id", "gender", "ability")], 
            temp)

m1 = tam.mml(d2[, !colnames(d2) %in% c("id", "gender", "ability")])
summary(m1)
IRT.itemfit(m1)

lordDif = difLord(d2[, !colnames(d2) %in% c("id", "ability")], 
                  group = "gender", focal.name = "m", model = "1PL")
lordDif

2[, !colnames(d2) %in% c("id", "gender", "ability")]
est_theta = IRT.factor.scores(m1)$EAP
nudif.0910 = difGenLogistic(d2[, !colnames(d2) %in% c("id", "gender", "ability")],
                            group = as.factor(d2$gender),
                            focal.names = "f",
                            type = "nudif",
                            alpha = .001,
                            p.adjust.method = "BH",
                            match = est_theta,
                            criterion = "Wald")


nudif.0910

# Testing for uniform DIF effect
udif.0910 = difGenLogistic(d2[, !colnames(d2) %in% c("id", "gender", "ability")],
                           group = as.factor(d2$gender),
                           focal.names = "f",
                           type = "udif",
                           alpha = .001,
                           p.adjust.method = "BH",
                           match = est_theta,
                           criterion = "Wald")
udif.0910


difLRT(data[, grep("item", colnames(data))], group = "gender", focal.name = "m")

raju = difRaju(d2[, !colnames(d2) %in% c("id", "gender", "ability")], 
               group = as.factor(d2$gender), 
               focal.name = "f", model = "1PL")
raju


## Look at the data first! 


(d2[, !colnames(d2) %in% c("id", "gender", "ability")])
ggplot(d, 
       aes(x = item, fill = resp)) + geom_bar()

p = data.frame(table(d$item, d$resp))
p$prop = p$Freq/500
p$Var1 = factor(p$Var1, 
                levels = unique(d$item))
ggplot(p, 
       aes(x = Var1, y=prop,  
 
           fill = Var2)) + geom_bar(stat = "identity") + 
  theme(axis.text = element_text(angle = 90)) + geom_hline(aes(yintercept=.50))

p = data.frame(table(d$item, d$resp, d$gender))
p$prop = p$Freq/500
p$Var1 = factor(p$Var1, 
                levels = unique(d$item))
ggplot(p[p$Var2 %in% 1,], 
       aes(x = Var1, y=prop, fill = Var3)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text = element_text(angle = 90)) + 
  geom_hline(aes(yintercept=.50)) 
