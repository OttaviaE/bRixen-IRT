## ----setup, include=FALSE-----------------------------------------------------

library(TAM)
library(mokken)
library(lavaan)
library(difR)
library(sirt)
library(ggplot2)

#  

## -----------------------------------------------------------------------------
# questa funzione calcola la probabilit√† di risposta corretta dato un certo theta e determinati valori dell'item
IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}




## -----------------------------------------------------------------------------
# questa funzione estrae tutti gli item e i loro parametri e calcola la probabilit√† di risposta corretta per ogni item per ogni livello di theta
# restituisce una lista con dentro il dataset usatoe un grafico in ggplot
irt.icc = function(model) {
  item_par = model$item
  est_theta = seq(-4,4, length.out=1000)
  item_prob = list()
  if (any(grep("guess", colnames(item_par))) == F) {
    for (i in 1:nrow(item_par)) {
      item_prob[[i]] = data.frame(theta = est_theta)
      item_prob[[i]]$it_p = IRT(item_prob[[i]]$theta, 
                          b = item_par[i, "xsi.item"], 
                          a = item_par[i, "B.Cat1.Dim1"])
      item_prob[[i]]$item = item_par[i, "item"]

}
  } else {
     for (i in 1:nrow(item_par)) {
      item_prob[[i]] = data.frame(theta = est_theta)
      item_prob[[i]]$it_p = IRT(item_prob[[i]]$theta, 
                          b = item_par[i, "AXsi_.Cat1"], 
                          a = item_par[i, "B.Cat1.Dim1"], 
                          c = item_par[i, "guess"])
      item_prob[[i]]$item = item_par[i, "item"]

}
  }
  p = do.call("rbind", item_prob)
  gp = ggplot(p, 
       aes(x = theta, y = it_p, group = item, col =
             item)) + geom_line(lwd = 1)
  object = list(prob.data = p, 
              icc.graph = gp)

return(object)
}

## -----------------------------------------------------------------------------
irt.iif = function(model) {
  est_theta = IRT.factor.scores(model, 
                              type = "EAP")$EAP
ii = IRT.informationCurves(model, theta = est_theta)

test_info = data.frame(theta = est_theta, 
               info = ii$test_info_curve, 
               se = ii$se_curve)

iif_info = list()
for(i in 1:nrow(ii$info_curves_item)) {
    iif_info[[i]] = data.frame(theta = est_theta)
    iif_info[[i]]$ii_item = ii$info_curves_item[i, ]
    iif_info[[i]]$item = dimnames(ii$info_curves_item)[[1]][i]
}

dat_info = do.call("rbind", iif_info)

info_tot = list(test_info = test_info, 
                item_info = dat_info)

return(info_tot)
}


# import data ------
  data = read.csv("Dati/dataClass.csv", header = T, sep = ",")


vis_data = data
vis_data$id = paste0("s", 1:nrow(data))

vis_data = vis_data[, c(ncol(vis_data), 1:ncol(data))]

vis_data = reshape(vis_data, idvar = "id", direction = "long", varying = list(2:ncol(vis_data)), v.names = "resp", 
        times = names(vis_data)[-1], timevar = "item")

prop_data = data.frame(table(vis_data$item, vis_data$resp))

colnames(prop_data)[1:2] = c("item", "response")

prop_data$proportion = prop_data$Freq/nrow(data)

ggplot(prop_data, 
       aes(x = item, y=proportion,  
 
           fill = response)) + geom_bar(stat = "identity") + 
  theme(axis.text = element_text(angle = 90)) + geom_hline(aes(yintercept=.50)) + theme_bw() 



## -----------------------------------------------------------------------------
sbj_data = data
sbj_data$id = paste0("s", 1:nrow(data))
sbj_data = sbj_data[, c(ncol(sbj_data), 1:ncol(data))]

sbj_data$sum = rowSums(sbj_data[,-1])

boxplot(sbj_data$sum)


## -----------------------------------------------------------------------------
m1pl = tam.mml(data, verbose = F)

summary(m1pl)



## -----------------------------------------------------------------------------
f.m1pl = tam.modelfit(m1pl, progress = F)

f.m1pl$statlist



f.m1pl$modelfit.test

## -----------------------------------------------------------------------------
item.fit.1pl = IRT.itemfit(m1pl)

item.fit.1pl$chisquare_stat

item.fit.1pl$RMSD

item.fit.1pl$RMSD_summary


## -----------------------------------------------------------------------------
irt.icc(m1pl)$icc.graph + theme_classic() 

## -----------------------------------------------------------------------------
m2pl = tam.mml.2pl(data, irtmodel = "2PL", verbose = F)

summary(m2pl)

## -----------------------------------------------------------------------------
f.m2pl = tam.modelfit(m2pl, progress = F)

f.m2pl$statlist



f.m2pl$modelfit.test


## -----------------------------------------------------------------------------
item.fit.2pl = IRT.itemfit(m2pl)

item.fit.2pl$chisquare_stat

item.fit.1pl$RMSD

item.fit.1pl$RMSD_summary


## -----------------------------------------------------------------------------

irt.icc(m2pl)$icc.graph + theme_classic()



## -----------------------------------------------------------------------------

m3pl = tam.mml.3pl(data, est.guess = colnames(data),
                     verbose = F)

summary(m3pl)


## -----------------------------------------------------------------------------
f.m3pl = tam.modelfit(m3pl)

f.m3pl$statlist



f.m3pl$modelfit.test


## -----------------------------------------------------------------------------
item.fit.3pl = IRT.itemfit(m3pl)

item.fit.3pl$chisquare_stat

item.fit.3pl$RMSD

item.fit.3pl$RMSD_summary


## -----------------------------------------------------------------------------

irt.icc(m3pl)$icc.graph + theme_bw()

## -----------------------------------------------------------------------------
IRT.compareModels(m1pl, m2pl, m3pl)

## -----------------------------------------------------------------------------

item_lab = paste(colnames(data), collapse = " + ")
form = paste("latent =~", item_lab)

form


## -----------------------------------------------------------------------------
model = cfa(form, data = data,  ordered = colnames(data))
summary(model, fit.measures = T)

## -----------------------------------------------------------------------------
mono_check = check.monotonicity(data)
summary(mono_check)



## -----------------------------------------------------------------------------
f.m2pl$Q3_summary

## -----------------------------------------------------------------------------


info2pl = irt.iif(m2pl)

ggplot(info2pl$item_info, 
       aes(x = theta, y = ii_item, group = item, color = item)) + geom_line(lwd = 1) + theme_bw()



## ----out.width="60%"----------------------------------------------------------

ggplot(info2pl$test_info, 
       aes(x = theta, y = info)) + geom_line(lwd = 2) + theme_bw()



## ----out.width="60%"----------------------------------------------------------

ggplot(info2pl$test_info, 
       aes(x = theta, y = se, col = "red")) + geom_line(lwd = 2) + theme_bw() + theme(legend.position = "none")



