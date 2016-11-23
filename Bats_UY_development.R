## Bats_UY A machine learning algorithm for bat identification in Uruguay
##    Copyright (C) 2016  German Botto Nu?ez - Universidad de la Republica
##
##
##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
##    germanbotto@gmail.com
##    https://sites.google.com/site/germanbotto/home


library(kernlab)
library(randomForest)
load(raw_data.RData)

#### VARIABLE SELECTION MODEL ####
var_sel<-randomForest(SP~ 
               CallDuration + Fc + HiFreq + LowFreq + 
               Bndwdth + FreqMaxPwr + FreqKnee + StartF + 
               EndF + StartSlope + EndSlope + FreqLedge + FreqCtr, 
             data = bib2016_2,importance=T, proximity=T, na.action=na.omit,
             ntree=5000)

#### MULTICLASS MODELS (RF & SVM) ####

## RANDOM FOREST
multiclas<-randomForest(SP~ 
                          CallDuration + Fc + HiFreq + LowFreq + 
                          Bndwdth + FreqMaxPwr + FreqKnee + StartF + 
                          EndF + StartSlope + EndSlope + FreqLedge + FreqCtr, 
                        data = train,importance=T, proximity=T, na.action=na.omit,
                        ntree=5000)

mult_test<-predict(multiclas, newdata=test)
table(as.character(mult_test)==as.character(test$SP)) ## ERROR ESTIMATION (9.38%)

## SUPPORTING VECTOR MACHINES
multi_svm<-ksvm(SP~CallDuration + Fc + HiFreq + 
                  LowFreq + Bndwdth + FreqMaxPwr + 
                  FreqKnee + StartF + EndF + StartSlope + 
                  EndSlope + FreqLedge + FreqCtr, 
                data = train,kernel = "rbfdot", 
                prob.model = TRUE, C = 1)

mult_test_svm<-predict(multi_svm, newdata=test,type="response")
table(as.character(mult_test_svm)==as.character(test$SP)) ## ERROR ESTIMATION (11.72%)

#### CONSENSUS OVER DICHOTOMOUS MODELS ####

## CONSENSUS FUNCTION 
murcis<-function(mod, datos, umbral = 0.5){ 
  # "mod" IS THE LIST OF DICHOTOMOUS MODELS
  # "datos" IS THE TEST SAMPLE TO BE CLASSIFIED 
  # "umbral" IS THE DESIRED PROBABILITY THRESHOLD FOR CLASSIFICATION
  library(randomForest)
  library(kernlab)
  probs <- (data.frame(x = 1:nrow(datos))) [,FALSE] ## EMPTY PROBABILITY MATRIX
  for(i in 1:length (mod)){
    probs[,i]<-predict(mod[[i]],newdata=datos,type="prob")[,2]
  } # MATRIX OF PROBABILITY OF BELONGING TO EACH INDIVIDUAL CLASS FR EACH OBSERVATION
  predichos <- data.frame(pulso = paste(as.character(datos$Filename),as.character(datos$TimeInFile),sep = "__"),
                          predSP = ifelse(apply(probs[,1:ncol(probs)],1,max)>umbral,
                                          names(mod)[max.col(probs[,1:length (mod)])],
                                          "NO PREDICHO"),
                          probSP = ifelse(apply(probs[,1:ncol(probs)], 1, max) > umbral,
                                          apply(probs[,1:ncol(probs)], 1, max),
                                          NA))# THE RESULTING CLASS IS THE ONE SHOWING THE GREATER PROBABILITY (OVER THE THRESHOLD)
  }

## RANDOM FOREST
lista_sp<-as.vector(unique(train$SP))
modelos<-as.list(NULL)
for(i in 1:length(lista_sp)){ ## ITERATION FOR ADJUST OF i DICHOTOMOUS MODELS (i SPECIES Vs ALL OTHERS)
  train$SP1<-as.factor(ifelse(train$SP==lista_sp[i],1,0))
  modelos[[i]]<-randomForest(SP1~ 
                               CallDuration + Fc + HiFreq + LowFreq + 
                               Bndwdth + FreqMaxPwr + FreqKnee + StartF + 
                               EndF + StartSlope + EndSlope + FreqLedge + FreqCtr, 
                             data = train,importance=T, proximity=T, na.action=na.omit,
                             ntree=5000)
}
names(modelos) <- lista_sp ## NAMING OF MODELS ACCORDING TO CLASS

## SUPPORTING VECTOR MACHINES
modelos_svm <- as.list(NULL)
for(i in 1:length(lista_sp)){ ## ITERATION FOR ADJUST OF i DICHOTOMOUS MODELS (i SPECIES Vs ALL OTHERS)
  train$SP1<-as.factor(ifelse(train$SP==lista_sp[i],1,0))
  modelos_svm[[i]]<-ksvm(SP1~CallDuration + Fc + HiFreq + 
                           LowFreq + Bndwdth + FreqMaxPwr + 
                           FreqKnee + StartF + EndF + StartSlope + 
                           EndSlope + FreqLedge + FreqCtr, 
                         data = train,kernel = "rbfdot", 
                         prob.model = TRUE, C = 1)
}
names(modelos_svm) <- lista_sp ## NAMING OF MODELS ACCORDING TO CLASS

## MODEL EVALUATION WITH TEST SAMPLE
## THE FUNCTION "murcis" ALLOWS THE CONSENSUS OVER MODELS
resultado_rf <- murcis(modelos,test) ## RANDOM FOREST
resultado_svm <- murcis(modelos_svm,test) ## SVM
comp1=data.frame(SP_real=as.character(test$SP), # TABLE FOR VISUAL COMPARISION BETWEEN BOTHS ALGORTITHMS 
                 SP_RF=as.character(resultado_rf$predSP),
                 Prob_RF=resultado_rf$probSP,
                 SP_SVM=as.character(resultado_svm$predSP),
                 Prob_SVM=resultado_svm$probSP)

## ERROR RATES FOR INDIVIDUAL DICHOTOMOUS MODELS USING TEST SAMPLE

## SVM
svm=as.list(NULL)
for (i in 1:8){
  svm[[i]]=predict(modelos_svm[[i]],newdata=subset(test, test$SP==lista_sp[i]))
} 
errores1=as.list(NULL)
for (i in 1:8){
  errores1[[i]]=1-(table(svm[[i]])[2]/length(svm[[i]]))
}
## RANDOM FOREST
rf=as.list(NULL)
for (i in 1:8){
  rf[[i]]=predict(modelos[[i]],newdata=subset(test, test$SP==lista_sp[i]))
} 
errores2=as.list(NULL)
for (i in 1:8){
  errores2[[i]]=1-(table(rf[[i]])[2]/length(rf[[i]]))
}
##COMPARISION BETWEEN BOTHS ALGORTITHMS
errores=data.frame(SVM=unlist(errores1), RF=unlist(errores2)) #  comparacion de los errores

#### THRESHOLD OPTIMIZATION FOR SINGLE MULTICLASS MODEL ####
rf_mult<-predict(multiclas, newdata=test,type="prob")
rf_mult_ev<-data.frame(SP_real=test$SP,
                       SP_pred=dimnames(rf_mult)[[2]][max.col(rf_mult)],
                       prob=apply(rf_mult,1,max))
rf_mult_ev$error<-as.character(rf_mult_ev$SP_real)==as.character(rf_mult_ev$SP_pred) ## CLASSIFICATION ERRORS
lista_fallos <- as.list(NULL)
for (i in 1:101){
  rf_mult_ev$fallo <- ifelse(rf_mult_ev$prob >= seq(0,1,0.01)[i], 
                             ifelse(as.character(rf_mult_ev$SP_real)==as.character(rf_mult_ev$SP_pred),
                                    0,1),
                             0.5)
  lista_fallos[[i]] <- sum(rf_mult_ev$fallo)
  rf_mult_ev$fallo <-rep.int(0,128)
}
seq(0,1,0.01)[which.min(unlist(lista_fallos))] ## THIS IS THE OPTIMAL THRESHOLD (0.43)
plot(seq(0,1,0.01),unlist(lista_fallos), ## PLOTS THE OPTIMIZATION PROCESS
     main = "Threshold optimization for RF Multiclass Model", 
     xlab = "Threshold value", 
     ylab = "Error function",
     type = "l", 
     cex.lab=1.5, 
     cex.main=1.5, 
     col="blue",
     lwd=2, 
     xaxs="i",
     yaxs="r")
points(seq(0,1,0.01)[which.min(unlist(lista_fallos))], ## SHOWS THE FIRST GLOBAL MINIMUM 
       min(unlist(lista_fallos)), 
       pch=20,
       cex=2, 
       col="red")

#### FINAL SINGLE MULTICLASS RF MODEL WITH PROBABILITY THRESHOLD ####
murcis2<-function (model, datos, umbral=0.43){ ## FUNCTION FOR CLASSIFICATION BASED IN ONE MULTICLASS RF MODEL
  ## model IS THE RF ADJUSTED MODEL
  ## datos IS THE DATABASE TO BE CLASSIFIED
  ## umbral IS THE PROBABILITY THRESHOLD (0.43, THE OPTIMUM BY DEFAULT)
  library(randomForest)
  probs<-predict(model, newdata=datos,type="prob")
  preds<-data.frame(pulso = paste(as.character(datos$Filename),as.character(datos$TimeInFile),sep = "__"),
                    pred_SP = ifelse(apply(probs,1,max)>=umbral,
                                     dimnames(probs)[[2]][max.col(probs)], 
                                     "NO PREDICHO"), ## "UNKNOWN" GROUP IS ASSIGNED IF PRABILITY<THRESHOLD 
                    pred_prob = ifelse(apply(probs,1,max)>=umbral,apply(probs,1,max),0))
}