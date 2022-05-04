setwd("~/RProjects/AISeminarska")

#install.packages("lubridate")
#install.packages(c("CORElearn", "e1071", "randomForest", "kernlab", "nnet"))


podatki <- read.csv("dataSem1.txt", stringsAsFactors = T)
podatki$datum <- as.Date(podatki$datum)
summary(podatki)
vzh <- podatki$regija=="vzhodna"
zah <- podatki$regija=="zahodna"
library(showtext)
font_add(family = "Arial", regular = "/System/Library/Fonts/Supplemental/Arial.ttf") ## here is the path to the font to add.
showtext.auto()
meseci <- seq( from=1, to=12, by=1)
library(lubridate)
tmp2 <- c()
for (mesec in meseci) {
  tmp <- sum(podatki$poraba[vzh & (month(podatki$datum) == mesec)])
  tmp2 <- c(tmp2, tmp)
}
plot(xaxt = 'n', tmp2, type = "b", xlab="Meseci", ylab = "Skupna poraba posamenze regije", main ="Poraba po posameznih regiji po mesecih")

tmp2 <- c()
for (mesec in meseci) {
  tmp <- sum(podatki$poraba[zah & (month(podatki$datum) == mesec)])
  tmp2 <- c(tmp2, tmp)
}

lines(meseci, tmp2, type = "b", col ="red")
axis(1,at=seq(12),labels=meseci_imena, las=2)


sum(month(podatki$datum) ==1 & vzh) # za maj manjka veliko meritev


izobrazevalna <- podatki$namembnost == "izobrazevalna"
javno_storitvena <- podatki$namembnost == "javno_storitvena"
kulturno_razvedrilna <- podatki$namembnost == "kulturno_razvedrilna"
poslovna <- podatki$namembnost == "poslovna"
stanovanjska <- podatki$namembnost == "stanovanjska"

tmp2 <- c()
for (mesec in meseci) {
  tmp <- sum(podatki$poraba[izobrazevalna & (month(podatki$datum) == mesec)])
  tmp2 <- c(tmp2, tmp)
}
plot(xaxt = 'n', tmp2, type = "b", xlab="Meseci", ylab = "Skupna poraba po namembnosti", main ="Poraba po posamezni namembnosti po mesecih",ylim=range( c(0, tmp2) ))
tmp2

tmp2 <- c()
for (mesec in meseci) {
  tmp <- sum(podatki$poraba[javno_storitvena & (month(podatki$datum) == mesec)])
  tmp2 <- c(tmp2, tmp)
}
lines(meseci, tmp2, type = "b", col = "red")
tmp2
tmp2 <- c()
for (mesec in meseci) {
  tmp <- sum(podatki$poraba[kulturno_razvedrilna & (month(podatki$datum) == mesec)])
  tmp2 <- c(tmp2, tmp)
}
lines(meseci,tmp2, type = "b", col = "green")

tmp2 <- c()
for (mesec in meseci) {
  tmp <- sum(podatki$poraba[poslovna & (month(podatki$datum) == mesec)])
  tmp2 <- c(tmp2, tmp)
}
lines(meseci, tmp2, type = "b", col = "blue")

tmp2 <- c()
for (mesec in meseci) {
  tmp <- sum(podatki$poraba[stanovanjska & (month(podatki$datum) == mesec)])
  tmp2 <- c(tmp2, tmp)
}

lines(meseci, tmp2, type = "b", col = "yellow")
meseci_imena <- c("Januar", "Februar", "Marec", "April", "Maj", "Junij", "Julij", "Avgust", "September", "Oktober", "November","December")
axis(1,at=seq(12),labels=meseci_imena, las=2)

sum(month(podatki$datum) == 1 & izobrazevalna)

sum(month(podatki$datum) == 5 & izobrazevalna) #manjkajo predvsem podatki od poletnih mesecev pri izobra?evalnih ustanovah

sum(month(podatki$datum) == 1 & poslovna)

sum(month(podatki$datum) == 5 & poslovna)

teden <-  c("ponedeljek", "torek", "sreda", "?etrtek", "petek", "sobota", "nedelja");

tmp2 <- c()
for (dan in teden) {
  tmp <- sum(podatki$poraba[weekdays(podatki$datum) == dan])
  tmp2 <- c(tmp2, tmp)
}
plot(tmp2,xaxt = 'n', type = "b", xlab="Dan", ylab = "Skupna poraba po dnevih", main ="Poraba po posameznih dnevih v tednu")
axis(1,at=seq(7),labels=teden)

tmp2

#2. naloga
podatki$vikend <- c(weekdays(podatki$datum)=="sobota" | weekdays(podatki$datum) == "nedelja")

#################
#
# Letni ?asi:
# pomlad: 21.3-20.6
# poletje: 21.6-22.9
# jesen: 23.9-20.12
# zima: 21.12-20.3
#
#################
letni_cas <- c()
#for(dan in podatki$datum){
#  if(dan < as.Date("2016-03-21"))
#    letni_cas <- c(letni_cas,"zima")
#  else if(dan < as.Date("2016-06-21"))
#    letni_cas <- c(letni_cas,"pomlad")
#  else if(dan < as.Date("2016-09-23"))
#    letni_cas <- c(letni_cas,"poletje")
#  else if(dan < as.Date("2016-12-21"))
#    letni_cas <- c(letni_cas,"jesen")
#  else
#    letni_cas <- c(letni_cas,"zima")
#  }

letni_cas <- c(podatki$datum<as.Date("2016-03-21"))
letni_cas <- letni_cas + c(podatki$datum<as.Date("2016-06-21"))
letni_cas <- letni_cas + c(podatki$datum<as.Date("2016-09-23"))
letni_cas <- letni_cas + c(podatki$datum<as.Date("2016-12-21"))

letni_cas <- as.factor(letni_cas)
levels(letni_cas) <- c("zima","pomlad","poletje","jesen","zima")
podatki$letni_cas <- letni_cas
  

ure <- c()
#for (ura in podatki$ura) {
#  if(ura <= 11)
#    ure <- c(ure,"dopoldan")
#  else
#    ure <- c(ure,"popoldan")
#}
ure <- c(podatki$ura <= 11)
ure <- as.factor(ure)
levels(ure) <- c("dopoldan","popoldan")
podatki$del_dneva <- ure

podatki$poletne_pocitnice <- c(podatki$datum > as.Date("2016-06-24") & podatki$datum <as.Date("2016-09-01"))

presnji_dan <- c()
for(r in 1:nrow(podatki)) {
    podatek_presnji_dan <-  podatki$poraba[podatki$datum == (podatki$datum[r] - 1) & podatki$ura == podatki$ura[r] & podatki$stavba == podatki$stavba[r]]
    if(length(podatek_presnji_dan) == 0)
      presnji_dan <- c(presnji_dan,NA)
    else
      presnji_dan <- c(presnji_dan,podatek_presnji_dan)
}
podatki$poraba_presnji_dan <- presnji_dan

summary(podatki)

podatki$vikend <- as.factor(podatki$vikend)
podatki$poletne_pocitnicecitnice <- as.factor(podatki$poletne_pocitnice)
podatki$ura <- as.factor(podatki$ura)


set.seed(0)
sel <- sample(1:nrow(podatki), as.integer(nrow(podatki) * 0.7), replace=F)
train <- podatki[sel,-15]
test <- podatki[-sel,-15]

library(rpart)
library(rpart.plot)


source("wrapper.r")

myTrainFunc <- function(formula, traindata)
{
  rpart(formula, traindata)	
}


#
# Funkcija za pridobivanje napovedi modela (razredi)
#

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}


#
# Atribute lahko izberemo glede na klasifikacijsko tocnost modela
#

myEvalFunc <- function(predicted, observed, trained)
{
  # vracamo napako modela, saj wrapper minimizira vrednost ocene
  1.0 - mean(observed == predicted)	
}





# Lahko napisemo funkcijo za izracun klasifikacijske tocnosti
CA <- function(obs, pred)
{
  tab <- table(obs, pred)
  
  sum(diag(tab)) / sum(tab)
}

#
# Za dvorazredne probleme lahko uporabimo dodatne mere:
#
# Senzitivnost - odstotek pravilno klasificiranih pozitivnih primerov
#
# Specificnost - odstotek pravilno klasificiranih negativnih primerov
#
# Preciznost - odstotek pravilno klasificiranih primerov, ki so bili klasificirani kot pozitivni
#

# Funkcija za izracun senzitivnosti modela
Sensitivity <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  
  tab[pos.class, pos.class] / sum(tab[pos.class,])
}

# Funkcija za izracun specificnosti modela
Specificity <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  neg.class <- which(row.names(tab) != pos.class)
  
  tab[neg.class, neg.class] / sum(tab[neg.class,])
}

# Funkcija za izracun preciznosti modela
Precision <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  
  tab[pos.class, pos.class] / sum(tab[, pos.class])
}

# Funkcija za izracun Brierjeve mere
brier.score <- function(obsMat, predMat)
{
  sum((obsMat - predMat) ^ 2) / nrow(predMat)
}

inf.score <- function(trainClass, testClass, predMat)
{
  result <- 0
  
  priors <- table(trainClass)/length(trainClass)
  
  for (i in 1:nrow(predMat))
  {
    p.prior <- priors[[testClass[i]]]
    p.posterior <- predMat[i, testClass[i]]
    
    if (p.posterior >= p.prior)
      result <- result - log2(p.prior) + log2(p.posterior)
    else
      result <- result + log2(1-p.prior) - log2(1-p.posterior)				
  }
  
  result/nrow(predMat)
}


set.seed(0)
wrapper( norm_poraba ~ ., train, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)
#best model: estimated error =  0.3124178 , selected feature subset =  norm_poraba ~ poraba_presnji_dan + povrsina + datum + ura + regija + stavba + namembnost + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + vikend + letni_cas + del_dneva + poletne_pocitnice

sum(podatki$norm_poraba == "SREDNJA") / length(podatki$norm_poraba)


dt <- rpart(norm_poraba ~ ., train)
rpart.plot(dt)
observed <- test$norm_poraba
predicted <- predict(dt, test, type="class")

q <- observed == predicted
sum(q)/length(q)

table(observed, predicted)
Sensitivity(observed, predicted, "Large")
Specificity(observed, predicted, "Large")
Precision(observed, predicted, "Large")


dt <- rpart(norm_poraba ~ poraba_presnji_dan, train)
rpart.plot(dt)
observed <- test$norm_poraba
predicted <- predict(dt, test, type="class")

q <- observed == predicted
sum(q)/length(q)





dt <- rpart(norm_poraba ~ poraba_presnji_dan + povrsina + datum + ura + regija + stavba + namembnost + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra + vikend + letni_cas + del_dneva + poletne_pocitnice, train)
rpart.plot(dt)
observed <- test$norm_poraba
predicted <- predict(dt, test, type="class")

q <- observed == predicted
sum(q)/length(q)



dt <- rpart(norm_poraba ~ poraba_presnji_dan + povrsina, train)
rpart.plot(dt)
observed <- test$norm_poraba
predicted <- predict(dt, test, type="class")

q <- observed == predicted
sum(q)/length(q)

summary(podatki)


dt <- rpart(norm_poraba ~ ., train,cp=0)
rpart.plot(dt)

# rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
printcp(dt)
tab <- printcp(dt)

observed <- test$norm_poraba
predicted <- predict(dt, test, type="class")

q <- observed == predicted
sum(q)/length(q)


row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th

# porezemo drevo z izbrano nastavitvijo
dt <- prune(dt, cp=th)
rpart.plot(dt)

predicted <- predict(dt, test, type="class")
CA(observed, predicted)


summary(podatki)



library(CORElearn)
dt <- CoreModel(norm_poraba ~ ., data = train, model="tree")
predicted <- predict(dt, test, type="class")
CA(observed, predicted)
library(nnet)

predMat <- predict(dt, test, type = "prob")
obsMat <- class.ind(test$norm_poraba)
brier.score(obsMat, predMat)


#
#
# NAIVNI BAYESOV KLASIFIKATOR
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(e1071)

nb <- naiveBayes(norm_poraba ~ ., data = train)
predicted <- predict(nb, test, type="class")
CA(observed, predicted)

predMat <- predict(nb, test, type = "raw")
brier.score(obsMat, predMat)



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
nb <- CoreModel(norm_poraba ~ ., data = train, model="bayes")
predicted <- predict(nb, test, type="class")
CA(observed, predicted)

predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat)



#
#
# K-NAJBLIZJIH SOSEDOV
#
#

# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
knn <- CoreModel(norm_poraba ~ ., data = train, model="knn", kInNN = 5)
predicted <- predict(knn, test, type="class")
CA(observed, predicted)

predMat <- predict(knn, test, type = "prob")
brier.score(obsMat, predMat)


#
#
# NAKLJUCNI GOZD
#
#

# gradnja modela s pomocjo knjiznice "randomForest"

library(randomForest)

rf <- randomForest(norm_poraba ~ ., data = train, na.action=na.exclude)
predicted <- predict(rf, test, type="class")
CA(observed, predicted)

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat)


library(nnet)
set.seed(0)
nn <- nnet(norm_poraba ~ poraba_presnji_dan+namembnost+poletne_pocitnice, data = train, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, test, type = "class")
CA(observed, predicted)

###########################################################

myTrainFunc2 <- function(formula, traindata)
{
  randomForest(formula, traindata, na.action=na.exclude)	
}

set.seed(0)
wrapper( norm_poraba ~ ., train, myTrainFunc2, myPredictFunc, myEvalFunc, cvfolds=10)


podatki <- na.omit(podatki)

# srednja absolutna napaka
mae <- function(obs, pred)
{
  mean(abs(obs - pred))
}

# srednja kvadratna napaka
mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}

# relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) 
{
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) 
{
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}


set.seed(0)
sel <- sample(1:nrow(podatki), as.integer(nrow(podatki) * 0.7), F)
train <- podatki[sel, -16]
test <- podatki[-sel, -16]

library(randomForest)

rf.model <- randomForest(poraba ~ ., train)
predicted <- predict(rf.model, test)
mae(test$poraba, predicted)
rmae(test$poraba, predicted, mean(train$poraba))
