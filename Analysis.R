###################################################
# TRAINING SET: V-FOLDS CROSS VALIDATION
###################################################

# setwd("/path/to/your/directory")
test <- read.csv("Howell-test1.csv", header = TRUE, sep= ";")
training <- read.csv("Howell-training.csv", header = TRUE, sep = ",")

# install.packages("tidyverse")
library(tidyverse)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("mixtools")
library(mixtools)
# install.packages("Rmixmod")
library(Rmixmod)
# install.packages("GGally")
library(GGally)
# install.packages("caret")
library(caret)
# install.packages("mclust")
library(mclust)
# install.packages("tidymodels")
library(tidymodels)

###############
# Prima parte #
###############
# Analisi Descrittiva -----------------------------------------------------

training # E' tidy
str(training) # Alcune variabili contengono 0
cor(training[, 5:86])
summary(training)

training$Sex <- as.factor(training$Sex)
training$Sex # 2 Levels: F M

boxplot(training$GOL ~ training$Sex, main = "Boxplot sesso - lunghezza cranio", 
        xlab = "Sesso", ylab = "Lunghezza cranio (GOL)")
# Si nota una differenza tra i due sessi per la lunghezza del cranio
boxplot(training$XCB ~ training$Sex, main = "Boxplot sesso - larghezza cranio", 
        xlab = "Sesso", ylab = "Larghezza cranio (XCB)")
# Si nota una differenza tra i due sessi per la larghezza del cranio
boxplot(training$BBH ~ training$Sex, main = "Boxplot sesso - altezza cranio", 
        xlab = "Sesso", ylab = "Altezza cranio (BBH)")
# Si nota una differenza tra i due sessi per altezza del cranio

plot(training$GOL, training$XCB, col = training$Sex,
     main = "Lunghezza vs Larghezza cranio", xlab = "Lunghezza cranio (GOL)", 
     ylab = "Larghezza cranio (XCB)")

plot(training$GOL, training$BBH, col = training$Sex,
     main = "Lunghezza vs Altezza cranio", xlab = "Lunghezza cranio (GOL)", 
     ylab = "Altezza cranio (BBH)")

plot(training$XCB, training$BBH, col = training$Sex,
     main = "Larghezza vs Altezza cranio", xlab = "Larghezza cranio (XCB)", 
     ylab = "Altezza cranio (BBH)")

# Ricerca di cluster all'interno delle distribuzioni delle variabili:
ggpairs(training[5:15])
ggpairs(training[15:25])
ggpairs(training[25:35])
ggpairs(training[35:45])
ggpairs(training[45:55])
ggpairs(training[55:65])
ggpairs(training[65:75])
ggpairs(training[75:86])

sum(training$Sex == "F") # 1156
sum(training$Sex == "M") # 1368
sum(test$Sex == "F") # 157
sum(test$Sex == "M") # 348

# Pulizia dati ------------------------------------------------------------

# Pulizia da dati nulli o scritti in maniera errata e non correggibile
test <- test[,-c(1,4,5,6,7)]
test <- test[-c(501,517,518,519,520,521,522,523,524), ]
test <- na.omit(test)

test$Sex <- as.factor(test$Sex)
test$Sex # 2 Levels: F M
training$Population <- as.factor(training$Population)
test$Tribe <- as.factor(test$Tribe)

# Esclusione variabili "difettose" a causa  della SINOSTOSI:
# In numerosi casi il valore inserito è 0, questo perché in casi di anziani nelle
# intersezioni di suture avviene la sinostosi: le suture si fondono completamente e
# l osso diventa liscio. I crani sono stati misurati su cadaveri quindi per forza una
# gran parte proviene da anziani questo spiega la possibile difficloltà nel misurarlo.
training <- training %>% select(-c("RFA", "RPA", "ROA", "BSA", "SBA", "SLA", "TBA", "LAR", "OSR", "BAR", "BRR"))
test <- test %>% select(-c("RFA", "RPA", "ROA", "BSA", "SBA", "SLA", "LAR", "OSR", "BAR", "BRR", "TBA"))

labels <- select(training, Sex)
labels # Cluster noti

# PCA ---------------------------------------------------------------------

nomi <- names(training[-c(1,2,3,4)]) # Nomi variabili

training_centered <- training %>% 
  select(-c(1:3)) %>%
  group_by(Population) %>%
  mutate(across(all_of(nomi), ~ scale(.)[,1])) %>%
  ungroup() %>%
  select(all_of(nomi))

test_centered <- test %>% 
  group_by(Tribe) %>%
  mutate(across(all_of(nomi), ~ scale(.)[,1])) %>%
  ungroup() %>%
  select(, -c(Tribe))

test_centered <- na.omit(test_centered)

pca <- princomp(training_centered, cor=T)
cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
sort(abs(loadings(pca)[,1]), decreasing = TRUE)
# NAR, GOL, JUB, ZYB, BNL, DKR, NOL, SSR, ZOR, PRR, FMB, AVR, EKB, EKR, ZMR, FMR, AUB, BPL

pca_test <- princomp(test_centered[ ,-1], cor=T)
cumsum(pca_test$sdev^2)/sum(pca_test$sdev^2)
screeplot(pca_test)
sort(abs(loadings(pca_test)[,1]), decreasing = TRUE)

training_centered <- training_centered %>% select(c(NAR, GOL, JUB, ZYB, BNL, DKR, NOL, SSR, ZOR, PRR, FMB, AVR, EKB, EKR, ZMR, FMR, AUB, BPL))
test_centered <- test_centered %>% select(c("Sex", NAR, GOL, JUB, ZYB, BNL, DKR, NOL, SSR, ZOR, PRR, FMB, AVR, EKB, EKR, ZMR, FMR, AUB, BPL))

sum(test_centered$Sex == "F") # 121
sum(test_centered$Sex == "M") # 213

# V-Fold Cross Validation with 18 variabiles ------------------------------

levels(test_centered$Sex) # 2 Levels: F M
labels$Sex <- as.factor(labels$Sex)
levels(labels$Sex) # 2: Levels: F M

# nbCVBlock=10 ------------------------------------------------------------

set.seed(123)
a=b=c <- rep(NA,100)
for(i in 1:length(a)){
  x <- mixmodLearn(training_centered, labels[ ,1],
                   models=mixmodGaussianModel(family="all",
                                              free.proportions = TRUE,
                                              equal.proportions = FALSE),
                   criterion="CV", nbCVBlocks=10)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=test_centered[,2:19],
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b) # Modello con CV minore:
a[24] # Gaussian_pk_L_C
b[24] # CV = 0.1224247
c[24] # 0.8023952

which.max(c) # Modello con accuratezza maggiore:
a[14] # Gaussian_pk_Lk_C
b[14] # CV = 0.1267829
c[14] # 0.8263473


training.10 <- mixmodLearn(training_centered, labels[ ,1],
                 models=mixmodGaussianModel(family="all",
                                            free.proportions = TRUE,
                                            equal.proportions = FALSE),
                 criterion="CV", nbCVBlocks=10)
test.10 <- mixmodPredict(data=test_centered[,2:19],
                   classificationRule=training.10['bestResult'])

# nbCVBlock=8 -------------------------------------------------------------

set.seed(321)
for(i in 1:length(a)){
  x <- mixmodLearn(training_centered, labels[ ,1],
                   models=mixmodGaussianModel(family="all",
                                              free.proportions = TRUE,
                                              equal.proportions = FALSE),
                   criterion="CV", nbCVBlocks=8)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=test_centered[,2:19],
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b) # Modello con CV minore:
a[17] # Gaussian_pk_L_C
b[17] # CV = 0.1220285
c[17] # 0.8023952

which.max(c) # Modello con accuratezza maggiore:
a[6] # Gaussian_pk_Lk_C
b[6] # CV = 0.1267829
c[6] # 0.8263473

# nbCVBlock=5 -------------------------------------------------------------

set.seed(213)
for(i in 1:length(a)){
  x <- mixmodLearn(training_centered, labels[ ,1],
                   models=mixmodGaussianModel(family="all",
                                              free.proportions = TRUE,
                                              equal.proportions = FALSE),
                   criterion="CV", nbCVBlocks=5)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=test_centered[,2:19],
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b) # Modello con CV minore:
a[28] # Gaussian_pk_L_C
b[28] # CV = 0.1220285
c[28] # 0.8023952

which.max(c) # Modello con accuratezza maggiore:
a[15] # Gaussian_pk_Lk_C
b[15] # CV = 0.1283677
c[15] # 0.8263473

# nbCVBlock=15 ------------------------------------------------------------

set.seed(1012)
for(i in 1:length(a)){
  x <- mixmodLearn(training_centered, labels[ ,1],
                   models=mixmodGaussianModel(family="all",
                                              free.proportions = TRUE,
                                              equal.proportions = FALSE),
                   criterion="CV", nbCVBlocks=15)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=test_centered[,2:19],
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b) # Modello con CV minore:
a[10] # Gaussian_pk_L_C
b[10] # CV = 0.1220285
c[10] # 0.8023952

which.max(c) # Modello con accuratezza maggiore:
a[2] # Gaussian_pk_L_D_Ak_D
b[2] # CV = 0.1251981
c[2] # 0.8113771

# nbCVBlock=12 ------------------------------------------------------------

for(i in 1:length(a)){
  x <- mixmodLearn(training_centered, labels[ ,1],
                   models=mixmodGaussianModel(family="all",
                                              free.proportions = TRUE,
                                              equal.proportions = FALSE),
                   criterion="CV", nbCVBlocks=12)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=test_centered[,2:19],
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b) # Modello con CV minore:
a[19] # Gaussian_pk_L_C
b[19] # CV = 0.1212361
c[19] # 0.8023952

which.max(c) # Modello con accuratezza maggiore:
a[80] # Gaussian_pk_Lk_C
b[80] # CV = 0.1263867
c[80] # 0.8263473

# Conclusione -------------------------------------------------------------
# Dopo aver compiuto numerosi tentativi (100 per ogni V), il modello che minimizza il
# CV è sempre Gaussian_pk_L_C con un CV di circa 0.122 e un'accuratezza attorno
# all'80%. Mentre il modello che massimizza l'accuratezza è sempre Gaussian_pk_Lk_C
# e meno frequentemente risulta essere Gaussian_pk_L_D_Ak_D, con un'accuratezza
# di circa 82-83% e un CV di circa 0.125-0.128.
# V-Fold Cross Validation with PCA ----------------------------------------

pca_test <- princomp(test_centered[ ,-1], cor=T)
cumsum(pca_test$sdev^2)/sum(pca_test$sdev^2)
screeplot(pca_test)
sort(abs(loadings(pca_test)[,1]), decreasing = TRUE)

pca_training <- as_tibble(pca$scores[,1:2])
pca_test <- as_tibble(pca_test$scores[,1:2])

# Partition = 10 ----------------------------------------------------------

a = b = c <- rep(NA,200)

set.seed(6543)
for(i in 1:length(a)){
  x <- mixmodLearn(as_tibble(pca_training), labels[ ,1],   
                   models=mixmodGaussianModel(family='all', equal.proportions = F),
                   criterion="CV")
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=pca_test,
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b) # Modello con CV minore:
a[59] # Gaussian_pk_L_D_Ak_D = EVE
b[59] # CV = 0.1454041
c[59] # 0.8113772

which.max(c) # Modello con accuratezza maggiore:
a[1] # Gaussian_pk_Lk_Dk_A_Dk = EEV
b[1] # CV = 0.1473851
c[1] # 0.8113772

# Partition = 20 ----------------------------------------------------------

set.seed(9876)
for(i in 1:length(a)){
  x <- mixmodLearn(as_tibble(pca_training), labels[ ,1],   
                   models=mixmodGaussianModel(family='all', equal.proportions = F),
                   criterion=c('CV'),
                   nbCVBlocks = 20)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data= pca_test,
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b)  
a[48] # Gaussian_pk_L_D_Ak_D = EVE
b[48] # 0.1461965
c[48] # 0.8113772

which.max(c)  
a[1] # Gaussian_pk_Lk_C
b[1] # 0.1481775
c[1] # 0.8203593

# Partition = 60 ----------------------------------------------------------

set.seed(1357)
for(i in 1:length(a)){
  x <- mixmodLearn(as_tibble(pca_training), labels[ ,1],   
                   models=mixmodGaussianModel(family='all', equal.proportions = F),
                   criterion=c('CV'),
                   nbCVBlocks = 60)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=pca_test,
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b)  
a[20] # Gaussian_pk_L_C
b[20] # 0.1465927
c[20] # 0.8083832

which.max(c)  
a[46] # Gaussian_pk_Lk_Dk_A_Dk = EEV
b[46] # 0.1477813
c[46] # 0.8203593

# Partition = 100 ---------------------------------------------------------

set.seed(2468)
for(i in 1:length(a)){
  x <- mixmodLearn(as_tibble(pca_training), labels[ ,1],   
                   models=mixmodGaussianModel(family='all', equal.proportions = F),
                   criterion=c('CV'),
                   nbCVBlocks = 100)
  a[i] <- x@bestResult@model
  b[i] <- x@bestResult@criterionValue[1]
  y <- mixmodPredict(data=pca_test,
                     classificationRule=x['bestResult'])
  c[i] <- mean(as.integer(test_centered$Sex) == y@partition)
  i <- i+1
}

which.min(b)  
a[3] # Gaussian_pk_Lk_B
b[3] # 0.1469889
c[3] # 0.8053892

which.max(c)  
a[77] # Gaussian_pk_Lk_Dk_A_Dk = EEV
b[77] # 0.1477813
c[77] # 0.8203593

# Grafico V-Fold CV (con 18 variabili) ------------------------------------

set.seed(4536)
training.10 <- mixmodLearn(training_centered, labels[ ,1],
                           models=mixmodGaussianModel(family="all",
                                                      free.proportions = TRUE,
                                                      equal.proportions = FALSE),
                           criterion="CV", nbCVBlocks=10)
training.10@bestResult
test.10 <- mixmodPredict(data=test_centered[,2:19],
                         classificationRule=training.10['bestResult'])


# Grafico delle missclassification di mixmodLearn e mixmodPredict:
errori_logici <- as.integer(test_centered$Sex) != as.integer(test.10@partition)
d <- which(errori_logici)

test_centered <- as.data.frame(test_centered) # Assicuriamoci sia un dataframe

ggplot(test_centered, aes(x = test_centered[,2], y = test_centered[,3], col = Sex)) +
  # Punti originali (tutti)
  geom_point(pch = 16, size = 3, alpha = 0.6) + 
  # Ellissi di confidenza (molto utili per vedere la sovrapposizione)
  stat_ellipse(level = 0.95) +
  # Sovrapposizione punti neri (solo gli errori)
  geom_point(data = test_centered[d, ], 
             aes(x = test_centered[d, 2], y = test_centered[d, 3]), 
             col = 'black', pch = 16, size = 3) + 
  scale_colour_manual(values = c("F" = "#c05a9d", "M" = "#3ba5dd")) +
  theme_light() +
  labs(title = "Analisi Missclassification",
       subtitle = "I punti neri indicano le predizioni errate del modello",
       x = "Variabile 1", y = "Variabile 2")

# K-mean clustering -------------------------------------------------------

training.df <- as.data.frame(training_centered)

training.kmeans <- kmeans(training_centered, centers=2, nstart =20)
coordProj (training.df, what="classification",
           classification=training.kmeans$cluster ,
           col=c("green3","red2","dodgerblue2"), symbols=c(17 ,0 ,16),
           sub="(f) k means")
classError (training.kmeans$cluster , labels[ ,1])
# $errorRate: 0.2135499

adjustedRandIndex(training.kmeans$cluster , labels[ ,1])
# 0.3279444

# Conclusione -------------------------------------------------------------

# Dopo aver provato numerosi metodi, il massimo ottenuto è stato circa l'82%
# di accuratezza con il V-Fold Cross Validation sia con 18 variabili che con le
# PCA. Il metodo K-means ha dato risultati non soddisfacenti.

#################
# Seconda Parte #
#################
# Import ------------------------------------------------------------------

rm(list = ls())

test <- read.csv("Howell-test1.csv", header = TRUE, sep= ";")
training <- read.csv("Howell-training.csv", header = TRUE, sep = ",")

# Pulizia training -----------------------------------------------------------------

training <- training[-c(1,3)] %>%
  select(-c(RFA, RPA, ROA, BSA, SBA, SLA, TBA, BAR, LAR, OSR, BRR ))
training$Sex <- as.factor(training$Sex)
training$Population <- as.factor(training$Population)
nomi <- names(training[-c(1,2)]) # nomi variabili
data_centered <- training %>% 
  group_by(Population) %>%
  mutate(across(all_of(nomi), ~ scale(.)[,1])) %>%
  ungroup() %>%
  select(all_of(nomi))

# Pulizia test ------------------------------------------------------------

test$Tribe <- as.factor(test$Tribe)

test <- na.omit(test) %>%
  select(-c(RFA, RPA, ROA, BSA, SBA, SLA, BAR, LAR, OSR, BRR,TBA))
anyNA(test)
test$Sex <- as.factor(test$Sex)
test_sex <- unlist(test[2])

test <- test %>%
  select(,-c(1,2,4,5,6,7))

str(test)
summary(test)
nomi <- names(test[,-1])
test$Sex <-test_sex 
table(test$Tribe)
which(table(test$Tribe)>1)

test1 <- test %>%
  group_by(Tribe) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(Tribe = factor(Tribe)) # Questo ricalcola i livelli presenti
table(test1$Tribe) # Verifica che le tribù con 0 o pochi dati siano sparite

data_centeredd <- test1 %>%
  group_by(Tribe) %>%
  mutate(across(all_of(nomi), ~ scale(.)[,1])) %>%
  ungroup() %>%
  select(all_of(nomi),Sex)
anyNA(data_centeredd)
data_centeredd<-na.omit(data_centeredd)
anyNA(data_centeredd)

# PCA ---------------------------------------------------------------------

pca <- princomp(data_centered, cor=T) # PCA
cumsum(pca$sdev^2)/sum(pca$sdev^2)
screeplot(pca)
pca$scores # Score

# Mclust ------------------------------------------------------------------

sort(abs(loadings(pca)[,1]), decreasing = TRUE)
top_features <- c("NAR", "BNL", "ZOR", "GOL", "NOL", "PRR", "JUB", "ZYB", "SSR", "DKR")

data_ridotto <- data_centered %>% select(all_of(top_features))
pairs(data_ridotto[1:5], col=data_ridotto$class)

mclust_ottimizzato <- Mclust(data_ridotto)
summary(mclust_ottimizzato)

mclust_ottimizzato <- Mclust(data_ridotto, G=2, modelNames = "VEE")
summary(mclust_ottimizzato)

classError(mclust_ottimizzato$classification, training$Sex)$errorRate

# Verifica ----------------------------------------------------------------

error <- numeric(20) 
for (i in 1:20) {
  pca_mclust <- Mclust(data_ridotto, G=2, modelNames = "VEE")
  res <- classError(pca_mclust$classification, training$Sex)$errorRate
  error[i] <- res 
}
mean(error)

# Soluzione 2 -------------------------------------------------------------

sort(abs(loadings(pca)[,1]), decreasing = TRUE)

features_all <- c("NAR","GOL","JUB","ZYB","BNL","DKR","NOL","SSR",
                  "ZOR","PRR","FMB","AUB","BPL","XML","AVR","ZMR",
                  "EKR","FMR","EKB","NPH","FRC","MAB","ZMB","BBH")

risultati <- vector("list", 200)

##### ATTENZIONE ########
# non fare partire il ciclo for perchè dura 1 ora

set.seed(555)
for (i in 1:200) {
  
  error <- numeric(6)   
  
  feat_i <- sample(features_all, 10, replace = FALSE)
  
  data_ridotto <- data_centered %>% select(all_of(feat_i))
  
  mclust_ottimizzato <- Mclust(data_ridotto, G = 2)
  migliore <- mclust_ottimizzato$modelName
  
  error[1] <- classError(
    mclust_ottimizzato$classification,
    training$Sex
  )$errorRate
  
  for (j in 1:5) {
    mclust_ottimizzato <- Mclust(
      data_ridotto,
      G = 2,
      modelNames = migliore
    )
    
    error[j + 1] <- classError(
      mclust_ottimizzato$classification,
      training$Sex
    )$errorRate
  }
  
  risultati[[i]] <- list(
    features   = feat_i,
    mean_error = mean(error),
    model      = migliore
  )
}

mean_errors <- sapply(risultati, function(x) x$mean_error)
best_i <- which.min(mean_errors)
risultati[[best_i]]

# $features [1] "MAB" "FRC" "AUB" "GOL" "NOL" "AVR" "JUB" "ZYB" "NPH" [10] "FMB"

# $mean_error [1] 0.1316693

# $model [1] "VEE"

# Modello finale ----------------------------------------------------------

top_features <- c("MAB", "FRC", "AUB", "GOL", "NOL", "AVR", "JUB", "ZYB", "NPH", "FMB")
data_ridotto <- data_centered %>% select(all_of(top_features))
pairs(data_ridotto, col = training$Sex)
ggpairs(data_ridotto)
set.seed(778)
mclust_ottimizzato <- Mclust(data_ridotto)
summary(mclust_ottimizzato)
classError(mclust_ottimizzato$classification, training$Sex)$errorRate
# 00.1295563
# abbiamo abbassato leggermente l'errore e questa volta non dobbiamo più imporre due gruppi

# Verifica 2 --------------------------------------------------------------

error <- numeric(20) 
for (i in 1:20) {
  p_mclust <- Mclust(data_ridotto, G=2, modelNames = "VEE")
  res <- classError(p_mclust$classification, training$Sex)$errorRate
  error[i] <- res 
}
mean(error)

# KL verification ---------------------------------------------------------

class <- as.factor(mclust_ottimizzato$classification)
levels(class) <- c('M','F')
M <- confusionMatrix(class, training$Sex)
Incertezza<- unname(1- M$overall['Accuracy'])
# Accuracy : 0.8704
#Incertezza:0.1295563
adjustedRandIndex(mclust_ottimizzato$classification, training$Sex) # 0.5486889

mu <- mclust_ottimizzato$parameters$mean          
Sigma <- mclust_ottimizzato$parameters$variance$sigma 
p <- mclust_ottimizzato$parameters$pro            

S1 <- Sigma[,,1]
S2 <- Sigma[,,2]
invS1 <- solve(S1)
invS2 <- solve(S2)
diff_mu <- mu[,1] - mu[,2]
d <- length(top_features) 

KL_d <- 0.5 * (t(diff_mu) %% (invS1 + invS2) %% diff_mu) + 
  0.5 * sum(diag(invS2 %% S1 + invS1 %% S2)) - d

mu_true <- p[1] * mu[,1] + p[2] * mu[,2]

within <- p[1] * S1 + p[2] * S2

term1 <- (mu[,1] - mu_true) %*% t(mu[,1] - mu_true)
term2 <- (mu[,2] - mu_true) %*% t(mu[,2] - mu_true)
between <- p[1] * term1 + p[2] * term2

tot <- within + between

R2t <- 1 - (sum(diag(within)) / sum(diag(tot)))
R2d <- 1 - (det(within) / det(tot))

output <- list(
  R2_Traccia = R2t,
  R2_Determinante = R2d, 
  KL_distance = KL_d
)

print(output)

# Forecast ----------------------------------------------------------------

test_ottimo <- data_centeredd %>%
  select(all_of(top_features))
summary(test_ottimo)
p <- predict(mclust_ottimizzato, test_ottimo)

levels(data_centeredd$Sex) <- c(2,1)

sum(as.factor(p$classification)==as.factor(data_centeredd$Sex))

confusionMatrix(as.factor(p$classification), as.factor(data_centeredd$Sex)) 
adjustedRandIndex (p$classification,data_centeredd$Sex)