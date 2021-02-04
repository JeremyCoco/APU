library("mlr")
library("DiscriMiner")
library("rFerns")
library("randomForestSRC")
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}
setwd("C:/Users/blond/OneDrive/Pulpit/semestr 1/APU Analiza procesów uczenia/lab5_2")
data <- read.csv("lodowki.csv")
data$ocena_klientow<-factor(data$ocena_klientow)
data$poj_chlodziarki<-normalize(data$poj_chlodziarki)
data$poj_zamrazarki<-normalize(data$poj_zamrazarki)
data<-data.frame(data)
zadanie = makeClassifTask(id = 'lodowki',
                          data,
                          "ocena_klientow",
                          weights = NULL,
                          blocking = NULL,
                          coordinates = NULL,
                          positive = NA_character_,
                          fixup.data = "warn",
                          check.data = TRUE)
ponowne_probkowanie = makeResampleDesc("Bootstrap", iters = 3)
metody_uczenia <- makeLearners(c("rpart",
                                 "C50","rFerns","randomForestSRC"), type = "classif")
porownanie_metod_uczenia <- benchmark(learners = metody_uczenia,
                                      tasks = zadanie,
                                      resampling =
                                        ponowne_probkowanie)

