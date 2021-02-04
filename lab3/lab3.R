#zad_1

library("neuralnet")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

argument <-as.data.frame(runif(100, min = 1, max = 3))
wynik_funkcji <- cos(argument^2)

argument
wynik_funkcji

zeskalowane.argument <- as.data.frame(lapply(argument, normalize))
zeskalowane.argument

dane_treningowe <- cbind(zeskalowane.argument, wynik_funkcji)
colnames(dane_treningowe) <- c("Argument", "Wynik_funkcji")


net.price <- neuralnet(Wynik_funkcji~Argument, dane_treningowe, hidden = c(3, 2), threshold = 0.01)
plot(net.price)

#zad_2

library("neuralnet")
setwd("C:/Users/blond/OneDrive/Pulpit/semestr 1/APU Analiza procesów uczenia/lab3")
lodowki <- read.csv("lodowki.csv")

cena1 <- normalize(lodowki$cena)
cena1

lodowki <- lodowki[,-5]
lodowki <- lodowki[,-1]
lodowki
pojemnosc_chlodziarki1 <- normalize(lodowki$pojemnosc_uzytkowa_chlodziarki)
pojemnosc_chlodziarki1
pojemnosc_zamrazarki1 <- normalize(lodowki$pojemnosc_uzytkowa_zamrazarki)
pojemnosc_zamrazarki1
lodowki <- cbind(lodowki,cena1,pojemnosc_chlodziarki1,pojemnosc_zamrazarki1)
lodowki

net.price <- neuralnet(cena1~pojemnosc_chlodziarki1+pojemnosc_zamrazarki1,
                       lodowki, hidden = c(3,2), threshold = 0.01)

plot(net.price)
