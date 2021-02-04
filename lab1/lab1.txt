#a)

a <- 4*sin(3.141592653589538208792)
b <- 3*a
max(a,b)

#b)

?max()

#c)

a <- c(90^2:115^2)
sum(a)

#d)

??max

#e)

setwd("C:/Users/blond/OneDrive/Pulpit/projekt")
a <- paste ("lodówka", "z", "najwiêksz¹", "pojemnoœci¹", sep=" ")
a
save(a, file= "workspace.RData")
remove(a)
a
load("workspace.RData")
a

#f)

install.packages("gridExtra")
library("gridExtra")
g <- tableGrob(rivers)
grid.arrange(g)

#g)

a <- seq(1000, 850, -2)

#h)

a <- c(30:5)
b <- c(11:23)
d <- c(b, a)
d

#i)

nazwy <- c('Samsung', 'Beko','Elektrolux',
           'Amica', 'SamsungNoFrost', 'LG',
           'Bosh', 'Ravason', 'MPM ',
           'Whirlpool')
poj_uzytkowa_chlodziarki <- c(240, 220, 200, 190, 210, 215, 50, 185, 160, 297)
poj_uzytkowa_zamrazarki <- c(130, 140, 100, 150, 140, 135, 145, 100, 115, 150)
cena <- c(2399, 1939, 2499, 1799, 1399, 1995, 2199, 699, 979, 2100)
status_opinni <- c(45, 23, 1, 300, 2, 220, 64, 16, 7, 48)
lodowki <- data.frame(nazwy, poj_uzytkowa_chlodziarki, poj_uzytkowa_zamrazarki, cena, status_opinni)
mean(lodowki$cena)

#j)

newRow <- data.frame(nazwy = 'INdesit', poj_uzytkowa_chlodziarki = 259, poj_uzytkowa_zamrazarki = 150, cena = 2899, status_opinni = 74)
lodowki <- rbind(lodowki, newRow)
mean(lodowki$cena)

#k)

lodowki$ocena <- c(5, 5, 3, 5, 4, 5, 5, 5, 3, 2, 1)
aggregate(lodowki$cena, list(lodowki$ocena), mean)


#l)

newRow <- data.frame(nazwy = 'INdesit', poj_uzytkowa_chlodziarki = 259, poj_uzytkowa_zamrazarki = 150, cena = 2899, status_opinni = 74, ocena = 5)
lodowki <- rbind(lodowki, newRow)

newRow <- data.frame(nazwy = 'Bosh Frosty', poj_uzytkowa_chlodziarki = 230 , poj_uzytkowa_zamrazarki = 155, cena = 1899, status_opinni = 78, ocena = 5)
lodowki <- rbind(lodowki, newRow)

newRow <- data.frame(nazwy = 'INdesit Mini', poj_uzytkowa_chlodziarki = 129 , poj_uzytkowa_zamrazarki = 50, cena = 670, status_opinni = 64, ocena = 3)
lodowki <- rbind(lodowki, newRow)

newRow <- data.frame(nazwy = 'Samsung Nofrost Max', poj_uzytkowa_chlodziarki = 290 , poj_uzytkowa_zamrazarki = 250, cena = 3899, status_opinni = 51, ocena = 5)
lodowki <- rbind(lodowki, newRow)

dane <- aggregate(lodowki$status_opinni, list(lodowki$ocena), sum)
barplot(dane[,2], names.arg = dane[,1], main = 'Liczba reprezentantow kazdej z ocen')

#m)

#wykres kolowy
labels <- round(dane[,2]/sum(dane[,2]) * 100, 1)
labels <- paste(labels, "%", sep="")
pie(dane[,2], radius = 1, col = rainbow(length(dane[,2])),
    labels = labels)
legend(1.6, 0.8, dane[,1], cex=0.8, fill=rainbow(length(dane[,2])))

#wykres wachlarzowy
install.packages("plotrix")
library(plotrix)
percentage <- table(lodowki$ocena) / length(lodowki$ocena)
fan.plot(percentage, labels = names(percentage), main = "Procentowy udzial oceny")

#n)

new_column <- ifelse(lodowki$status_opinni>100,'wiecej 100 opinii', 
                     ifelse(lodowki$status_opinni>=50, '50-100 opinii',
                            ifelse(lodowki$status_opinni>0, 'mniej 50 opinii', 'nie ma')))
lodowki['status_opinii'] <- factor(new_column)

#wykres kolowy
pie(table(lodowki$status_opinii), radius = 1, col = rainbow(length(lodowki$status_opinii)))

#o)

for (i in 1:length(lodowki$nazwy)){
  print(paste(lodowki$nazwy[i], 'posiada ocene klientów', lodowki$ocena[i], 'bo posiada liczbe opinii',
              lodowki$status_opinni[i]))
}

#p)

write.csv(lodowki, 'lodowki.csv')
dane <- read.csv('lodowki.csv')

