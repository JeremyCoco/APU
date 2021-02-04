library("C50")
library("MASS")
data(Aids2)
head(Aids2)
treeModel <- C5.0(x=Aids2[,-6], y=Aids2$T.categ)
treeModel
summary(treeModel)
plot(treeModel)