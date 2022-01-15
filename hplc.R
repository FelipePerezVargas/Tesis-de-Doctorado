hplc <- read.table("/Users/User/Desktop/MATLAB/hplcfur.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)


plot(hplc[["CON"]],hplc[["FUR"]])
rhp<-lm(hplc[["FUR"]]~hplc[["CON"]]); rhp
round(coefficients(rhp)[2],4)
summary(rhp)
coefficients(rhp)
abline(round(coef(rhp)[1:2],4), lwd=2, col="red")


plot(hplc[["CON"]],hplc[["X5HMF"]])
rhp1<-lm(hplc[["X5HMF"]]~hplc[["CON"]])
round(coefficients(rhp1)[2],4)
summary(rhp1)
coefficients(rhp1)
abline(round(coef(rhp1)[1:2],4), lwd=2, col="red")


plot(hplc[["CON"]],hplc[["X5MF"]])
rhp2<-lm(hplc[["X5MF"]]~hplc[["CON"]])
round(coefficients(rhp2)[2],4)
summary(rhp2)
coefficients(rhp2)
abline(round(coef(rhp2)[1:2],4), lwd=2, col="red")


plot(hplc[["CON"]],hplc[["X2AF"]])
rhp3<-lm(hplc[["X2AF"]]~hplc[["CON"]])
round(coefficients(rhp3)[2],4)
summary(rhp3)
coefficients(rhp3)
abline(round(coef(rhp3)[1:2],4), lwd=2, col="red")


plot(hplc[["CON"]],hplc[["VAI"]])
rhp4<-lm(hplc[["VAI"]]~hplc[["CON"]])
round(coefficients(rhp4)[2],4)
summary(rhp4)
coefficients(rhp4)
abline(round(coef(rhp4)[1:2],4), lwd=2, col="red")


plot(hplc[["CON"]],hplc[["SYR"]])
rhp4<-lm(hplc[["SYR"]]~hplc[["CON"]])
round(coefficients(rhp4)[2],4)
summary(rhp4)
coefficients(rhp4)
abline(round(coef(rhp4)[1:2],4), lwd=2, col="red")