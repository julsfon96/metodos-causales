# Usanndo Diferencias en Diferencias 

# Cargamos la base de datos

banks <- read.csv("data/banks.csv")
attach(banks)

str(banks)

banks$date <- ISOdate(year, month, day)

head(banks)

banks$lbib6 <- log(bib6)
banks$lbio6 <- log(bio6)
banks$lbib8 <- log(bib8)
banks$lbio8 <- log(bio8)

attach(banks)

plot(date, lbib6, type = "l", col = "blue", lwd = "3")
lines(date, lbib8, col = "red", lwd = "3")


plot(date, lbio6, type = "l", col = "blue", lwd = "3")
lines(date, lbio8, col = "red", lwd = "3")


# Crear contrafactual

banks <- banks[month == 7 & day == 1,]
diff <- bib8 - bib6
bibc <- bib6 * (year == 1929) + (bib8 - diff[2]) * (year >= 1930)

ldiff <- lbib8 - lbib6
lbibc <- lbib6 * (year == 1929) + (lbib8 - ldiff[2]) * (year >= 1930)

# plot levels
attach(banks)
plot(date, bib8, type = "l", col = "red", ylim = c(80,170))
lines(date, bib6, col = "blue")
lines(date, bibc, col = "blue", lty = "dashed")

######################################

dd <- (bib6[match(1931,year)] - bib6[match(1930,year)]) - (bib8[match(1931,year)] - bib8[match(1930,year)])
dd

dd <- (bib6[match(1931,year)] - bib8[match(1931,year)]) - (bib6[match(1930,year)] - bib8[match(1930,year)])
dd


#########################################
banksLong <- reshape2::melt(banks[c(1,6:13)], id.vars = "date")

banksLong$bib6 <- ifelse(banksLong$variable == "bib6",1,0)

bnk <- banksLong[banksLong$variable %in% c("bib6", "bib8"),]
bnk$post <- ifelse(bnk$date >= "1931-07-01",1,0)

attach(bnk)

DDMi <- lm(value ~ bib6 + post + bib6 * post)
summary(DDMi)
