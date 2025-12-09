library(MASS)

data(Cars93)

boxplot(Price~Manufacturer, data = Cars93,
        main = "ceny aut wg prodeucenta",
        las =2, cex.axis = 0.6)

subsetCF = subset(Cars93, Manufacturer %in% c("Chevrolet", "Ford"))
subsetCF$Manufacturer = droplevels(subsetCF$Manufacturer)

boxplot(Price~Manufacturer, data = subsetCF,
        main = "ceny chevroleta i forda",
        col = c("lightgreen", "lightblue"))

Chevrolets93 = subset(Cars93, Manufacturer == "Chevrolet")
Fords93 = subset(Cars93, Manufacturer == "Ford")
ChevNFord93 = subset(Cars93, Manufacturer %in% c("Chevrolet", "Ford"))


mu_0 = 15

prices_chev = Chevrolets93$Price
n = length(prices_chev)
x_bar = mean(prices_chev)
sigma = sd(prices_chev)

t_stat = (x_bar - mu_0) / (sigma/ sqrt(n))

cat("t-statystyka:", t_stat, "\n\n")


df = n - 1

p_value = 2 * (1-pt(abs(t_stat),df))

cat("p-wartosc obliczona: ", p_value, "\n\n")

alpha = 0.05
t_kryt = qt(1-alpha/2,df)
margin_error = t_kryt * (sigma/sqrt(n))

ci_lower = x_bar - margin_error
ci_upper = x_bar + margin_error

cat("95% Przedział ufności: [", ci_lower, ", ", ci_upper, "]\n\n")

test_result = t.test(Chevrolets93$Price, mu = 15)
#print(test_result)


test_less <- t.test(Chevrolets93$Price, mu = 15, alternative = "less")

test_greater <- t.test(Chevrolets93$Price, mu = 15, alternative = "greater")

cat("P-value dla H1: średnia < 15 wynosi:", test_less$p.value, "\n")
cat("P-value dla H1: średnia > 15 wynosi:", test_greater$p.value, "\n\n")



test_2samp_diff <- t.test(Chevrolets93$Price, Fords93$Price, alternative = "two.sided")


test_2samp_less <- t.test(Chevrolets93$Price, Fords93$Price, alternative = "less")


test_2samp_greater <- t.test(Chevrolets93$Price, Fords93$Price, alternative = "greater")

cat("P-value (różne):", test_2samp_diff$p.value, "\n")
cat("P-value (Chevrolet < Ford):", test_2samp_less$p.value, "\n")
cat("P-value (Chevrolet > Ford):", test_2samp_greater$p.value, "\n\n")

cor_test <- cor.test(Cars93$EngineSize, Cars93$Horsepower)

print(cor_test)
cat("Współczynnik korelacji:", cor_test$estimate, "\n")

