zarobki = c(45617, 7166, 18594, 2236, 1278, 19828, 4033, 28151 , 2414, 3800)
n = length(zarobki)
srednia = mean(zarobki)

set.seed(42) 

# --- A ---
od_populacji = 15000

oblicz_Z = function(poziomUfnosci){
  alpha = 1 - poziomUfnosci
  zScore = qnorm(1 - alpha/2)
  blad = od_populacji / sqrt(n)
  
  marginesBledu = zScore * blad
  dolnaGranica = srednia - marginesBledu
  gornaGranica = srednia + marginesBledu
  
  return(c(dolnaGranica, gornaGranica))
}

ci_90_Z = oblicz_Z(0.90)
ci_95_Z = oblicz_Z(0.95)


# --- B ---
od_probki = sd(zarobki)

oblicz_t = function(poziomUfnosci){
  alpha = 1 - poziomUfnosci
  df = n - 1
  tScore = qt(1 - alpha/2, df)
  blad = od_probki / sqrt(n)
  
  marginesBledu = tScore * blad
  dolnaGranica = srednia - marginesBledu
  gornaGranica = srednia + marginesBledu
  
  return(c(dolnaGranica, gornaGranica))
}

ci_90_t = oblicz_t(0.90)
ci_95_t = oblicz_t(0.95)


# --- C ---
par(mfrow = c(1, 2)) 

hist(zarobki, 
     main = "Histogram zarobków", 
     xlab = "Zarobki", 
     ylab = "Częstość",
     col = "lightblue", 
     border = "white",
     breaks = 5)

qqnorm(zarobki, main = "Wykres Q-Q")
qqline(zarobki, col = "red", lwd = 2)

par(mfrow = c(1, 1)) 

B = 10000 

bootstrapSrednie = replicate(B, {
  probkaBoot = sample(zarobki, size = n, replace = TRUE) 
  mean(probkaBoot) 
})

ci_90_boot = quantile(bootstrapSrednie, probs = c(0.05, 0.95))
ci_95_boot = quantile(bootstrapSrednie, probs = c(0.025, 0.975))



formatuj_przedzial = function(przedzial) {
  sprintf("[%.2f, %.2f]", przedzial[1], przedzial[2])
}

cat("--- Porównanie przedziałów ufności dla średniej ---\n\n")

cat(sprintf("Średnia z próbki: %.2f\n", srednia))
cat(sprintf("Odch. std. z próbki: %.2f\n\n", od_probki))

cat("Metoda A (Z-test, znane σ = 15000):\n")
cat("  90% CI:", formatuj_przedzial(ci_90_Z), "\n")
cat("  95% CI:", formatuj_przedzial(ci_95_Z), "\n\n")

cat("Metoda B (t-Studenta, nieznane σ):\n")
cat("  90% CI:", formatuj_przedzial(ci_90_t), "\n")
cat("  95% CI:", formatuj_przedzial(ci_95_t), "\n\n")

cat("Metoda C (Bootstrap, percentylowy):\n")
cat("  90% CI:", formatuj_przedzial(ci_90_boot), "\n")
cat("  95% CI:", formatuj_przedzial(ci_95_boot), "\n")