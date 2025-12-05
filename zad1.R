set.seed(155231) # ustawiam aby wyniki moje ze sprawozdania pokrywaly
                 # sie z tym jak Pan odpali program

zarobki = c(45617, 7166, 18594, 2236, 1278, 19828, 4033, 28151 , 2414, 3800) # zarobki studentow
n = length(zarobki) # liczba proby
srednia = mean(zarobki) # srednia z proby



#A 
# znamy odchylenie standardowe, rozklad normalny

od_populacji = 15000 

oblicz_Z = function(poziomUfnosci){
  alpha = 1 - poziomUfnosci
  zScore = qnorm(1 - alpha/2) # wartosc krytyczna
  blad = od_populacji / sqrt(n) # blad standardowy sredniej
  
  marginesBledu = zScore * blad # margines bledu
  dolnaGranica = srednia - marginesBledu
  gornaGranica = srednia + marginesBledu
  
  return(c(dolnaGranica, gornaGranica))
}

ci_90_Z = oblicz_Z(0.90)
ci_95_Z = oblicz_Z(0.95)


#B
# nieznane odchylenie standardowe, rozklad t-studenta, n < 30

od_probki = sd(zarobki) # odchylenie standardowe z proby

oblicz_t = function(poziomUfnosci){
  alpha = 1 - poziomUfnosci
  df = n - 1 # stopnie swobody
  tScore = qt(1 - alpha/2, df) # wartosc krytyczna
  blad = od_probki / sqrt(n) # blad standardowy
  
  marginesBledu = tScore * blad
  dolnaGranica = srednia - marginesBledu
  gornaGranica = srednia + marginesBledu
  
  return(c(dolnaGranica, gornaGranica))
}

ci_90_t = oblicz_t(0.90)
ci_95_t = oblicz_t(0.95)


#C
# Bootstrap

par(mfrow = c(1, 2)) # ustawienie okna aby pokazac 2 wykresy obok siebie 

hist(zarobki, 
     main = "Histogram zarobkow", 
     xlab = "Zarobki", 
     ylab = "Czestosc",
     col = "lightblue", 
     border = "white",
     breaks = 5)

qqnorm(zarobki, main = "Wykres kwantyl-kwantyl")
qqline(zarobki, col = "red", lwd = 2)
# punkty na wykresie kwantyl-kwantyl odstaja od czerwonej linii, moze to
# sugerowac, ze dane nie maja rozkladu normalnego, wystepuja wartosci odstajace




par(mfrow = c(1, 1)) 

B = 10000 # liczba powtorzen

bootstrapSrednie = replicate(B, {
  probkaBoot = sample(zarobki, size = n, replace = TRUE) 
  mean(probkaBoot) 
})

ci_90_boot = quantile(bootstrapSrednie, probs = c(0.05, 0.95))
ci_95_boot = quantile(bootstrapSrednie, probs = c(0.025, 0.975))



formatuj_przedzial = function(przedzial) {
  sprintf("[%.2f, %.2f]", przedzial[1], przedzial[2])
}



cat(sprintf("Srednia z probki: %.2f\n", srednia))
cat(sprintf("Odch. std. z probki: %.2f\n\n", od_probki))

cat("Metoda A (Z-test, znane σ = 15000):\n")
cat("  90% CI:", formatuj_przedzial(ci_90_Z), "\n")
cat("  95% CI:", formatuj_przedzial(ci_95_Z), "\n\n")

cat("Metoda B (t-studenta, nieznane σ):\n")
cat("  90% CI:", formatuj_przedzial(ci_90_t), "\n")
cat("  95% CI:", formatuj_przedzial(ci_95_t), "\n\n")

cat("Metoda C (Bootstrap, percentylowy):\n")
cat("  90% CI:", formatuj_przedzial(ci_90_boot), "\n")
cat("  95% CI:", formatuj_przedzial(ci_95_boot), "\n")