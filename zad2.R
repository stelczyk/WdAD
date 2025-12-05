library(MASS)

set.seed(155231) # ustawiam aby wyniki moje ze sprawozdania pokrywaly
                 # sie z tym jak Pan odpali program


# tworze sobie funkcje bo jak mam zrobic 3 razo to samo to wole
# miec do tego funkcje
analiza = function(dane,nazwa) {
  cat("analizujemy", nazwa, "\n")
  
  n = length(dane)
  
  cat("liczebnosc: ", n, "\n" )
  cat("srednia: ", mean(dane), "\n" )
  cat("odchylenie: ", sd(dane), "\n" )
  
  par(mfrow = c(1,2))
  
  # histogram z linia gestosci
  
  hist(dane,
      probability = TRUE,
      main = "histogram",
      xlab = "wartosc",
      col = "lightblue",
      border = 'white')
  
  lines(density(dane), col = "darkblue", lwd = 2)
  
  # wykres kwantyl-kwantyl
  
  qqnorm(dane, main = "wykres kwantyl kwantyl")
  qqline(dane, col = "red", lwd = 2)
  
  # bootstrap 10k
  
  B = 10000
  
  # generujemy probko bootstrap
  # zwracam wektor ktory ma: srednia, odchylenie i wariancja
  
  boot = replicate(B, {
    probka = sample(dane, size = n, replace = TRUE)
    c(mean(probka), sd(probka), var(probka))
  })
  
  # przedzialy ufnosci 95%
  
  ci_srednia = quantile(boot[1,], probs = c(0.025, 0.975))
  ci_sd = quantile(boot[2,], probs = c(0.025, 0.975))
  ci_var= quantile(boot[3,], probs = c(0.025, 0.975))
  
  cat("srednia: [", ci_srednia[1], " , ", ci_srednia[2], "]\n")
  cat("odchylenie: [", ci_sd[1], " , ", ci_sd[2], "]\n")
  cat("wariancja: [", ci_var[1], " , ", ci_var[2], "]\n\n\n")
  
}



analiza(anorexia$Postwt, "zbior: anoreksja")

analiza(Aids2$age, "zbior: aids")

analiza(birthwt$lwt, "zbior: waga matki po porodzie")