library(here)
source(here("skrypty", "generator_danych.R"))

oblicz_grey_zone <- function(dane_pacjentow, docelowe_PPV = 0.70, docelowe_NPV = 0.90) {
  
  p <- mean(dane_pacjentow$prawdziwy_stan == 1)
  szanse_przedtestowe <- p / (1 - p)
  
  LR_plus_cel <- (docelowe_PPV / (1 - docelowe_PPV)) / szanse_przedtestowe
  LR_minus_cel <- ((1 - docelowe_NPV) / docelowe_NPV) / szanse_przedtestowe
  
  progi <- sort(unique(dane_pacjentow$wynik_testu))
  
  LR_plus_empiryczne <- numeric(length(progi))
  LR_minus_empiryczne <- numeric(length(progi))
  
  for (i in seq_along(progi)) {
    c <- progi[i]
    
    diagnoza <- ifelse(dane_pacjentow$wynik_testu >= c, 1, 0)
    
    TP <- sum(diagnoza == 1 & dane_pacjentow$prawdziwy_stan == 1)
    FP <- sum(diagnoza == 1 & dane_pacjentow$prawdziwy_stan == 0)
    TN <- sum(diagnoza == 0 & dane_pacjentow$prawdziwy_stan == 0)
    FN <- sum(diagnoza == 0 & dane_pacjentow$prawdziwy_stan == 1)
    
    Sensitivity <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    Specificity <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
    
    LR_plus_empiryczne[i] <- ifelse(Specificity == 1, Inf, Sensitivity / (1 - Specificity))
    LR_minus_empiryczne[i] <- ifelse(Specificity == 0, Inf, (1 - Sensitivity) / Specificity)
  }
  
  idx_U <- which.min(abs(LR_plus_empiryczne - LR_plus_cel))
  c_U <- progi[idx_U]
  
  idx_L <- which.min(abs(LR_minus_empiryczne - LR_minus_cel))
  c_L <- progi[idx_L]
  
  return(c(min(c_L, c_U), max(c_L, c_U)))
}

liczba_iteracji <- 10000
n_pacjentow <- 500

wyniki_GZ <- data.frame(
  iteracja = 1:liczba_iteracji,
  c_L = numeric(liczba_iteracji),
  c_U = numeric(liczba_iteracji),
  szerokosc_strefy = numeric(liczba_iteracji),
  odsetek_w_szarej = numeric(liczba_iteracji),
  dokladnosc_poza = numeric(liczba_iteracji)
)

for (i in 1:liczba_iteracji) {
  #set.seed(1000 + i)
  dane_symulacyjne <- generator_danych(n_prob = n_pacjentow)
  
  granice <- oblicz_grey_zone(dane_pacjentow = dane_symulacyjne)
  c_L_obecne <- granice[1]
  c_U_obecne <- granice[2]
  
  wyniki_GZ$c_L[i] <- c_L_obecne
  wyniki_GZ$c_U[i] <- c_U_obecne

  wyniki_GZ$szerokosc_strefy[i] <- c_U_obecne - c_L_obecne
#szerokosc
  w_strefie <- sum(dane_symulacyjne$wynik_testu >= c_L_obecne & dane_symulacyjne$wynik_testu <= c_U_obecne)
#ilu "wpada" do szarej strefy
  wyniki_GZ$odsetek_w_szarej[i] <- w_strefie / n_pacjentow
  
#dokladnosc
  pacjenci_zdecydowani <- subset(dane_symulacyjne, wynik_testu > c_U_obecne | wynik_testu < c_L_obecne)
  
  if (nrow(pacjenci_zdecydowani) > 0) {
    diagnoza_poza <- ifelse(pacjenci_zdecydowani$wynik_testu > c_U_obecne, 1, 0)
    poprawne_diagnozy <- sum(diagnoza_poza == pacjenci_zdecydowani$prawdziwy_stan)
    wyniki_GZ$dokladnosc_poza[i] <- poprawne_diagnozy / nrow(pacjenci_zdecydowani)
  } else {
    wyniki_GZ$dokladnosc_poza[i] <- NA 
  }
  
  if (i %% 1000 == 0) {
    cat("Policzono i zmierzono", i, "z", liczba_iteracji, "iteracji\n")
  }
}

saveRDS(wyniki_GZ, here("wyniki", "wyniki_symulacji_GZ_metryki.rds"))
