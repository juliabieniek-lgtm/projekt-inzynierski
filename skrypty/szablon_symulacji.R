library(here)

source(here("skrypty", "generator_danych.R"))

#z jakims przykładowym czyms lala
oblicz_metode_xyz <- function(dane_pacjentow) {
  #tu będzie kiedyś skomplikowany wzór
  c_L <- 0.40
  c_U <- 0.60
  return(c(c_L, c_U))
}

liczba_iteracji <- 10000
n_pacjentow <- 500
#tabela juz z metrykami
wyniki_symulacji <- data.frame(
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
  
  granice <- oblicz_metode_xyz(dane_pacjentow = dane_symulacyjne) 
  c_L_obecne <- granice[1]
  c_U_obecne <- granice[2]
  
  wyniki_symulacji$c_L[i] <- c_L_obecne
  wyniki_symulacji$c_U[i] <- c_U_obecne
  
  wyniki_symulacji$szerokosc_strefy[i] <- c_U_obecne - c_L_obecne
  
  w_strefie <- sum(dane_symulacyjne$wynik_testu >= c_L_obecne & dane_symulacyjne$wynik_testu <= c_U_obecne)
  wyniki_symulacji$odsetek_w_szarej[i] <- w_strefie / n_pacjentow
  
  pacjenci_zdecydowani <- subset(dane_symulacyjne, wynik_testu > c_U_obecne | wynik_testu < c_L_obecne)
  #pacjenci poza szara strefa
  if (nrow(pacjenci_zdecydowani) > 0) {
    diagnoza_poza <- ifelse(pacjenci_zdecydowani$wynik_testu > c_U_obecne, 1, 0)
    poprawne_diagnozy <- sum(diagnoza_poza == pacjenci_zdecydowani$prawdziwy_stan)
    wyniki_symulacji$dokladnosc_poza[i] <- poprawne_diagnozy / nrow(pacjenci_zdecydowani)
  } else {
    wyniki_symulacji$dokladnosc_poza[i] <- NA 
  }
  
  if (i %% 1000 == 0) {
    cat("Policzono i zmierzono", i, "z", liczba_iteracji, "iteracji\n")
  }
}

saveRDS(wyniki_symulacji, here("wyniki", "wyniki_symulacji_NAZWA_METODY.rds"))