library(MASS)
library(here)
model_optymalny <- readRDS(here("dane", "moj_model.rds"))
dane_zredukowane <- readRDS(here("dane", "dane_zredukowane.rds"))

wybrane_kolumny <- c("radius_mean", "texture_mean", "area_mean", 
                     "smoothness_mean", "concave.points_mean", "symmetry_mean")

prawdziwe_niezlosliwe <- subset(dane_zredukowane, diagnosis == 0)[, wybrane_kolumny]
prawdziwe_zlosliwe    <- subset(dane_zredukowane, diagnosis == 1)[, wybrane_kolumny]

srednie_niezlosliwe <- colMeans(prawdziwe_niezlosliwe)
kowariancja_niezlosliwe <- cov(prawdziwe_niezlosliwe)

srednie_zlosliwe <- colMeans(prawdziwe_zlosliwe)
kowariancja_zlosliwe <- cov(prawdziwe_zlosliwe)

generator_danych <- function(n_prob) {
  
  sztuczne_niezlosliwe <- mvrnorm(n = n_prob, mu = srednie_niezlosliwe, Sigma = kowariancja_niezlosliwe)
  sztuczne_zlosliwe <- mvrnorm(n = n_prob, mu = srednie_zlosliwe, Sigma = kowariancja_zlosliwe)
  
  df_symulacja <- data.frame(rbind(sztuczne_niezlosliwe, sztuczne_zlosliwe))
  df_symulacja$prawdziwy_stan <- c(rep(0, n_prob), rep(1, n_prob))
  
  df_symulacja$wynik_testu <- predict(model_optymalny, newdata = df_symulacja, type = "response")
  
  return(df_symulacja)
}

#View(df_symulacja)
#tail(df_symulacja)
#moja_tabela_100 <- generator_danych(n_prob = 100)
#View(moja_tabela_100)
