library(here)
dane <- read.csv(here("dane", "data.csv"))


#musiałam zostawić tylko te zmienne bo tak naprawdę mamy 10 fizycznych cech guza, 
#tylko zostały ine zmierzone na 3 sposoby, zmienne typu radius mean i radius se 
#są ze sobą bardzo mocno skorelowane
dane_zredukowane <- dane_czyste[, c("diagnosis", 
                                    "radius_mean", "texture_mean", "perimeter_mean", 
                                    "area_mean", "smoothness_mean", "compactness_mean", 
                                    "concavity_mean", "concave.points_mean", 
                                    "symmetry_mean", "fractal_dimension_mean")]

model_pelny <- glm(diagnosis ~ ., data = dane_zredukowane, family = binomial)
model_optymalny <- step(model_pelny, direction = "backward")
summary(model_optymalny)

saveRDS(model_optymalny, here("dane", "moj_model.rds"))
saveRDS(dane_zredukowane, here("dane", "dane_zredukowane.rds"))
