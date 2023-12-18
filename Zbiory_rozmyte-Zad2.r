# install.packages("sets")
library(sets)

# Wczytaj dane z pliku CSV do tablicy decyzyjnej
wine_data <- read.table("./wine.txt", header = TRUE, sep = " ")

# Wyświetl strukturę danych
print("wine_data:")
str(wine_data)


a = split(wine_data, sample(rep(1:3, times=nrow(wine_data)/3)))
train_data = rbind(a$'1',a$'2')
test_data = a$'3'

print("train_data:")
str(train_data)
print("test_data:")
str(test_data)


# Zbiór rozmyty dla kolumny "alcohol"
u1 = seq(11, 15, 0.01)
low_alcohol  = fuzzy_cone_gset(center=11, radius=1.5, universe=u1)
avg_alcohol = fuzzy_trapezoid_gset(corners=tuple(12, 12.5, 13.5, 14), universe=u1)
high_alcohol = fuzzy_cone_gset(center=15, radius=1.5, universe=u1)

alcohol = fuzzy_variable(low_alcohol =low_alcohol, avg_alcohol=avg_alcohol, high_alcohol=high_alcohol)
alcohol_sets = set(low_alcohol=low_alcohol, avg_alcohol=avg_alcohol, high_alcohol=high_alcohol)
plot(alcohol)


# Zbiór rozmyty dla kolumny "malic_acid"
u2 = seq(0, 6, 0.01)
low_malic_acid  = fuzzy_cone_gset(center=0, radius=3, universe=u2)
high_malic_acid = fuzzy_cone_gset(center=6, radius=4, universe=u2)

malic_acid = fuzzy_variable(low_malic_acid=low_malic_acid, high_malic_acid=high_malic_acid)
malic_acid_sets = set(low_malic_acid=low_malic_acid, high_malic_acid=high_malic_acid)
plot(malic_acid)


# Zbiór rozmyty dla kolumny "color_intensity"
u3 = seq(0, 12, 0.01)
low_color_intensity  = fuzzy_cone_gset(center=0, radius=8, universe=u3)
high_color_intensity = fuzzy_cone_gset(center=12, radius=8, universe=u3)

color_intensity = fuzzy_variable(low_color_intensity =low_color_intensity, high_color_intensity=high_color_intensity)
color_intensity_sets = set(low_color_intensity =low_color_intensity, high_color_intensity=high_color_intensity)
plot(color_intensity)


# Zbiór rozmyty dla kolumny "cultivar"
u4 = seq(1, 3, 1)
low_cultivar  = fuzzy_cone_gset(center=1, radius=1, universe=u4)
avg_cultivar = fuzzy_triangular_gset(corners = c(1, 2, 3), universe=u4)
high_cultivar = fuzzy_cone_gset(center=3, radius=1, universe=u4)

cultivar = fuzzy_variable(low_cultivar=low_cultivar, avg_cultivar=avg_cultivar, high_cultivar=high_cultivar)
cultivar_sets = set(low_cultivar=low_cultivar, avg_cultivar=avg_cultivar, high_cultivar=high_cultivar)
plot(cultivar)
plot(alcohol)

# Funkcja do zamiany wartości liczbowych na symboliczne na podstawie zbiorów rozmytych
fuzzy_to_symbolic <- function(value, breaks, labels) {
  cut_value <- cut(value, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.character(cut_value))
}

alcohol_fuzzy_to_symbolic <- function(values) {
  cut_value <- cut(value, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.character(cut_value))
}

# Definicja zbiorów rozmytych

breaks_alcohol <- c(-Inf, 12, 14, Inf)
labels_alcohol <- c("low_alcohol", "avg_alcohol", "high_alcohol")

u2 <- seq(0, 6, 0.5)
breaks_malic_acid <- c(-Inf, 3, Inf)
labels_malic_acid <- c("low_malic_acid", "high_malic_acid")

u3 <- seq(0, 12, 1)
breaks_color_intensity <- c(-Inf, 6, Inf)
labels_color_intensity <- c("low_color_intensity", "high_color_intensity")

u4 <- seq(1, 3, 0.1)
breaks_cultivar <- c(-Inf, 1.5, 2.5, Inf)
labels_cultivar <- c("low_cultivar", "avg_cultivar", "high_cultivar")

# Zamień wartości liczbowe na symboliczne dla "alcohol"
train_data$alcohol <- fuzzy_to_symbolic(train_data$alcohol, breaks_alcohol, labels_alcohol)

# Zamień wartości liczbowe na symboliczne dla "malic_acid"
train_data$malic_acid <- fuzzy_to_symbolic(train_data$malic_acid, breaks_malic_acid, labels_malic_acid)

# Zamień wartości liczbowe na symboliczne dla "color_intensity"
train_data$color_intensity <- fuzzy_to_symbolic(train_data$color_intensity, breaks_color_intensity, labels_color_intensity)

# Zamień wartości liczbowe na symboliczne dla "cultivar"
train_data$cultivar <- fuzzy_to_symbolic(train_data$cultivar, breaks_cultivar, labels_cultivar)

# Wyświetl przekształcone dane
#print(train_data)

#print(color_intensity_sets$low_color_intensity)
print(attributes(alcohol_sets$avg_alcohol[12])[[2]])
