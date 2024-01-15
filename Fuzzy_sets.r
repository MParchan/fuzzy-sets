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

# Wykresy zbiorów rozmytych
par(mfrow=c(2,2))  # Ustawienie układu na 2 wiersze i 2 kolumny

# Zbiór rozmyty dla kolumny "alcohol"
u1 = seq(11, 15, 0.01)
low_alcohol  = fuzzy_cone_gset(center=11, radius=1.5, universe=u1)
avg_alcohol = fuzzy_trapezoid_gset(corners=tuple(12, 12.5, 13.5, 14), universe=u1)
high_alcohol = fuzzy_cone_gset(center=15, radius=1.5, universe=u1)

alcohol = fuzzy_variable(low_alcohol =low_alcohol, avg_alcohol=avg_alcohol, high_alcohol=high_alcohol)
alcohol_sets = set(low_alcohol=low_alcohol, avg_alcohol=avg_alcohol, high_alcohol=high_alcohol)
plot(alcohol, main="Alcohol")


# Zbiór rozmyty dla kolumny "malic_acid"
u2 = seq(0, 6, 0.01)
low_malic_acid  = fuzzy_cone_gset(center=0, radius=3, universe=u2)
high_malic_acid = fuzzy_cone_gset(center=6, radius=4, universe=u2)

malic_acid = fuzzy_variable(low_malic_acid=low_malic_acid, high_malic_acid=high_malic_acid)
malic_acid_sets = set(low_malic_acid=low_malic_acid, high_malic_acid=high_malic_acid)
plot(malic_acid, main="Malic Acid")


# Zbiór rozmyty dla kolumny "color_intensity"
u3 = seq(0, 12, 0.01)
low_color_intensity  = fuzzy_cone_gset(center=0, radius=8, universe=u3)
high_color_intensity = fuzzy_cone_gset(center=12, radius=8, universe=u3)

color_intensity = fuzzy_variable(low_color_intensity =low_color_intensity, high_color_intensity=high_color_intensity)
color_intensity_sets = set(low_color_intensity =low_color_intensity, high_color_intensity=high_color_intensity)
plot(color_intensity, main="Color Intensity")


# Zbiór rozmyty dla kolumny "cultivar"
u4 = seq(1, 3, 1)
low_cultivar  = fuzzy_cone_gset(center=1, radius=1, universe=u4)
avg_cultivar = fuzzy_triangular_gset(corners = c(1, 2, 3), universe=u4)
high_cultivar = fuzzy_cone_gset(center=3, radius=1, universe=u4)

cultivar = fuzzy_variable(low_cultivar=low_cultivar, avg_cultivar=avg_cultivar, high_cultivar=high_cultivar)
cultivar_sets = set(low_cultivar=low_cultivar, avg_cultivar=avg_cultivar, high_cultivar=high_cultivar)
plot(cultivar, main="Cultivar")

# Funkcja do zamiany wartości liczbowych na symboliczne na podstawie zbiorów rozmytych
fuzzy_to_symbolic <- function(value, breaks, labels) {
  cut_value <- cut(value, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.character(cut_value))
}


# Definicja zbiorów rozmytych
# alcohol
alco_a = 12
alco_b = 13.5
for (i in 1:length(alcohol_sets$avg_alcohol)) {
  i = 12+(i-1)*0.01
  if(length(attributes(alcohol_sets$avg_alcohol[i]))>1 & length(attributes(alcohol_sets$low_alcohol[i]))>1) {
    if(attributes(alcohol_sets$low_alcohol[i])[[2]]>attributes(alcohol_sets$avg_alcohol[i])[[2]])
    {
      alco_a = i
    }
    else {
      break
    }
  }
}
for (i in 1:length(alcohol_sets$high_alcohol)) {
  i = 13.5+(i-1)*0.01
  
  if(length(attributes(alcohol_sets$high_alcohol[i]))>1 & length(attributes(alcohol_sets$avg_alcohol[i]))>1) {
    if(attributes(alcohol_sets$high_alcohol[i])[[2]] < attributes(alcohol_sets$avg_alcohol[i])[[2]])
    {
      alco_b = i
    }
    else {
      break
    }
  }
}
breaks_alcohol <- c(-Inf, alco_a, alco_b, Inf)
labels_alcohol <- c("low_alcohol", "avg_alcohol", "high_alcohol")

# malic_acid
malic_acid_a = 2
for (i in 1:length(malic_acid_sets$high_malic_acid)) {
  i = 2+(i-1)*0.01
  if(length(attributes(malic_acid_sets$high_malic_acid[i]))>1 & length(attributes(malic_acid_sets$low_malic_acid[i]))>1) {
    if(attributes(malic_acid_sets$low_malic_acid[i])[[2]]>attributes(malic_acid_sets$high_malic_acid[i])[[2]])
    {
      malic_acid_a = i
    }
    else {
      break
    }
  }
}
u2 <- seq(0, 6, 0.5)
breaks_malic_acid <- c(-Inf, malic_acid_a, Inf)
labels_malic_acid <- c("low_malic_acid", "high_malic_acid")

# color_intensity
color_intensity_a = 4
for (i in 1:length(color_intensity_sets$high_color_intensity)) {
  i = 4+(i-1)*0.01
  if(length(attributes(color_intensity_sets$high_color_intensity[i]))>1 & length(attributes(color_intensity_sets$low_color_intensity[i]))>1) {
    if(attributes(color_intensity_sets$low_color_intensity[i])[[2]]>attributes(color_intensity_sets$high_color_intensity[i])[[2]])
    {
      color_intensity_a = i
    }
    else {
      break
    }
  }
}
u3 <- seq(0, 12, 1)
breaks_color_intensity <- c(-Inf, color_intensity_a, Inf)
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
print(train_data)
write.table(train_data, file = "train_data_symbolic.txt", sep = "\t", row.names = FALSE)

# alcohol rdzeń
alcohol_core <- c()
for (i in 1:length(avg_alcohol)) {
  i = 12+(i-1)*0.01
  if(length(attributes(avg_alcohol[i]))==1 & i>=12.5)
  {
    if((length(attributes(avg_alcohol[i+0.01]))==1) | (length(attributes(avg_alcohol[i-0.01]))==1))
    {
      alcohol_core <- c(alochol_core, i)
    }
  }
}
print("Rdzeń dla alcohol:")
print(alcohol_core)
write.table(alcohol_core, file = "alcohol_core.txt", col.names = FALSE, row.names = FALSE)


# alcohol α-przekrój
alcohol_section <- c()
for (i in 1:length(alcohol_sets$avg_alcohol)) {
  i = 12+(i-1)*0.01
  if(length(attributes(alcohol_sets$avg_alcohol[i]))==1 & i>=12.5)
  {
    if((length(attributes(avg_alcohol[i+0.01]))==1) | (length(attributes(avg_alcohol[i-0.01]))==1))
    {
    alcohol_section <- c(alochol_section, i)
    }
  }
  if(length(attributes(alcohol_sets$avg_alcohol[i]))==2)
  {
    if(attributes(alcohol_sets$avg_alcohol[i])[[2]]>=0.75)
    {
      alcohol_section <- c(alcohol_section, i)
    }
  }
}
print("alfa-przekrój, gdzie alfa = 0.75:")
print(alcohol_section)
write.table(alcohol_section, file = "alcohol_section.txt", col.names = FALSE, row.names = FALSE)


