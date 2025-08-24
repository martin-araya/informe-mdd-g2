# Cargar el conjunto de datos
data("airquality")

# Ver el tipo de objeto (verás que ya es un data.frame)
class(airquality)
# > [1] "data.frame"

# Ver las primeras 6 filas para tener una idea de los datos
head(airquality)
# >   Ozone Solar.R Wind Temp Month Day
# > 1    41     190  7.4   67     5   1
# > 2    36     118  8.0   72     5   2
# > 3    12     149 12.6   74     5   3
# > 4    18     313 11.5   62     5   4
# > 5    NA      NA 14.3   56     5   5
# > 6    28      NA 14.9   66     5   6

# Obtener un resumen estadístico de cada columna
summary(airquality)
# >      Ozone           Solar.R           Wind             Temp           Month            Day
# >  Min.   :  1.00   Min.   :  7.0   Min.   : 1.700   Min.   :56.00   Min.   :5.000   Min.   : 1.0
# >  1st Qu.: 18.00   1st Qu.:115.8   1st Qu.: 7.400   1st Qu.:72.00   1st Qu.:6.000   1st Qu.: 8.0
# >  Median : 31.50   Median :205.0   Median : 9.700   Median :79.00   Median :7.000   Median :16.0
# >  Mean   : 42.13   Mean   :185.9   Mean   : 9.958   Mean   :77.88   Mean   :6.993   Mean   :15.8
# >  3rd Qu.: 63.25   3rd Qu.:258.8   3rd Qu.:11.500   3rd Qu.:85.00   3rd Qu.:8.000   3rd Qu.:23.0
# >  Max.   :168.00   Max.   :334.0   Max.   :20.700   Max.   :97.00   Max.   :9.000   Max.   :31.0
# >  NA's   :37       NA's   :7