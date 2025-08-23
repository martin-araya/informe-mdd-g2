# Cargar el conjunto de datos
data("Titanic")

# Ver el tipo de objeto que es
class(Titanic)
# > [1] "table"

# Ver la estructura del objeto
# Muestra las 4 dimensiones: Class, Sex, Age, Survived y el número de personas
str(Titanic)
# > 'table' num [1:4, 1:2, 1:2, 1:2] 0 0 35 0 0 0 17 0 118 154 ...
# > - attr(*, "dimnames")=List of 4
# >  ..$ Class   : chr [1:4] "1st" "2nd" "3rd" "Crew"
# >  ..$ Sex     : chr [1:2] "Male" "Female"
# >  ..$ Age     : chr [1:2] "Child" "Adult"
# >  ..$ Survived: chr [1:2] "No" "Yes"

# Imprimir la tabla para ver los conteos
print(Titanic)