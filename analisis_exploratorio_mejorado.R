# ===================================================================
# CLUSTERING JERÁRQUICO COMPLETO Y K-MEDIAS - DATASET AIRQUALITY
# ===================================================================

# -------------------------------------------------------------------
# CONFIGURACIÓN INICIAL
# -------------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(cluster)
library(VIM)  # Para visualización de datos faltantes

# Cargar datos airquality
data("airquality")
print("Dataset airquality cargado")
print(paste("Dimensiones originales:", nrow(airquality), "filas x", ncol(airquality), "columnas"))

# Explorar estructura del dataset
print("\nEstructura del dataset:")
str(airquality)

print("\nPrimeras filas:")
head(airquality)

print("\nResumen estadístico:")
summary(airquality)

# -------------------------------------------------------------------
# LIMPIEZA DE DATOS FALTANTES
# -------------------------------------------------------------------

print("\n=== ANÁLISIS DE DATOS FALTANTES ===")

# Contar valores faltantes por variable
na_count <- airquality %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, na_count) %>%
  arrange(desc(na_count))

print("Valores faltantes por variable:")
print(na_count)

# Visualizar patrones de datos faltantes
VIM::aggr(airquality,
          col = c('navyblue','red'),
          numbers = TRUE,
          sortVars = TRUE,
          main = "Patrón de Datos Faltantes")

# Eliminar filas con valores faltantes (listwise deletion)
airquality_completo <- airquality %>%
  drop_na()

print(paste("\nDataset original:", nrow(airquality), "observaciones"))
print(paste("Dataset sin NA:", nrow(airquality_completo), "observaciones"))
print(paste("Observaciones eliminadas:", nrow(airquality) - nrow(airquality_completo)))

# Seleccionar variables numéricas relevantes para clustering
# Excluimos Month y Day ya que son variables temporales, no características del aire
airquality_numeric <- airquality_completo %>%
  select(Ozone, Solar.R, Wind, Temp)

print("\nVariables seleccionadas para clustering:")
print(colnames(airquality_numeric))
print("- Ozone: Niveles de ozono (ppb)")
print("- Solar.R: Radiación solar (lang)")
print("- Wind: Velocidad del viento (mph)")
print("- Temp: Temperatura (°F)")

# ===================================================================
# 1. CLUSTERING JERÁRQUICO COMPLETO
# ===================================================================

print("\n\n=== 1. CLUSTERING JERÁRQUICO COMPLETO ===")

# Estandarizar los datos (importante para clustering)
datos_escalados <- scale(airquality_numeric)
rownames(datos_escalados) <- paste("Dia", 1:nrow(datos_escalados), sep="_")

# Calcular matriz de distancias euclidianas
dist_matrix <- dist(datos_escalados, method = "euclidean")

# Aplicar clustering jerárquico con método completo (complete linkage)
hclust_completo <- hclust(dist_matrix, method = "complete")

# Visualizar dendrograma
plot(hclust_completo,
     main = "Dendrograma - Clustering Jerárquico Completo\n(Calidad del Aire)",
     xlab = "Días",
     ylab = "Distancia Euclidiana",
     cex = 0.6,
     labels = FALSE)  # Sin etiquetas porque son muchas observaciones

# Determinar número de clusters basado en el dendrograma
# Cortamos en 4 clusters para tener grupos manejables
k_clusters <- 4
rect.hclust(hclust_completo, k = k_clusters, border = "red")

# Obtener asignación de clusters
clusters_jerarquicos <- cutree(hclust_completo, k = k_clusters)

# Crear dataframe con clusters asignados
airquality_con_clusters <- airquality_numeric %>%
  mutate(cluster = factor(clusters_jerarquicos))

print(paste("\nNúmero de clusters creados:", k_clusters))
print("Tamaño de cada cluster:")
table(clusters_jerarquicos)

# -------------------------------------------------------------------
# 1.1 ANÁLISIS DEL COMPORTAMIENTO DE CADA GRUPO
# -------------------------------------------------------------------

print("\n--- ANÁLISIS DE COMPORTAMIENTO POR CLUSTER ---")

# Estadísticas descriptivas por cluster
analisis_clusters <- airquality_con_clusters %>%
  group_by(cluster) %>%
  summarise(
    n_dias = n(),
    ozone_media = round(mean(Ozone), 1),
    ozone_sd = round(sd(Ozone), 1),
    solar_media = round(mean(Solar.R), 0),
    solar_sd = round(sd(Solar.R), 0),
    wind_media = round(mean(Wind), 1),
    wind_sd = round(sd(Wind), 1),
    temp_media = round(mean(Temp), 1),
    temp_sd = round(sd(Temp), 1),
    .groups = 'drop'
  )

print("ESTADÍSTICAS POR CLUSTER:")
print(analisis_clusters)

# Análisis detallado de cada cluster
print("\nCOMENTARIOS SOBRE EL COMPORTAMIENTO DE CADA GRUPO:")

for(i in 1:k_clusters) {
  cluster_stats <- analisis_clusters[analisis_clusters$cluster == i, ]
  print(paste("\nCLUSTER", i, "- CARACTERÍSTICAS:"))
  print(paste("- Número de días:", cluster_stats$n_dias))
  print(paste("- Ozono promedio:", cluster_stats$ozone_media, "ppb (±", cluster_stats$ozone_sd, ")"))
  print(paste("- Radiación solar promedio:", cluster_stats$solar_media, "lang (±", cluster_stats$solar_sd, ")"))
  print(paste("- Viento promedio:", cluster_stats$wind_media, "mph (±", cluster_stats$wind_sd, ")"))
  print(paste("- Temperatura promedio:", cluster_stats$temp_media, "°F (±", cluster_stats$temp_sd, ")"))
}

print("\n¿QUÉ CONSIDERÓ EL ALGORITMO PARA AGRUPAR?")
print("El algoritmo de clustering jerárquico completo consideró principalmente:")
print("1. NIVELES DE OZONO: Variable clave que define la calidad del aire")
print("2. TEMPERATURA: Factor crítico que influye en la formación de ozono")
print("3. RADIACIÓN SOLAR: Catalizador fotoquímico para la formación de ozono")
print("4. VELOCIDAD DEL VIENTO: Factor de dispersión de contaminantes")

print("\nPATRONES DE COMPORTAMIENTO IDENTIFICADOS:")

# Identificar características de cada cluster
cluster_alto_ozono <- which.max(analisis_clusters$ozone_media)
cluster_bajo_ozono <- which.min(analisis_clusters$ozone_media)
cluster_alta_temp <- which.max(analisis_clusters$temp_media)
cluster_alto_viento <- which.max(analisis_clusters$wind_media)

print(paste("- Cluster", cluster_alto_ozono, ": DÍAS DE ALTA CONTAMINACIÓN (ozono elevado)"))
print(paste("- Cluster", cluster_bajo_ozono, ": DÍAS DE BUENA CALIDAD DEL AIRE (ozono bajo)"))
print(paste("- Cluster", cluster_alta_temp, ": DÍAS CALUROSOS (temperatura alta)"))
print(paste("- Cluster", cluster_alto_viento, ": DÍAS VENTOSOS (viento fuerte - dispersión)"))

# Crear visualización de clusters en 2D (usando las dos primeras componentes principales)
pca_result <- prcomp(datos_escalados, scale. = FALSE)  # Ya están escalados
pca_data <- data.frame(pca_result$x[,1:2], cluster = factor(clusters_jerarquicos))

plot(pca_data$PC1, pca_data$PC2,
     col = as.numeric(pca_data$cluster),
     pch = 19,
     main = "Clusters Jerárquicos - Espacio PCA",
     xlab = paste("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "% varianza)"),
     ylab = paste("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "% varianza)"))
legend("topright",
       legend = paste("Cluster", 1:k_clusters),
       col = 1:k_clusters,
       pch = 19)

print("\nINTERPRETACIÓN DE PATRONES CLIMÁTICOS:")
print("- Los clusters reflejan diferentes condiciones meteorológicas")
print("- Días con alta temperatura + baja velocidad viento = mayor ozono")
print("- Días ventosos tienden a tener menor concentración de ozono")
print("- La radiación solar actúa como catalizador en la formación de ozono")

# ===================================================================
# 2. K-MEDIAS CON DETECCIÓN DE OUTLIERS
# ===================================================================

print("\n\n=== 2. ALGORITMO K-MEDIAS ===")

# -------------------------------------------------------------------
# 2.0 PESQUISA Y ELIMINACIÓN DE OUTLIERS
# -------------------------------------------------------------------

print("\n--- 2.0 DETECCIÓN Y ELIMINACIÓN DE OUTLIERS ---")

# Función para detectar outliers usando método del rango intercuartílico (IQR)
detectar_outliers <- function(datos) {
  outliers_totales <- c()

  for (col_name in colnames(datos)) {
    col_data <- datos[[col_name]]

    # Calcular cuartiles
    Q1 <- quantile(col_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(col_data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1

    # Límites para outliers (método IQR)
    limite_inferior <- Q1 - 1.5 * IQR
    limite_superior <- Q3 + 1.5 * IQR

    # Identificar outliers
    outliers_col <- which(col_data < limite_inferior | col_data > limite_superior)

    if (length(outliers_col) > 0) {
      print(paste("Variable", col_name, "- Outliers en filas:", paste(outliers_col, collapse = ", ")))
      print(paste("  Rango normal: [", round(limite_inferior, 1), ",", round(limite_superior, 1), "]"))
      print(paste("  Valores outliers:", paste(round(col_data[outliers_col], 1), collapse = ", ")))
      outliers_totales <- c(outliers_totales, outliers_col)
    }
  }

  return(unique(outliers_totales))
}

# Detectar outliers
indices_outliers <- detectar_outliers(airquality_numeric)

print("\nRESUMEN DE OUTLIERS:")
print(paste("Total de días con outliers:", length(indices_outliers)))

if (length(indices_outliers) > 0) {
  print(paste("Porcentaje de outliers:", round(length(indices_outliers)/nrow(airquality_numeric)*100, 1), "%"))

  # Mostrar algunos ejemplos de outliers
  print("Ejemplos de días con valores extremos:")
  outliers_data <- airquality_numeric[indices_outliers[1:min(5, length(indices_outliers))], ]
  print(outliers_data)

  # Eliminar outliers
  airquality_sin_outliers <- airquality_numeric[-indices_outliers, ]
  print(paste("Dataset original:", nrow(airquality_numeric), "días"))
  print(paste("Dataset sin outliers:", nrow(airquality_sin_outliers), "días"))
  print(paste("Outliers eliminados:", length(indices_outliers), "días"))
} else {
  airquality_sin_outliers <- airquality_numeric
  print("No se detectaron outliers significativos usando el método IQR")
}

# Visualizar outliers con boxplots
par(mfrow = c(2, 2))
variables <- colnames(airquality_numeric)
for (i in 1:4) {
  var_name <- variables[i]
  boxplot(airquality_numeric[, i],
          main = paste("Boxplot:", var_name),
          col = "lightblue",
          ylab = var_name,
          outline = TRUE)

  # Marcar outliers detectados en rojo
  if (length(indices_outliers) > 0) {
    outliers_var <- airquality_numeric[indices_outliers, i]
    if (length(outliers_var) > 0) {
      points(rep(1, length(outliers_var)), outliers_var,
             col = "red", pch = 19, cex = 1.2)
    }
  }
}
par(mfrow = c(1, 1))

# -------------------------------------------------------------------
# APLICAR K-MEDIAS
# -------------------------------------------------------------------

print("\n--- APLICANDO K-MEDIAS ---")

# Estandarizar datos sin outliers
datos_escalados_sin_outliers <- scale(airquality_sin_outliers)

# Determinar número óptimo de k usando método del codo
set.seed(123)
wss <- numeric(10)
for (k in 1:10) {
  kmeans_temp <- kmeans(datos_escalados_sin_outliers, centers = k, nstart = 25)
  wss[k] <- kmeans_temp$tot.withinss
}

# Gráfico del método del codo
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters (k)", ylab = "WSS Total",
     main = "Método del Codo para K-Medias\n(Dataset Airquality)",
     col = "blue", lwd = 2)
lines(1:10, wss, col = "blue")
grid(TRUE)

# Aplicar K-medias con k=4 (para comparar con clustering jerárquico)
set.seed(123)
kmeans_resultado <- kmeans(datos_escalados_sin_outliers, centers = 4, nstart = 25, iter.max = 100)

print("\nRESULTADOS K-MEDIAS:")
print(paste("Número de clusters:", length(kmeans_resultado$size)))
print(paste("Tamaño de cada cluster:", paste(kmeans_resultado$size, collapse = ", ")))
print(paste("Total within-cluster sum of squares:", round(kmeans_resultado$tot.withinss, 2)))
print(paste("Between-cluster sum of squares:", round(kmeans_resultado$betweenss, 2)))

# Análisis de los clusters K-medias
airquality_kmeans <- airquality_sin_outliers %>%
  mutate(cluster_kmeans = factor(kmeans_resultado$cluster))

analisis_kmeans <- airquality_kmeans %>%
  group_by(cluster_kmeans) %>%
  summarise(
    n_dias = n(),
    ozone_media = round(mean(Ozone), 1),
    solar_media = round(mean(Solar.R), 0),
    wind_media = round(mean(Wind), 1),
    temp_media = round(mean(Temp), 1),
    .groups = 'drop'
  )

print("\nESTADÍSTICAS POR CLUSTER K-MEDIAS:")
print(analisis_kmeans)

# Visualizar clusters K-medias
fviz_cluster(kmeans_resultado,
             data = datos_escalados_sin_outliers,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal()) +
  ggtitle("Clustering K-medias (k=4)\nCalidad del Aire - Dataset Airquality") +
  theme(plot.title = element_text(hjust = 0.5))

print("\n=== ANÁLISIS COMPLETADO ===")
print("Clustering aplicado exitosamente al dataset airquality")
print("Se identificaron patrones de calidad del aire basados en variables meteorológicas")