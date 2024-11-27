library(dplyr)
library(tidyr)
install.packages("igraph")
library(igraph)
library(readr)
install.packages("networkD3")
library(networkD3)
library(stringr)
library(ggplot2)
library(RColorBrewer)  # Asegúrate de tener este paquete
install.packages('viridisLite')
library(viridis)
archivo= ('/Users/paula/Documents/Duponte/graficas_r/scopus_MIASTENIA.csv')
datos=read.csv(archivo)
# Definir una función para extraer los países de una afiliación
extraer_paises=function(afiliaciones) {
  # Separar las afiliaciones por punto y coma
  partes=str_split(afiliaciones, ";") [[1]] #[[1]] para acceder al primer (y único) elemento de esa lista.
  
  # Extraer el país de cada parte
  paises=str_extract(partes, "[A-Z][a-z]+(?: [A-Z][a-z]+)*$")  # Extraer el último nombre que se considera el país
  #* Significa "cero o más repeticiones" del patrón agrupado en (?: ... ).
  #$ Ancla al final de la cadena. Esto asegura que el patrón busca específicamente el texto final de cada elemento de partes.
  return(na.omit(unique(trimws(paises))))  # Retornar países únicos, eliminando NA y espacios en blanco
}

# Aplicar la función a todas las afiliaciones
paises_extraidos=sapply(datos$Affiliations, extraer_paises)

names(paises_extraidos)=NULL
#paises_extraidos [[3]]="United States"

datos$paises=paises_extraidos #añade una columna llamada paises en el dataframe datos.
# Convertir la lista de países en un dataframe expandido
datos_expandidos=datos %>% #operador pipe (%>%) se utiliza para encadenar operaciones de transformación en R
  unnest(paises) %>%  # Expandir la lista de países
  mutate(paises = trimws(paises))  # Opcional: eliminar espacios en blanco alrededor de los nombres de países
#esto separa en filas los distintos paises de un mismo articulo

datos_df=as.data.frame(datos_expandidos)
unique(datos_df$paises) #hacemos esto para ver los paises que nos enumera, si hubiera alguno que no es un pais hay que quitarlo a mano

# Calcular las colaboraciones entre países y su frecuencia
colaboraciones= datos_df %>%
  filter(!is.na(paises)) %>%      # Filtra las filas donde la columna paises no es NA (valores vacíos o nulos). Se eliminan datos incompletos.
  unnest(paises) %>%              # Separar los países, si una fila tiene una lista con varios paises se descompone en varias filas
  group_by(Title) %>%             # Agrupar por la columna Title. Esto organiza los países por artículos.
  summarize( 
    colaboraciones = list({       #Se crea una nueva columna llamada colaboraciones, que contiene listas de todas las combinaciones posibles de colaboraciones entre países por título de artículo.
      unique_paises=unique(paises)    #Obtiene los países únicos del artículo para evitar redundancias.    
      if (length(unique_paises) >= 2) {   #Si hay al menos dos países únicos, calcula todas las combinaciones posibles de pares usando combn.
        combn(unique_paises, 2, simplify = FALSE)
      } else {
        NULL
      }
    })
  ) %>%
  filter(!sapply(colaboraciones, is.null)) %>% # Filtrar combinaciones nulas
  unnest(colaboraciones) #Convierte las listas de colaboraciones (pares de países) en filas separadas para procesarlas fácilmente.

# Convertir los pares en un dataframe adecuado para un grafo
edges=do.call(rbind, colaboraciones$colaboraciones) #Combina todas las filas de listas de pares en una tabla
colnames(edges)=c("Pais1", "Pais2")
edges=edges[!(is.na(edges[,1]) | is.na(edges[,2]) | edges[,1] == "" | edges[,2] == ""), ] #se eliminan las filas que contengan algun valor vacío

# Ordenar alfabéticamente los nombres de los países para evitar duplicados 
edges_sorted=data.frame(
  Pais1 = pmin(edges[,1], edges[,2]), #pmin() toma el país que alfabéticamente viene primero, y pmax() toma el segundo
  Pais2 = pmax(edges[,1], edges[,2])
)

# Contar las ocurrencias de cada par de países
edges_count=edges_sorted %>%
  group_by(Pais1, Pais2) %>%  # Agrupa las filas por pares de países únicos. Por ejemplo, todas las colaboraciones entre "Italia" y "Francia" se agrupan.
  summarise(Count = n()) %>%  # Cuenta cuántas veces aparece cada par de países y lo almacena en una columna llamada Count
  arrange(desc(Count))         # Ordenar por el conteo en orden descendente

# Crear el grafo
grafo=graph_from_data_frame(d=edges_count[1:20,], directed=FALSE)

#Los grafos en igraph están formados por nodos (o vértices) y aristas (o conexiones entre nodos) 
#El objeto grafo es un grafo no dirigido que representa conexiones (aristas) entre nodos (países), 
#basado en las primeras 20 colaboraciones más frecuentes del dataframe edges_count.

# Crear una función para simular el efecto 3D en los nodos
add_shadow=function(colors, intensity = 0.8) {
  # Hacer que los bordes sean más oscuros para simular un efecto de sombra
  darker_colors=adjustcolor(colors, intensity)
  return(darker_colors)
}

# Asignar colores de los nodos
vertex_colors="lightgreen"
vertex_border_colors=add_shadow(vertex_colors)

# Definir el layout para mayor separación
layout=layout_with_fr(grafo, niter = 1000)


#Es una función del paquete igraph que calcula la posición de los nodos del grafo para su visualización.
#Utiliza el algoritmo de Fruchterman-Reingold, un método de diseño por fuerza (force-directed layout)

png("/Users/paula/Documents/Duponte/graficas_r/miastenia_grafo_scopus_NUEVO.png", width = 3000, height = 2500, res = 300)  # Ajusta width, height y res según tus necesidades
# Visualizar el grafo con efecto de sombra (simulando 3D)
plot(grafo, 
     layout = layout,                  # Usar el layout definido
     vertex.label = V(grafo)$name, 
     vertex.size = 30,                 # Tamaño de los nodos
     vertex.color = vertex_colors,     # Color de los nodos
     vertex.frame.color = vertex_border_colors, # Bordes más oscuros para simular 3D
     edge.width = 1,
     vertex.label.color = "black",     # Color de las etiquetas
     vertex.label.cex = 0.8,           # Tamaño de las etiquetas
     vertex.label.font = 2             # Negrita
)
dev.off()


#gráfica numero de publicaciones por año
# Pasamos a caracteres la lista de países
datos_df$paises=paste(datos_df$paises)

# Suponiendo que tu dataframe se llama 'df'
# Agrupar por 'Year' y 'paises' y contar el número de publicaciones
df_summary=datos_df %>%
  group_by(Year, paises) %>%
  summarise(Production = n(), .groups = 'drop')  # Contar el número de publicaciones
#'drop': Indica que se eliminará la agrupación al final. Es decir, el marco de datos df_summary resultante no estará agrupado.

# Crear el gráfico con un color fijo
ggplot(df_summary, aes(x = paises, y = Year, size = Production)) +
  geom_point(alpha = 0.7, color = "blue") +  # Ajusta el color de los puntos
  scale_size(range = c(2, 10), name = "Producción") +  # Ajusta el rango del tamaño de los puntos
  labs(title = "Producción Anual por País",
       x = "País",
       y = "Año") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Ajustar el ángulo del texto del eje x

png("/Users/paula/Documents/Duponte/graficas_r/miastenia_produccion_paises_NUEVO.png", width = 2500, height = 2500, res = 300)  # Ajusta width, height y res según tus necesidades
df_summary %>% 
  #filter(Production > 1) %>% 
  ggplot(aes(x = paises, y = Year, size = Production, color = paises)) +  
  geom_point(alpha = 0.7, show.legend = FALSE) +  # Quitar la leyenda de los países
  scale_size(range = c(2, 10), name = "Producción") + 
  labs(title = "",
       x = "País",
       y = "Año") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 14),  # Aumentar tamaño del texto del eje x
    axis.text.y = element_text(size = 14),  # Aumentar tamaño del texto del eje y
    axis.title.x = element_text(size = 14),  # Aumentar tamaño del título del eje x
    axis.title.y = element_text(size = 14)   # Aumentar tamaño del título del eje y
  ) + 
  scale_color_viridis_d() +  # Usar la paleta viridis para más colores
  scale_y_continuous(breaks = seq(floor(min(df_summary$Year)), ceiling(max(df_summary$Year)), by = 1))  # Eje y solo con enteros
dev.off()
