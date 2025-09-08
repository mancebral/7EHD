library(readr)
library(stringr)
library(tidyverse)

setwd("~/RPROJECTS/7EHD")

# Ruta a tu archivo de texto con las líneas (una por fila)
ruta_txt <- "abstracts-4.txt"  # Cambia a tu ruta real

# Leer todas las líneas
lineas <- read_lines(ruta_txt)

# v: tu vector de texto
# v <- c("[1]", "Título 1", "Texto A", "[2]", "Título 2", "Texto B", "Más texto")

tabla <- tibble(lineas)

library(stringr)

pat <- regex("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", dotall = TRUE)

tabla$modificacion <- vapply(str_extract_all(tabla$lineas, pat),
                   function(x) if (length(x)) tail(x, 1) else NA_character_,
                   character(1))

tablaOrdenada <- tabla %>%
  mutate(
    id                 = str_split_fixed(lineas, "'", 2)[,1],
    title_full         = str_split_fixed(lineas, "'", 2)[,2],
    title              = str_split_fixed(title_full, "', '", 2)[,1],
    abstract_full      = str_split_fixed(title_full, "', '", 2)[,2],
    abstract           = str_split_fixed(abstract_full, ", 1,", 2)[,1],
    ids_full           = str_split_fixed(abstract_full, "', ", 2)[,2],
    ids                = str_split_fixed(ids_full, ", '", 2)[,1]) %>% 
  separate(
    col = ids, 
    into = paste0("id", 1:7),   # nombres de columnas nuevas
    sep = ",\\s*",               # separador: coma + posible espacio
    fill = "right",              # si faltan columnas, rellena a la derecha con NA
    remove = TRUE                # elimina la columna original
    ) %>%  
  mutate(
    otros_campos_full  = str_split_fixed(abstract_full, ", 1,", 2)[,2],
    autores            = str_split_fixed(otros_campos_full, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos_full, "', '", 2)[,2],
    contacto           = str_split_fixed(otros_campos, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos, "', '", 2)[,2],
    tipo               = str_split_fixed(otros_campos, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos, "', '", 2)[,2],
    keywords           = str_split_fixed(otros_campos, "', ", 2)[,1]
  ) %>%
  select(-title_full, -abstract_full, -otros_campos_full)

tablaOrdenada <- tablaOrdenada %>% 
  mutate(id = str_remove_all(id, "[^0-9]")) %>% 
  select(-c(otros_campos, lineas)) %>%
  mutate(autores = str_remove(autores, ".*NULL, NULL, NULL, '"))

tablaOrdenada <- tablaOrdenada %>% 
  mutate(status=ifelse(id4==5, "Aceptado con cambios",
                       ifelse(id4==3, "Aceptado", 
                              ifelse(id4==4, "Rechazado",
                                     ifelse(id4==1, "Pendiente de asignación",
                                            "En revisión")))))

tablaOrdenada <- tablaOrdenada %>% 
  mutate(topico=ifelse(id2==1, "Geopolíticas, imaginarios de fronteras y cultura digital",
                       ifelse(id2==2, "Patrimonio cultural, arte y sociedad", 
                              ifelse(id2==4, "Analítica cultural y métodos digitales",
                                     "Docencia de las Humanidades Digitales"))))

tablaOrdenada <- tablaOrdenada %>% 
  select(-c(ids_full: id7))

#guardar
saveRDS(tablaOrdenada, "abstracts.rds")
tablaOrdenada %>% 
  select(-c(abstract, contacto)) %>% 
  mutate(title=paste0('<a href="https://viiencuentro.humanidadesdigitales.net/envios-aceptados/?abs_id=', id, 
                     '"><b>', title, '</b></a>')) %>% 
  select(-id) %>%
  mutate(tipo=sub(" \\|.*$", "", tipo)) %>% 
  write.csv("abstracts.csv", row.names = FALSE)

tablaOrdenada %>%
  mutate(tipo=sub(" \\|.*$", "", tipo)) %>% 
  mutate(tipo = str_trim(tipo)) %>% 
  write.csv("check_propuestas.csv", row.names = FALSE)

library(plotly)

tablaOrdenada %>%
  mutate(tipo=sub(" \\|.*$", "", tipo)) %>% 
  mutate(tipo = str_trim(tipo)) %>% 
  count(tipo)

topicosPlot <- tablaOrdenada %>%
  filter(status!="Rechazado") %>% 
  count(topico) %>% 
  ggplot(aes(n, reorder(as_factor(topico), n))) +
  geom_col(fill="red")+
  ylab(NULL)

ggplotly(topicosPlot)

tablaOrdenada %>%
  count(status)

keywords <- paste(tablaOrdenada$keywords, collapse = ", ") %>% 
  str_split(",") %>%
  unlist() %>%
  str_trim() %>%
  tolower() %>%
  unique()

count(keywords)

# tablaOrdenada %>% 
#   mutate(status=ifelse(id %in% Dictaminaciones_Propuestas_Sheet3$`Aceptados con cambios`, "Aceptado con cambios", 
#                        ifelse(id %in% Dictaminaciones_Propuestas_Sheet3$`2 rechazados`, "Rechazado", 
#                               "Aceptado"))) %>% 
#   write.csv("propuestas.csv", row.names = FALSE)


#reviews
# Ruta a tu archivo de texto con las líneas (una por fila)
reviews_txt <- "reviews.txt"  # Cambia a tu ruta real

# Leer todas las líneas
reviews_lineas <- read_lines(reviews_txt)

reviews_tabla <- tibble(reviews_lineas)

tablaOrdenada <- reviews_tabla %>%
  mutate(
    id                 = str_split_fixed(reviews_lineas, "'", 2)[,1],
    title_full         = str_split_fixed(reviews_lineas, "'", 2)[,2],
    title              = str_split_fixed(title_full, "', '", 2)[,1],
    abstract_full      = str_split_fixed(title_full, "', '", 2)[,2],
    abstract           = str_split_fixed(abstract_full, ", 1,", 2)[,1]) %>% View()
    otros_campos_full  = str_split_fixed(abstract_full, ", 1,", 2)[,2],
    autores            = str_split_fixed(otros_campos_full, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos_full, "', '", 2)[,2],
    contacto           = str_split_fixed(otros_campos, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos, "', '", 2)[,2],
    tipo               = str_split_fixed(otros_campos, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos, "', '", 2)[,2],
    keywords           = str_split_fixed(otros_campos, "', ", 2)[,1]
  ) %>%
  select(-title_full, -abstract_full, -otros_campos_full)


tablaOrdenada <- tabla %>%
  mutate(
    id                 = str_split_fixed(lineas, "'", 2)[,1],
    title_full         = str_split_fixed(lineas, "'", 2)[,2],
    title              = str_split_fixed(title_full, "', '", 2)[,1],
    abstract_full      = str_split_fixed(title_full, "', '", 2)[,2],
    abstract           = str_split_fixed(abstract_full, ", 1,", 2)[,1],
    otros_campos_full  = str_split_fixed(abstract_full, ", 1,", 2)[,2],
    autores            = str_split_fixed(otros_campos_full, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos_full, "', '", 2)[,2],
    contacto           = str_split_fixed(otros_campos, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos, "', '", 2)[,2],
    tipo               = str_split_fixed(otros_campos, "', '", 2)[,1],
    otros_campos       = str_split_fixed(otros_campos, "', '", 2)[,2],
    keywords           = str_split_fixed(otros_campos, "', ", 2)[,1]
  ) %>%
  select(-title_full, -abstract_full, -otros_campos_full)

tablaOrdenada <- tablaOrdenada %>% 
  mutate(id = str_remove_all(id, "[^0-9]")) %>% 
  select(-c(otros_campos, lineas)) %>%
  mutate(autores = str_remove(autores, ".*NULL, NULL, NULL, '"))

#guardar
saveRDS(tablaOrdenada, "abstracts.rds")
tablaOrdenada %>% 
  select(-c(abstract, contacto)) %>% 
  mutate(title=paste0('<a href="https://viiencuentro.humanidadesdigitales.net/envios-aceptados/?abs_id=', id, 
                      '"><b>', title, '</b></a>')) %>% 
  select(-id) %>%
  mutate(tipo=sub(" \\|.*$", "", tipo)) %>% 
  write.csv("abstracts.csv", row.names = FALSE)

tablaOrdenada %>% 
  mutate(status=ifelse(id %in% Dictaminaciones_Propuestas_Sheet3$`Aceptados con cambios`, "Aceptado con cambios", 
                       ifelse(id %in% Dictaminaciones_Propuestas_Sheet3$`2 rechazados`, "Rechazado", 
                              "Aceptado"))) %>% 
  write.csv("propuestas.csv", row.names = FALSE)

####otras operaciones####
#vector de keywords
keywords <- paste(tablaOrdenada$keywords, collapse = ", ") %>% 
  str_split(",") %>%
  unlist() %>%
  str_trim() %>%
  tolower() %>%
  unique()

json_keywords <-jsonlite::toJSON(keywords, auto_unbox = TRUE)
cat(json_keywords)

#vector de autores
autores <- paste(tablaOrdenada$autores, collapse = " | ") %>% 
  str_split(" \\| ") %>%
  unlist() %>%
  str_trim() %>%
  tolower() %>%
  str_to_title() %>% 
  unique()

json_autores <-jsonlite::toJSON(autores, auto_unbox = TRUE)
cat(json_autores)


library(widyr)
keywords <- tablaOrdenada %>% 
  separate_rows (keywords, sep = ",")%>%
  mutate(Keywords = trimws(keywords, which = "left"))%>%
  filter(!Keywords == "") %>% 
  mutate(Keywords = str_replace(Keywords, "\\.+$", "")) %>% 
  mutate(Keywords=toupper(Keywords)) %>% 
  select (id, Keywords)

Keywords_count <- keywords %>% 
  pairwise_count(Keywords, id, sort = TRUE)

Keywords_pairs <- Keywords_count[!duplicated(t(apply(Keywords_count, 1, sort))),]

#clusterizacion UMA

library(tidyverse)
library(text2vec)
library(uwot)
library(janitor)
library(cluster)

# 2. Vectorización con TF-IDF
# ======================================
# Tokenizador y limpieza
prep_fun <- tolower
tok_fun <- word_tokenizer

it <- itoken(tablaOrdenada$abstract, 
             preprocessor = prep_fun,
             tokenizer = tok_fun,
             ids = tablaOrdenada$id,
             progressbar = FALSE)

vectorizer <- vocab_vectorizer(vocabulary = create_vocabulary(it) %>%
                                 prune_vocabulary(term_count_min = 2))

dtm <- create_dtm(it, vectorizer)

# Ponderar con TF-IDF
tfidf <- TfIdf$new()
mat_tfidf <- tfidf$fit_transform(dtm)


# ======================================
# 3. Reducción de dimensionalidad (UMAP)
# ======================================
set.seed(123)
umap_res <- umap(as.matrix(mat_tfidf), n_neighbors = 10, min_dist = 0.1, metric = "cosine")
umap_df <- as.data.frame(umap_res)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$id <- tablaOrdenada$id

# ======================================
# 4. Selección automática de k (silhouette)
# ======================================
sil_scores <- data.frame(k = 2:10, sil_width = NA)
dist_mat <- dist(as.matrix(mat_tfidf), method = "euclidean")

for (k in 2:10) {
  km <- kmeans(mat_tfidf, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist_mat)
  sil_scores[sil_scores$k == k, "sil_width"] <- mean(ss[, 3])
}

best_k <- sil_scores$k[which.max(sil_scores$sil_width)]
cat("Número óptimo de clusters según silueta:", best_k, "\n")

# ======================================
# 5. Clustering final
# ======================================
clust <- kmeans(mat_tfidf, centers = best_k, nstart = 25)
umap_df$cluster <- factor(clust$cluster)

# ======================================
# 6. Visualización
# ======================================
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Mapa temático de abstracts (TF-IDF + UMAP)",
       subtitle = paste("Clusters detectados automáticamente:", best_k),
       color = "Cluster")




# ==============================
# 3. Crear matriz binaria abstract x keyword
# ==============================
mat <- keywords %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Keywords, values_from = value, values_fill = 0) %>%
  clean_names()

# Guardar ids y matriz numérica
ids <- mat$id
mat_num <- mat %>% select(-id) %>% as.matrix()


set.seed(123)
umap_res <- umap(mat_num, n_neighbors = 10, min_dist = 0.1, metric = "euclidean")
umap_df <- as.data.frame(umap_res)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$id <- ids

# ==============================
# 5. Selección automática de k (método de silueta)
# ==============================
sil_scores <- data.frame(k = 2:10, sil_width = NA)

for (k in 2:10) {
  km <- kmeans(mat_num, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(mat_num, method = "euclidean"))
  sil_scores[sil_scores$k == k, "sil_width"] <- mean(ss[, 3])
}

best_k <- sil_scores$k[which.max(sil_scores$sil_width)]
cat("Número óptimo de clusters según silueta:", best_k, "\n")

# ==============================
# 6. Clustering final con k óptimo
# ==============================
clust <- kmeans(mat_num, centers = 4, nstart = 25)
umap_df$cluster <- factor(clust$cluster)

# ==============================
# 7. Visualización
# ==============================
myPlot <- umap_df %>% 
  right_join(tablaOrdenada) %>% 
  ggplot(aes(x = UMAP1, y = UMAP2, color = cluster, text=title)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Mapa temático de abstracts (UMAP + Clustering automático)",
       subtitle = paste("Clusters detectados automáticamente:", best_k),
       color = "Cluster")

library(plotly)
ggplotly(myPlot, tooltip = "text")

# ==============================
# 4. Reducción de dimensionalidad con UMAP
# ==============================
set.seed(123)
umap_res <- umap(mat_num, n_neighbors = 6, min_dist = 0.1, metric = "euclidean")

umap_df <- as.data.frame(umap_res)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$id <- ids

# ==============================
# 5. Clustering (k-means)
# ==============================
# Número de clusters: ajusta según datos
k <- 3
clust <- kmeans(mat_num, centers = k, nstart = 25)
umap_df$cluster <- factor(clust$cluster)

# ==============================
# 6. Visualización
# ==============================
library(ggplot2)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Mapa temático de abstracts (UMAP)",
       color = "Cluster")


####opcion transformers####
library(tidyverse)
library(uwot)
library(reticulate)

reticulate::py_install(c("sentence-transformers", "hdbscan"), pip = TRUE)


# ==============================
# 1. Configurar Python y cargar librerías
# ==============================
st <- import("sentence_transformers")
np <- import("numpy")
hdbscan <- import("hdbscan")

# ==============================
# 3. Generar embeddings con Sentence-BERT
# ============================== 
tablaOrdenada <- tablaOrdenada %>% 
  mutate(contenido=paste(title, abstract, keywords, sep = " "))
model <- st$SentenceTransformer('all-MiniLM-L6-v2')
embeddings <- model$encode(tablaOrdenada$contenido)

# Convertir a matriz R
emb_mat <- np$array(embeddings)

# ==============================
# 4. Reducción de dimensionalidad (UMAP)
# ==============================
set.seed(123)
umap_res <- umap(emb_mat, n_neighbors = 3, min_dist = 0.05, metric = "euclidean")

umap_df <- as.data.frame(umap_res)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$id <- tablaOrdenada$id

# ==============================
# 5. Clustering con HDBSCAN
# ==============================
clusterer <- hdbscan$HDBSCAN(min_cluster_size = as.integer(3), 
                             metric = "euclidean")
clusters <- clusterer$fit_predict(emb_mat)

umap_df$cluster <- as.factor(ifelse(clusters == -1, NA, clusters)) # NA para "ruido"

# ==============================
# 6. Visualización
# ==============================
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2", na.value = "grey70") +
  labs(title = "Mapa temático de abstracts (Embeddings + UMAP + HDBSCAN)",
       subtitle = "NA = abstract no asignado a ningún cluster",
       color = "Cluster")
