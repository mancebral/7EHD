library(readr)
library(stringr)
library(tidyverse)

setwd("~/RPROJECTS/7EHD")

# Ruta a tu archivo de texto con las líneas (una por fila)
ruta_txt <- "abstracts-4.txt"  # Cambia a tu ruta 

# Leer todas las líneas
lineas <- read_lines(ruta_txt)

#convertir a tibble
tabla <- tibble(lineas)

#cadena para detectar columnas
pat <- regex("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", dotall = TRUE)

#generar una columna con la última fecha de modificación del abstract
tabla$modificacion <- vapply(str_extract_all(tabla$lineas, pat),
                   function(x) if (length(x)) tail(x, 1) else NA_character_,
                   character(1))

#ordenar el resto de columnas de interés
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

#limpiar algunas columnas
tablaOrdenada <- tablaOrdenada %>% 
  mutate(id = str_remove_all(id, "[^0-9]")) %>% 
  select(-c(otros_campos, lineas)) %>%
  mutate(autores = str_remove(autores, ".*NULL, NULL, NULL, '"))

#cambiar los status según IDs. Aquí CUIDADO porque los IDs dependen de qué campos hayan creado como posibles status.
tablaOrdenada <- tablaOrdenada %>% 
  mutate(status=ifelse(id4==5, "Aceptado con cambios",
                       ifelse(id4==3, "Aceptado", 
                              ifelse(id4==4, "Rechazado",
                                     ifelse(id4==1, "Pendiente de asignación",
                                            "En revisión")))))

#cambiar los tópicos según IDs. También CUIDADO porque los IDs dependen de qué campos hayan creado como posibles topicos.
tablaOrdenada <- tablaOrdenada %>% 
  mutate(topico=ifelse(id2==1, "Geopolíticas, imaginarios de fronteras y cultura digital",
                       ifelse(id2==2, "Patrimonio cultural, arte y sociedad", 
                              ifelse(id2==4, "Analítica cultural y métodos digitales",
                                     "Docencia de las Humanidades Digitales"))))

#limpiar columnas
tablaOrdenada <- tablaOrdenada %>% 
  select(-c(ids_full: id7))

#guardar el objeto de R
saveRDS(tablaOrdenada, "abstracts.rds")

#exportar a csv para desplegar en la web con el título como hipervínculo
tablaOrdenada %>% 
  select(-c(abstract, contacto)) %>% 
  mutate(title=paste0('<a href="https://viiencuentro.humanidadesdigitales.net/envios-aceptados/?abs_id=', id, 
                     '"><b>', title, '</b></a>')) %>% 
  select(-id) %>%
  mutate(tipo=sub(" \\|.*$", "", tipo)) %>% 
  write.csv("abstracts.csv", row.names = FALSE)

#exportar a CSV para cotejo
tablaOrdenada %>%
  mutate(tipo=sub(" \\|.*$", "", tipo)) %>% 
  mutate(tipo = str_trim(tipo)) %>% 
  write.csv("check_propuestas.csv", row.names = FALSE)

#obtener las keywords
keywords <- paste(tablaOrdenada$keywords, collapse = ", ") %>% 
  str_split(",") %>%
  unlist() %>%
  str_trim() %>%
  tolower() %>%
  unique()

count(keywords)

#keywords to JSON para utilizar con javaScript                             
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

#autores to JSON para utilizar con javaScript                             
json_autores <-jsonlite::toJSON(autores, auto_unbox = TRUE)
cat(json_autores)

#mapa de keywords
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
