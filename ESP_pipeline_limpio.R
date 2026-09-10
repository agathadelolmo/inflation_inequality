pacman::p_load(dplyr, haven, labelled, readxl, writexl, tidyr, sjlabelled,
               glue, moderndive, ggplot2, janitor, qs, here, utils, openxlsx,
               readr, tidyverse, scales, fixest, broom, ggrepel, Hmisc, data.table)

conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::filter)

paleta_quintil <- c(
  "Quintil 1" = "#d73027", "Quintil 2" = "#fc8d59", "Quintil 3" = "#999999",
  "Quintil 4" = "#4575b4", "Quintil 5" = "#313695"
)

coicop_labels <- data.frame(
  COICOP = 1:12,
  COICOP_label = c("Alimentos y bebidas no alcohólicas", "Bebidas alcohólicas y tabaco",
                   "Vestido y calzado", "Vivienda", "Menaje", "Medicina", "Transporte",
                   "Comunicaciones", "Ocio y cultura", "Enseñanza",
                   "Hoteles, cafés y restaurantes", "Otros bienes y servicios")
)

# ==============================================================================
# 1. LECTURA Y LIMPIEZA DE MICRODATOS EPF (2006-2025)
# ==============================================================================

ruta_crudos <- file.path(dirname(getwd()), "datos_crudos")
ruta_limpios <- file.path(dirname(getwd()), "datos_limpios")
survey_anios <- 2006:2025

for (anio in survey_anios) {

  if (anio %in% 2006:2015) {
    relacion_labels <- data.frame(
      RELACION = 1:6,
      RELACION_label = c("Sustentador principal", "Cónyuge o pareja",
                         "Hijo del sustentador principal y/o pareja",
                         "Padre o madre del sustentador principal",
                         "Padre o madre del cónyuge o pareja", "Otro")
    )
    posiciones <- fwf_positions(
      start = c(5, 10, 16, 17, 20, 30), end = c(9, 11, 16, 18, 20, 30),
      col_names = c("NUMERO", "NORDEN", "RELACION", "EDAD", "SEXO", "ESTUDIOS")
    )
    esp_mhh <- read_fwf(
      file.path(ruta_crudos, paste0("ESP_", anio), "Data", "datos_epf",
                paste0("Fichero de usuario de miembros a", anio)),
      col_positions = posiciones, col_types = cols(.default = "c")
    ) %>%
      mutate(NUMERO = as.numeric(NUMERO), NORDEN = as.numeric(NORDEN))

  } else if (anio %in% 2016:2023) {
    relacion_labels <- data.frame(
      RELACION = 1:6,
      RELACION_label = c("Sustentador principal", "Cónyuge o pareja",
                         "Hijo del sustentador principal y/o pareja",
                         "Padre o madre del sustentador principal",
                         "Padre o madre del cónyuge o pareja", "Otro")
    )
    esp_mhh <- read_dta(file.path(ruta_crudos, paste0("ESP_", anio), "Data",
                                   "EPFmhogar", "STATA", paste0("EPFmhogar_", anio, ".dta"))) %>%
      rename(RELACION = RELASP)

  } else {
    relacion_labels <- data.frame(
      RELACION = c(NA, 10, 20, 50, 60, 30, 40, 70, 80, 90, 95, 99),
      RELACION_label = c("Sustentador principal", "Cónyuge o pareja",
                         "Hijo del sustentador principal y/o pareja",
                         "Padre o madre del sustentador principal",
                         "Padre o madre del cónyuge o pareja",
                         rep("Otro", 7))
    )
    esp_mhh <- read_dta(file.path(ruta_crudos, paste0("ESP_", anio), "Data",
                                   "EPFmhogar", "STATA", paste0("EPFmhogar_", anio, ".dta"))) %>%
      rename(RELACION = RELACION_01) %>%
      mutate(NUMERO = as.numeric(NUMERO), NORDEN = as.numeric(NORDEN))
  }

  individuos_df <- esp_mhh %>%
    mutate(hh_id = paste0("ESP-", anio, "-", NUMERO),
           ind_id = paste0(hh_id, "-", NORDEN),
           ESTUDIOS = as.numeric(ESTUDIOS), RELACION = as.numeric(RELACION),
           EDAD = as.numeric(EDAD), SEXO = as.numeric(SEXO)) %>%
    left_join(relacion_labels) %>%
    mutate(
      SEXO_label = case_when(SEXO == 1 ~ "Hombre", SEXO == 6 ~ "Mujer", TRUE ~ NA),
      ESTUDIOS_label = case_when(
        ESTUDIOS == 1 ~ "No sabe leer o escribir", ESTUDIOS == 2 ~ "Educación primaria",
        ESTUDIOS == 3 ~ "ESO, EGB o Bachiller Elemental",
        ESTUDIOS == 4 ~ "Bachiller, BUP, COU, Bachiller Superior, FP Básica y Media",
        ESTUDIOS == 5 ~ "FP de Grado Superior, FPII",
        ESTUDIOS == 6 ~ "Diplomatura, Arquitectura e Ingeniería Técnicas",
        ESTUDIOS == 7 ~ "Licenciatura, Arquitectura, Ingeniería, másteres",
        ESTUDIOS == 8 ~ "Doctorado universitario", TRUE ~ NA
      ),
      ESTUDIOS_label2 = case_when(
        ESTUDIOS %in% 1:2 ~ "Sin educación", ESTUDIOS %in% 3:4 ~ "Instituto",
        ESTUDIOS %in% 5:8 ~ "Universidad", TRUE ~ NA
      )
    ) %>%
    select(hh_id, ind_id, RELACION, RELACION_label, EDAD, SEXO, SEXO_label,
           ESTUDIOS, ESTUDIOS_label, ESTUDIOS_label2) %>%
    filter(!(EDAD < 22 & ESTUDIOS == 8 | EDAD < 14 & ESTUDIOS %in% 4:8 | EDAD < 18 & RELACION == 1))

  if (anio %in% 2006:2015) {
    posiciones <- fwf_positions(start = c(5, 18, 10), end = c(9, 22, 11),
                                 col_names = c("NUMERO", "FACTOR", "CCAA"))
    esp_hh <- read_fwf(
      file.path(ruta_crudos, paste0("ESP_", anio), "Data", "datos_epf",
                paste0("Fichero de usuario de hogar a", anio)),
      col_positions = posiciones, col_types = cols(.default = "c")
    ) %>%
      mutate(NUMERO = as.numeric(NUMERO), FACTOR = as.numeric(FACTOR))
  } else {
    esp_hh <- read_dta(file.path(ruta_crudos, paste0("ESP_", anio), "Data",
                                  "EPFhogar", "STATA", paste0("EPFhogar_", anio, ".dta")))
  }

  region_codes <- read.xlsx(file.path(ruta_crudos, paste0("ESP_", anio), "Data",
                                       "region_gadm_codes.xlsx")) %>%
    select(-survey_id)

  region_labels <- data.frame(
    CCAA = substr(101:119, 2, 3),
    region1 = c("Andlaucia", "Aragon", "Asturias, Principado de", "Balears, Illes",
                "Canarias", "Cantabria", "Castilla y Leon", "Castilla-La Mancha",
                "Cataluna", "Comunitat Valenciana", "Extremadura", "Galicia",
                "Madrid, Comunidad de", "Murcia, Region de", "Navarra, Comudad Foral de",
                "Pais Vasco", "Rioja, La", "Ceuta", "Melilla")
  )

  hogares_df <- esp_hh %>%
    mutate(NUMERO = as.numeric(NUMERO), hh_id = paste0("ESP-", anio, "-", NUMERO)) %>%
    left_join(region_labels, by = "CCAA") %>%
    left_join(region_codes, by = "region1") %>%
    select(hh_id, FACTOR, CCAA, CCAA_label = region1, gid_1)

  if (anio %in% 2006:2015) {
    posiciones <- fwf_positions(start = c(5, 10, 15, 48), end = c(9, 14, 29, 63),
                                 col_names = c("NUMERO", "CODIGO", "GASTO", "comp"))
    esp_exp <- read_fwf(
      file.path(ruta_crudos, paste0("ESP_", anio), "Data", "datos_epf",
                paste0("Fichero de usuario de gastos a", anio)),
      col_positions = posiciones, col_types = cols(.default = "c")
    ) %>%
      mutate(GASTO = as.numeric(GASTO) / 100)
  } else {
    esp_exp <- read_dta(file.path(ruta_crudos, paste0("ESP_", anio), "Data",
                                   "EPFgastos", "STATA", paste0("EPFgastos_", anio, ".dta")))
  }

  gastos_df <- esp_exp %>%
    mutate(NUMERO = as.numeric(NUMERO), hh_id = paste0("ESP-", anio, "-", NUMERO)) %>%
    select(hh_id, GASTO, CODIGO)

  ruta_output <- file.path(dirname(getwd()), "datos_limpios", paste0("ESP_", anio))
  if (!dir.exists(ruta_output)) dir.create(ruta_output, showWarnings = FALSE, recursive = TRUE)

  write.xlsx(gastos_df, file.path(ruta_output, "gastos_df.xlsx"))
  write.xlsx(individuos_df, file.path(ruta_output, "individuos_df.xlsx"))
  write.xlsx(hogares_df, file.path(ruta_output, "hogares_df.xlsx"))
}

# ==============================================================================
# 2. CARGA CONSOLIDADA
# ==============================================================================

lista_gastos <- vector("list", length(survey_anios))
lista_individuos <- vector("list", length(survey_anios))
lista_hogares <- vector("list", length(survey_anios))
names(lista_gastos) <- names(lista_individuos) <- names(lista_hogares) <- as.character(survey_anios)

for (anio in survey_anios) {
  carpeta <- file.path(ruta_limpios, paste0("ESP_", anio))
  lista_gastos[[as.character(anio)]] <- read.xlsx(file.path(carpeta, "gastos_df.xlsx"))
  lista_individuos[[as.character(anio)]] <- read.xlsx(file.path(carpeta, "individuos_df.xlsx"))
  lista_hogares[[as.character(anio)]] <- read.xlsx(file.path(carpeta, "hogares_df.xlsx"))
}

gastos_total <- bind_rows(lista_gastos)
individuos_total <- bind_rows(lista_individuos)
hogares_total <- bind_rows(lista_hogares)
rm(lista_gastos, lista_individuos, lista_hogares)
gc()

gastos_total <- gastos_total %>%
  left_join(hogares_total, by = "hh_id") %>%
  mutate(GASTO = GASTO / FACTOR) %>%
  select(hh_id, GASTO, CODIGO)

# ==============================================================================
# 3. CORRESPONDENCIA COICOP 2018 -> ECOICOP (solo 2024-2025)
# ==============================================================================

parsear_destino <- function(ecoicop_txt) {
  partes <- str_split(ecoicop_txt, "\\+")[[1]] %>% str_trim()
  es_parcial <- str_detect(partes, "^parte")
  codigos <- str_remove(partes, "^parte\\s*") %>% str_remove_all("\\.")
  tibble(codigo_destino = codigos, es_parcial = es_parcial)
}

parsear_correspondencia <- function(codigo_origen, ecoicop_txt, coef_txt) {
  destinos <- parsear_destino(ecoicop_txt)
  n <- nrow(destinos)
  coef_na <- length(coef_txt) == 1 && is.na(coef_txt)

  if (n == 1) {
    peso <- if (coef_na) 1 else as.numeric(str_replace(as.character(coef_txt), ",", "."))
    return(tibble(CODIGO = codigo_origen, CODIGO_nuevo = destinos$codigo_destino, PESO = peso))
  }
  if (coef_na) {
    return(tibble(CODIGO = codigo_origen, CODIGO_nuevo = destinos$codigo_destino, PESO = rep(1 / n, n)))
  }

  coefs <- str_split(as.character(coef_txt), "/")[[1]] %>% str_replace(",", ".") %>% as.numeric()
  n_parciales <- sum(destinos$es_parcial)

  if (length(coefs) == n) {
    pesos <- coefs
  } else if (length(coefs) == n_parciales) {
    residuo <- 1 - sum(coefs)
    n_integros <- n - n_parciales
    pesos <- numeric(n)
    pesos[destinos$es_parcial] <- coefs
    if (n_integros > 0) pesos[!destinos$es_parcial] <- residuo / n_integros
  } else {
    pesos <- rep(1 / n, n)
  }
  tibble(CODIGO = codigo_origen, CODIGO_nuevo = destinos$codigo_destino, PESO = pesos)
}

tabla_raw <- read_xlsx(file.path(ruta_crudos, "ESP_2024", "Data", "Corresp_COICOP_2018_ECOICOP.xlsx")) %>%
  mutate(CODIGO = str_remove_all(`Códigos COICOP/Recogida (5d)`, "\\."))

corr_coicop <- pmap_dfr(
  list(tabla_raw$CODIGO, tabla_raw$`ECOICOP/Recogida (5d)`, tabla_raw$`Coeficientes recogida`),
  parsear_correspondencia
)

gastos_2024_recodificados <- gastos_total %>%
  mutate(anio = substr(hh_id, 5, 8), CODIGO_original = str_remove_all(CODIGO, "\\.")) %>%
  filter(anio %in% c("2024", "2025"), CODIGO_original %in% corr_coicop$CODIGO) %>%
  left_join(corr_coicop, by = c("CODIGO_original" = "CODIGO")) %>%
  mutate(GASTO = GASTO * PESO, CODIGO = CODIGO_nuevo) %>%
  select(-CODIGO_original, -CODIGO_nuevo, -PESO)

gastos_2024_resto <- gastos_total %>%
  mutate(anio = substr(hh_id, 5, 8), CODIGO_check = str_remove_all(CODIGO, "\\.")) %>%
  filter(!(anio %in% c("2024", "2025") & CODIGO_check %in% corr_coicop$CODIGO)) %>%
  select(-CODIGO_check, -anio)

gastos_total <- bind_rows(gastos_2024_resto, gastos_2024_recodificados %>% select(-anio))

# ==============================================================================
# 4. CONSTRUCCIÓN DE GASTO AGREGADO Y DETALLADO
# ==============================================================================

gastos_agr_total <- gastos_total %>%
  mutate(anio = substr(hh_id, 5, 8), COICOP = as.numeric(substr(CODIGO, 1, 2))) %>%
  group_by(COICOP, hh_id) %>%
  summarise(GASTO = sum(GASTO, na.rm = TRUE), .groups = "drop") %>%
  left_join(hogares_total, by = "hh_id") %>%
  left_join(coicop_labels, by = "COICOP")

gastos_det_total <- gastos_total %>%
  mutate(anio = substr(hh_id, 5, 8),
         COICOP_1 = as.numeric(substr(CODIGO, 1, 2)),
         COICOP_2 = as.numeric(substr(CODIGO, 3, 3)),
         COICOP_3 = as.numeric(substr(CODIGO, 4, 4)),
         COICOP_4 = as.numeric(substr(CODIGO, 5, 5))) %>%
  left_join(hogares_total, by = "hh_id") %>%
  left_join(coicop_labels %>% rename(COICOP_1 = COICOP), by = "COICOP_1")

individuos_total <- individuos_total %>%
  mutate(anio = substr(hh_id, 5, 8)) %>%
  filter(hh_id %in% gastos_total$hh_id)

hogares_total <- hogares_total %>%
  mutate(anio = substr(hh_id, 5, 8)) %>%
  filter(hh_id %in% gastos_total$hh_id)

# ==============================================================================
# 5. DATOS DE INFLACIÓN (IPC AGREGADO Y DETALLADO)
# ==============================================================================

datos_ipc_agr <- read_xlsx(file.path(ruta_crudos, "datos_IPC.xlsx")) %>%
  pivot_longer(cols = -Categoría, names_to = c("anio", "mes"),
               names_pattern = "(\\d{4})M(\\d{2}).*", values_to = "ipc") %>%
  rename(categoria = "Categoría") %>%
  filter(categoria != "Índice general") %>%
  mutate(COICOP = as.numeric(substr(categoria, 1, 2)))

datos_inflacion_agr <- datos_ipc_agr %>%
  group_by(anio, COICOP) %>%
  summarise(ipc = sum(ipc) / 12, .groups = "drop") %>%
  group_by(COICOP) %>%
  arrange(anio, .by_group = TRUE) %>%
  mutate(inflacion = ipc / lag(ipc) - 1) %>%
  ungroup() %>%
  filter(anio %in% 2006:2025)

datos_ipc_det_2017_2025 <- read_xlsx(file.path(ruta_crudos, "datos_IPC_det.xlsx")) %>%
  pivot_longer(cols = -Categoría, names_to = c("anio", "mes"),
               names_pattern = "(\\d{4})M(\\d{2}).*", values_to = "ipc",
               values_transform = list(ipc = as.numeric)) %>%
  rename(categoria = "Categoría") %>%
  filter(categoria != "Índice general") %>%
  mutate(COICOP_1 = as.numeric(substr(categoria, 1, 2)),
         COICOP_2 = as.numeric(substr(categoria, 3, 3)),
         COICOP_3 = as.numeric(substr(categoria, 4, 4)),
         COICOP_4 = as.numeric(substr(categoria, 5, 5))) %>%
  filter(anio %in% 2017:2025)

datos_ipc_det_2006_2016_nvl3 <- read_xlsx(file.path(ruta_crudos, "datos_IPC_det_nvl3.xlsx")) %>%
  pivot_longer(cols = -Categoría, names_to = c("anio", "mes"),
               names_pattern = "(\\d{4})M(\\d{2}).*", values_to = "ipc",
               values_transform = list(ipc = as.numeric)) %>%
  rename(categoria = "Categoría") %>%
  filter(categoria != "Índice general") %>%
  mutate(CODIGO = str_extract(categoria, "(?<=\\()[^)]+")) %>%
  mutate(COICOP_1 = as.numeric(substr(CODIGO, 1, 2)),
         COICOP_2 = as.numeric(substr(CODIGO, 4, 4)),
         COICOP_3 = as.numeric(substr(CODIGO, 6, 6))) %>%
  mutate(COICOP_2 = ifelse(is.na(COICOP_2), 0, COICOP_2),
         COICOP_3 = ifelse(is.na(COICOP_3), 0, COICOP_3))

datos_ipc_det_2006_2016 <- datos_ipc_det_2017_2025 %>%
  select(categoria, COICOP_1, COICOP_2, COICOP_3) %>%
  unique() %>%
  cross_join(expand_grid(anio = 2005:2016, mes = sprintf("%02d", 1:12))) %>%
  mutate(anio = as.character(anio), mes = as.character(mes)) %>%
  left_join(datos_ipc_det_2006_2016_nvl3 %>% select(-categoria),
            by = c("COICOP_1", "COICOP_2", "COICOP_3", "anio", "mes")) %>%
  left_join(datos_ipc_det_2017_2025 %>% select(COICOP_4, categoria)) %>%
  select(names(datos_ipc_det_2017_2025))

datos_ipc_det <- rbind(datos_ipc_det_2017_2025, datos_ipc_det_2006_2016) %>%
  arrange(categoria, anio, mes) %>%
  group_by(categoria) %>%
  mutate(ipc = na_if(ipc, 0)) %>%
  fill(ipc, .direction = "down") %>%
  ungroup() %>%
  unique()

rm(datos_ipc_det_2017_2025, datos_ipc_det_2006_2016, datos_ipc_det_2006_2016_nvl3)

datos_inflacion_det <- datos_ipc_det %>%
  group_by(anio, COICOP_1, COICOP_2, COICOP_3, COICOP_4) %>%
  summarise(ipc = sum(ipc) / 12, .groups = "drop") %>%
  group_by(COICOP_1, COICOP_2, COICOP_3, COICOP_4) %>%
  arrange(anio, .by_group = TRUE) %>%
  mutate(inflacion = ipc / lag(ipc) - 1) %>%
  ungroup() %>%
  filter(anio %in% 2006:2025)

rm(datos_ipc_det)

# ==============================================================================
# 6. CONSTRUCCIÓN DE LA INFLACIÓN POR HOGAR
# ==============================================================================

## 6.1 Gasto total del hogar (escala de equivalencia)
gasto_hogar_total <- gastos_total %>%
  mutate(anio = substr(hh_id, 5, 8)) %>%
  group_by(hh_id, anio) %>%
  summarise(GASTO_TOTAL = sum(GASTO, na.rm = TRUE), .groups = "drop")

## 6.2 Inflación por hogar, nivel detallado
setDT(gastos_det_total)

prop_df_det <- gastos_det_total[
  , .(GASTO = sum(GASTO, na.rm = TRUE), FACTOR = first(FACTOR)),
  by = .(hh_id, anio, COICOP_1, COICOP_2, COICOP_3, COICOP_4)
][
  , PROP := GASTO / sum(GASTO, na.rm = TRUE), by = hh_id
][
  !is.na(PROP) & !is.nan(PROP)
]
prop_df_det <- as_tibble(prop_df_det)
rm(gastos_det_total)

inflacion_n4 <- datos_inflacion_det %>% select(anio, COICOP_1, COICOP_2, COICOP_3, COICOP_4, inflacion_n4 = inflacion)
inflacion_n3 <- datos_inflacion_det %>% group_by(anio, COICOP_1, COICOP_2, COICOP_3) %>%
  summarise(inflacion_n3 = mean(inflacion, na.rm = TRUE), .groups = "drop")
inflacion_n2 <- datos_inflacion_det %>% group_by(anio, COICOP_1, COICOP_2) %>%
  summarise(inflacion_n2 = mean(inflacion, na.rm = TRUE), .groups = "drop")
inflacion_n1 <- datos_inflacion_agr %>% select(anio, COICOP_1 = COICOP, inflacion_n1 = inflacion)

prop_df_det <- prop_df_det %>%
  left_join(inflacion_n4, by = c("anio", "COICOP_1", "COICOP_2", "COICOP_3", "COICOP_4")) %>%
  left_join(inflacion_n3, by = c("anio", "COICOP_1", "COICOP_2", "COICOP_3")) %>%
  left_join(inflacion_n2, by = c("anio", "COICOP_1", "COICOP_2")) %>%
  left_join(inflacion_n1, by = c("anio", "COICOP_1")) %>%
  mutate(inflacion = coalesce(inflacion_n4, inflacion_n3, inflacion_n2, inflacion_n1))

inflacion_hogar_det <- prop_df_det %>%
  mutate(CONTRIBUCION = PROP * inflacion) %>%
  group_by(hh_id, anio) %>%
  summarise(INFLACION_HOGAR = sum(CONTRIBUCION, na.rm = TRUE), .groups = "drop")

## 6.3 Inflación por hogar, nivel agregado (capítulo 4)
prop_df_agr <- gastos_agr_total %>%
  group_by(hh_id) %>%
  mutate(PROP = GASTO / sum(GASTO, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(PROP), !is.nan(PROP))
rm(gastos_agr_total)

inflacion_hogar_agr <- prop_df_agr %>%
  mutate(anio = substr(hh_id, 5, 8)) %>%
  left_join(datos_inflacion_agr %>% select(anio, COICOP, inflacion), by = c("anio", "COICOP")) %>%
  mutate(CONTRIBUCION = PROP * inflacion) %>%
  group_by(hh_id, anio) %>%
  summarise(INFLACION_HOGAR_AGR = sum(CONTRIBUCION, na.rm = TRUE), .groups = "drop")
rm(prop_df_agr)
gc()

## 6.4 Escala de equivalencia
unidades_consumo <- individuos_total %>%
  group_by(hh_id, anio) %>%
  mutate(peso_ocde = case_when(
    n() == 1 ~ 1, RELACION_label == "Sustentador principal" ~ 1,
    EDAD < 14 ~ 0.5, TRUE ~ 0.7
  )) %>%
  summarise(TAM_HOGAR = n(), UC_OCDE = sum(peso_ocde), .groups = "drop")

## 6.5 Quintiles de gasto equivalente
quintiles_df <- unidades_consumo %>%
  left_join(gasto_hogar_total, by = c("hh_id", "anio")) %>%
  left_join(hogares_total %>% select(hh_id, FACTOR, CCAA_label), by = "hh_id") %>%
  mutate(GASTO = GASTO_TOTAL / 12, PERCAP = GASTO_TOTAL / UC_OCDE / 12,
         FACTOR_PERCAP = FACTOR * TAM_HOGAR) %>%
  arrange(anio, PERCAP) %>%
  group_by(anio) %>%
  mutate(ACUM_PERS = cumsum(FACTOR_PERCAP), QUINTIL = ACUM_PERS / sum(FACTOR_PERCAP),
         QUINTIL_label = case_when(
           QUINTIL <= 0.2 ~ "Quintil 1", QUINTIL <= 0.4 ~ "Quintil 2",
           QUINTIL <= 0.6 ~ "Quintil 3", QUINTIL <= 0.8 ~ "Quintil 4", TRUE ~ "Quintil 5"
         )) %>%
  ungroup()
rm(hogares_total, gasto_hogar_total, unidades_consumo)

## 6.6 Dataset final a nivel de hogar
resultado_hogar <- quintiles_df %>%
  select(hh_id, anio, FACTOR, TAM_HOGAR, GASTO, PERCAP, QUINTIL, QUINTIL_label, CCAA_label) %>%
  left_join(inflacion_hogar_det, by = c("hh_id", "anio")) %>%
  left_join(inflacion_hogar_agr, by = c("hh_id", "anio"))
rm(inflacion_hogar_det)

# ==============================================================================
# CAPÍTULO 1 — EVOLUCIÓN DE LA DESIGUALDAD INFLACIONARIA
# ==============================================================================

## 1.1 Brecha Q1-Q5
inflacion_quintil <- resultado_hogar %>%
  group_by(anio, QUINTIL_label) %>%
  summarise(INFLACION_MEDIA = weighted.mean(INFLACION_HOGAR, w = FACTOR, na.rm = TRUE), .groups = "drop") %>%
  mutate(anio_num = as.integer(anio))

brecha_inflacion <- inflacion_quintil %>%
  select(anio, anio_num, QUINTIL_label, INFLACION_MEDIA) %>%
  pivot_wider(names_from = QUINTIL_label, values_from = INFLACION_MEDIA) %>%
  mutate(brecha_Q1_Q5 = `Quintil 1` - `Quintil 5`)

ggplot(brecha_inflacion, aes(anio_num, brecha_Q1_Q5, fill = brecha_Q1_Q5 > 0)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#4575b4"),
                     labels = c("TRUE" = "Más inflación en Q1", "FALSE" = "Más inflación en Q5"),
                     name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = unique(brecha_inflacion$anio_num)) +
  labs(title = "Brecha de inflación entre el quintil más pobre y el más rico",
       subtitle = "Diferencia Q1 − Q5 en inflación anual", x = NULL, y = "Brecha (Q1 − Q5)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2))

# Para todo el periodo
brecha_inflacion %>%
  summarise(
    brecha_media_periodo = mean(brecha_Q1_Q5, na.rm = TRUE),
    brecha_sd = sd(brecha_Q1_Q5, na.rm = TRUE),
    brecha_acumulada = sum(brecha_Q1_Q5, na.rm = TRUE)
  )

# Para 2006-2021 (igual que Basso, Dimakou y Pidkuyko (2022))
brecha_inflacion %>%
  filter(anio %in% 2006:2021) %>%
  summarise(
    brecha_media_periodo = mean(brecha_Q1_Q5, na.rm = TRUE),
    brecha_sd = sd(brecha_Q1_Q5, na.rm = TRUE),
    brecha_acumulada = sum(brecha_Q1_Q5, na.rm = TRUE)
  )

# Años más sensibles
brecha_inflacion %>%
  arrange(-abs(brecha_Q1_Q5)) %>%
  select(anio, brecha_Q1_Q5) %>%
  head(3)

# Para 2006-2015 (el más cercano a Jaravel (2019), 2004-2015)
brecha_inflacion %>%
  filter(anio %in% 2006:2015) %>%
  summarise(
    brecha_media_periodo = mean(brecha_Q1_Q5, na.rm = TRUE),
    q1_media_periodo = mean(`Quintil 1`,  na.rm = TRUE),
    q5_media_periodo = mean(`Quintil 5`,  na.rm = TRUE),
  )


## 1.2 Índice de dispersión (complemento propio)
indice_desigualdad <- resultado_hogar %>%
  group_by(anio) %>%
  summarise(media_ponderada = weighted.mean(INFLACION_HOGAR, w = FACTOR, na.rm = TRUE),
            var_ponderada = Hmisc::wtd.var(INFLACION_HOGAR, weights = FACTOR, na.rm = TRUE),
            sd_ponderada = sqrt(var_ponderada), .groups = "drop") %>%
  mutate(anio_num = as.integer(anio))

ggplot(indice_desigualdad, aes(anio_num, sd_ponderada)) +
  geom_col(fill = "#313695", width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = unique(indice_desigualdad$anio_num)) +
  labs(title = "Dispersión de la inflación entre todos los hogares, 2006-2025",
       subtitle = "Desviación típica ponderada", x = NULL, y = "Dispersión entre hogares (p.p.)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2))

# Años menos sensibles
indice_desigualdad %>%
  arrange(abs(sd_ponderada)) %>%
  head(3)

# Años más sensibles
indice_desigualdad %>%
  arrange(-abs(sd_ponderada)) %>%
  head(3)


## 1.3 Validación estadística
modelo_agregado <- feols(
  INFLACION_HOGAR ~ i(QUINTIL_label, ref = "Quintil 1") | anio,
  data = resultado_hogar, weights = ~FACTOR, vcov = "hetero"
)
summary(modelo_agregado)

modelo_anios <- feols(
  INFLACION_HOGAR ~ i(QUINTIL_label, anio, ref = "Quintil 1") | anio,
  data = resultado_hogar, weights = ~FACTOR, vcov = "hetero"
)
summary(modelo_anios)

modelo_solo_anio <- feols(
  INFLACION_HOGAR ~ 1 | anio,
  data = resultado_hogar, weights = ~FACTOR, vcov = "hetero"
  )
summary(modelo_solo_anio)

coefs_anios <- broom::tidy(modelo_anios, conf.int = TRUE) %>%
  mutate(quintil = str_extract(term, "Quintil \\d"), anio_num = as.integer(str_extract(term, "\\d{4}$")))

crisis_periodos <- tibble(
  inicio = c(2007.5, 2010.5, 2020.5), fin = c(2009.5, 2012.5, 2023.5),
  etiqueta = c("Crisis\nfinanciera", "Crisis de\ndeuda soberana", "Crisis\nenergética"),
  x_texto = c(2008.5, 2011.5, 2022)
)

ggplot(coefs_anios, aes(anio_num, estimate, color = quintil, group = quintil)) +
  geom_rect(data = crisis_periodos, inherit.aes = FALSE,
            aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf), fill = "grey85", alpha = 0.5) +
  geom_text(data = crisis_periodos, inherit.aes = FALSE, aes(x = x_texto, y = 0.011, label = etiqueta),
            size = 3, color = "grey40", lineheight = 0.85, fontface = "italic") +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = quintil), alpha = 0.08, color = NA) +
  scale_color_manual(values = paleta_quintil[-1], name = NULL) +
  scale_fill_manual(values = paleta_quintil[-1], guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = unique(coefs_anios$anio_num)) +
  labs(title = "Brecha respecto a Quintil 1, por año", x = NULL, y = "Diferencia respecto a Quintil 1 (p.p.)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2))

# ==============================================================================
# CAPÍTULO 2 — MECANISMO
# ==============================================================================

poblacion_quintil <- quintiles_df %>%
  group_by(anio, QUINTIL_label) %>%
  summarise(FACTOR_TOTAL = sum(FACTOR, na.rm = TRUE), .groups = "drop")

## 2.1 Cesta de consumo por quintil
cesta_quintil_det <- prop_df_det %>%
  filter(!is.na(COICOP_1)) %>%
  left_join(quintiles_df %>% select(hh_id, anio, QUINTIL_label), by = c("hh_id", "anio")) %>%
  filter(!is.na(QUINTIL_label)) %>%
  group_by(anio, QUINTIL_label, COICOP_1, COICOP_2, COICOP_3, COICOP_4) %>%
  summarise(GASTO_POND = sum(GASTO * FACTOR, na.rm = TRUE), .groups = "drop") %>%
  group_by(anio, QUINTIL_label) %>%
  mutate(PROP_MEDIA = GASTO_POND / sum(GASTO_POND)) %>%
  ungroup()

cesta_quintil_n1_dif <- cesta_quintil_det %>%
  left_join(coicop_labels %>% rename(COICOP_1 = COICOP), by = "COICOP_1") %>%
  group_by(anio, QUINTIL_label, COICOP_1, COICOP_label) %>%
  summarise(PROP_MEDIA = sum(PROP_MEDIA, na.rm = TRUE), .groups = "drop") %>%
  filter(QUINTIL_label %in% c("Quintil 1", "Quintil 5")) %>%
  select(anio, COICOP_1, COICOP_label, QUINTIL_label, PROP_MEDIA) %>%
  pivot_wider(names_from = QUINTIL_label, values_from = PROP_MEDIA) %>%
  mutate(diferencia_cesta = `Quintil 1` - `Quintil 5`) %>%
  filter(!is.na(COICOP_1))

cesta_quintil_n1_dif %>%
  group_by(COICOP_label) %>%
  summarise(impacto_medio = mean(diferencia_cesta, na.rm = TRUE)) %>%
  mutate(COICOP_label = fct_reorder(COICOP_label, impacto_medio)) %>%
  ggplot(aes(impacto_medio, COICOP_label, fill = impacto_medio > 0)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#4575b4"),
                     labels = c("TRUE" = "Mayor peso en la cesta de Q1", "FALSE" = "Mayor peso en la cesta de Q5"),
                     name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 0.01)) +
  labs(title = "Diferencia media histórica en el peso de cada partida entre Q1 y Q5",
       x = "Diferencia media Q1 − Q5 (peso en la cesta)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# Categorías con mayor peso
cesta_quintil_n1_dif %>%
  group_by(COICOP_label) %>%
  summarise(impacto_medio = mean(diferencia_cesta, na.rm = TRUE)) %>%
  arrange(-abs(impacto_medio))

## 2.2 Contribución por categoría a la brecha de inflación
contribucion_quintil_n1 <- prop_df_det %>%
  filter(!is.na(COICOP_1)) %>%
  left_join(quintiles_df %>% select(hh_id, anio, QUINTIL_label), by = c("hh_id", "anio")) %>%
  filter(!is.na(QUINTIL_label)) %>%
  mutate(CONTRIBUCION = PROP * inflacion) %>%
  group_by(anio, QUINTIL_label, COICOP_1) %>%
  summarise(CONTRIBUCION_POND = sum(CONTRIBUCION * FACTOR, na.rm = TRUE), .groups = "drop") %>%
  left_join(poblacion_quintil, by = c("anio", "QUINTIL_label")) %>%
  mutate(CONTRIBUCION_MEDIA = CONTRIBUCION_POND / FACTOR_TOTAL) %>%
  left_join(coicop_labels %>% rename(COICOP_1 = COICOP), by = "COICOP_1")

diferencia_q1_q5 <- contribucion_quintil_n1 %>%
  filter(QUINTIL_label %in% c("Quintil 1", "Quintil 5")) %>%
  select(anio, COICOP_1, COICOP_label, QUINTIL_label, CONTRIBUCION_MEDIA) %>%
  pivot_wider(names_from = QUINTIL_label, values_from = CONTRIBUCION_MEDIA) %>%
  mutate(diferencia_inflacion = `Quintil 1` - `Quintil 5`, anio_num = as.integer(anio)) %>%
  filter(!is.na(COICOP_1))

diferencia_q1_q5 %>%
  group_by(COICOP_label) %>%
  summarise(impacto_medio = mean(diferencia_inflacion, na.rm = TRUE)) %>%
  mutate(COICOP_label = fct_reorder(COICOP_label, impacto_medio)) %>%
  ggplot(aes(impacto_medio, COICOP_label, fill = impacto_medio > 0)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#4575b4"),
                     labels = c("TRUE" = "Penaliza más a Q1", "FALSE" = "Penaliza más a Q5"), name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 0.01)) +
  labs(title = "Impacto medio histórico de cada categoría en la brecha Q1-Q5",
       x = "Diferencia media Q1 − Q5", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# Categorías con mayor contribución
diferencia_q1_q5 %>%
  group_by(COICOP_label) %>%
  summarise(impacto_medio = mean(diferencia_inflacion, na.rm = TRUE)) %>%
  arrange(-abs(impacto_medio))


# ==============================================================================
# CAPÍTULO 3 — SESGO DE AGREGACIÓN
# ==============================================================================

indice_desigualdad_agr <- resultado_hogar %>%
  group_by(anio) %>%
  summarise(media_ponderada = weighted.mean(INFLACION_HOGAR_AGR, w = FACTOR, na.rm = TRUE),
            var_ponderada = Hmisc::wtd.var(INFLACION_HOGAR_AGR, weights = FACTOR, na.rm = TRUE),
            sd_ponderada_agr = sqrt(var_ponderada), .groups = "drop") %>%
  mutate(anio_num = as.integer(anio))

inflacion_quintil_agr <- resultado_hogar %>%
  group_by(anio, QUINTIL_label) %>%
  summarise(INFLACION_MEDIA_AGR = weighted.mean(INFLACION_HOGAR_AGR, w = FACTOR, na.rm = TRUE), .groups = "drop") %>%
  mutate(anio_num = as.integer(anio))

brecha_inflacion_agr <- inflacion_quintil_agr %>%
  select(anio, anio_num, QUINTIL_label, INFLACION_MEDIA_AGR) %>%
  pivot_wider(names_from = QUINTIL_label, values_from = INFLACION_MEDIA_AGR) %>%
  mutate(brecha_Q1_Q5_agr = `Quintil 1` - `Quintil 5`)

brecha_inflacion %>%
  select(anio_num, brecha_detallado = brecha_Q1_Q5) %>%
  left_join(brecha_inflacion_agr %>% select(anio_num, brecha_agr = brecha_Q1_Q5_agr), by = "anio_num") %>%
  pivot_longer(-anio_num, names_to = "nivel", values_to = "brecha") %>%
  ggplot(aes(anio_num, brecha, fill = nivel)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("brecha_detallado" = "#d73027", "brecha_agr" = "grey60"),
                     labels = c("brecha_detallado" = "COICOP detallado (real)", "brecha_agr" = "COICOP agregado (habitual)"),
                     name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = unique(brecha_inflacion$anio_num)) +
  labs(title = "Sesgo de agregación en la brecha Q1-Q5", x = NULL, y = "Brecha Q1 − Q5") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# Cuánto mayor es la brecha (en valor absoluto) a nivel detallado
brecha_inflacion %>%
  select(anio_num, brecha_detallado = brecha_Q1_Q5) %>%
  left_join(brecha_inflacion_agr %>% select(anio_num, brecha_agr = brecha_Q1_Q5_agr), by = "anio_num") %>%
  summarise(
    incremento_medio_pct = mean((abs(brecha_detallado) - abs(brecha_agr)) / abs(brecha_agr), na.rm = TRUE),
    anios_donde_detallado_es_mayor = sum(abs(brecha_detallado) > abs(brecha_agr), na.rm = TRUE),
    total_anios = n()
  )

# Porcentaje de datos de gasto por nivel de cascada
prop_df_det %>%
  mutate(nivel_usado = case_when(
    !is.na(inflacion_n4) ~ "n4 (detallado real)",
    !is.na(inflacion_n3) ~ "n3",
    !is.na(inflacion_n2) ~ "n2",
    !is.na(inflacion_n1) ~ "n1 (= agregado)",
    TRUE ~ "sin dato"
  )) %>%
  group_by(nivel_usado) %>%
  summarise(pct_gasto = sum(PROP, na.rm = TRUE) / n_distinct(hh_id), .groups = "drop")

# por año
prop_df_det %>%
  mutate(nivel_usado = case_when(
    !is.na(inflacion_n4) ~ "n4",
    !is.na(inflacion_n3) ~ "n3",
    !is.na(inflacion_n2) ~ "n2",
    !is.na(inflacion_n1) ~ "n1",
    TRUE ~ "sin dato"
  )) %>%
  group_by(anio, nivel_usado) %>%
  summarise(gasto_pond = sum(PROP * FACTOR, na.rm = TRUE), .groups = "drop") %>%
  group_by(anio) %>%
  mutate(pct = gasto_pond / sum(gasto_pond)) %>%
  filter(nivel_usado == "n4") %>%
  select(anio, pct) %>%
  print(n = 20)


indice_desigualdad %>%
  select(anio_num, sd_detallado = sd_ponderada) %>%
  left_join(indice_desigualdad_agr %>% select(anio_num, sd_agr = sd_ponderada_agr), by = "anio_num") %>%
  pivot_longer(-anio_num, names_to = "nivel", values_to = "sd") %>%
  ggplot(aes(anio_num, sd, fill = nivel)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("sd_detallado" = "#d73027", "sd_agr" = "grey60"),
                     labels = c("sd_detallado" = "COICOP detallado (real)", "sd_agr" = "COICOP agregado (habitual)"),
                     name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = unique(indice_desigualdad$anio_num)) +
  labs(title = "Sesgo de agregación en la dispersión entre hogares", x = NULL, y = "Dispersión entre hogares (p.p.)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# Cuánto mayor es la dispersión (en valor absoluto) a nivel detallado
indice_desigualdad %>%
  select(anio_num, sd_det = sd_ponderada ) %>%
  left_join(indice_desigualdad_agr %>% select(anio_num, sd_agr = sd_ponderada_agr), by = "anio_num") %>%
  summarise(
    incremento_medio_pct = mean((sd_det - sd_agr) / sd_agr, na.rm = TRUE),
    anios_donde_detallado_es_mayor = sum(sd_det > sd_agr, na.rm = TRUE),
    total_anios = n()
  )

# años con mayor diferencia absoluta
indice_desigualdad %>%
  select(anio_num, sd_detallado = sd_ponderada) %>%
  left_join(indice_desigualdad_agr %>% select(anio_num, sd_agr = sd_ponderada_agr), by = "anio_num") %>%
  mutate(dif = abs(sd_detallado) - abs(sd_agr)) %>%
  arrange(-dif) %>%
  head(3)

# ==============================================================================
# CAPÍTULO 4 — SIMULACIÓN DE POLÍTICA (RDL 20/2022)
# ==============================================================================

TRASLACION_IVA <- 0.90
CAMBIO_4A0_PLENO <- 1 / 1.04 - 1
CAMBIO_4A0_PARCIAL <- 1.02 / 1.04 - 1
CAMBIO_10A5_PLENO <- 1.05 / 1.10 - 1
CAMBIO_10A5_PARCIAL <- 1.075 / 1.10 - 1

cambio_bruto_por_anio <- tribble(
  ~anio, ~tipo_iva, ~cambio_bruto,
  "2023", "4_a_0", CAMBIO_4A0_PLENO,
  "2023", "10_a_5", CAMBIO_10A5_PLENO,
  "2024", "4_a_0", (9 / 12) * CAMBIO_4A0_PLENO + (3 / 12) * CAMBIO_4A0_PARCIAL,
  "2024", "10_a_5", (9 / 12) * CAMBIO_10A5_PLENO + (3 / 12) * CAMBIO_10A5_PARCIAL
)

codigos_alimentos_basicos <- datos_inflacion_det %>%
  filter(COICOP_1 == 1) %>%
  distinct(COICOP_1, COICOP_2, COICOP_3, COICOP_4) %>%
  mutate(
    tipo_iva = case_when(

      # --- 01.1.1 Cereales y derivados ---
      COICOP_2 == 1 & COICOP_3 == 1 & COICOP_4 == 1 ~ "4_a_0",  # Cereales
      COICOP_2 == 1 & COICOP_3 == 1 & COICOP_4 == 2 ~ "4_a_0",  # Harinas de cereales
      COICOP_2 == 1 & COICOP_3 == 1 & COICOP_4 == 3 ~ "4_a_0",  # Pan
      # COICOP_4 == 4 Otros productos de panadería -> NO incluido (bollería)
      # COICOP_4 == 5 Cereales de desayuno -> NO incluido (procesados/azucarados)
      COICOP_2 == 1 & COICOP_3 == 1 & COICOP_4 == 6 ~ "10_a_5", # Pastas alimenticias y cuscús
      # COICOP_4 == 9 Otros elaborados con cereales -> NO incluido

      # --- 01.1.2 Carne: NINGUNA incluida en la ley ---
      # (COICOP_3 == 2, todos los COICOP_4 -> sin match, quedan fuera)

      # --- 01.1.3 Pescado: NINGUNO incluido en la ley ---
      # (COICOP_3 == 3, todos los COICOP_4 -> sin match, quedan fuera)

      # --- 01.1.4 Leche, queso y huevos ---
      COICOP_2 == 1 & COICOP_3 == 4 & COICOP_4 == 1 ~ "4_a_0",  # Leche entera animal
      COICOP_2 == 1 & COICOP_3 == 4 & COICOP_4 == 2 ~ "4_a_0",  # Leche desnatada/semi animal
      COICOP_2 == 1 & COICOP_3 == 4 & COICOP_4 == 3 ~ "4_a_0",  # Leche conservada y nata
      # COICOP_4 == 4 Bebidas y leches VEGETALES -> NO incluido (ley exige origen animal)
      COICOP_2 == 1 & COICOP_3 == 4 & COICOP_4 == 5 ~ "4_a_0",  # Queso y requesón
      # COICOP_4 == 6 Yogures y leches fermentadas -> NO incluido
      # COICOP_4 == 7 Postres/bebidas a base de leche -> NO incluido
      COICOP_2 == 1 & COICOP_3 == 4 & COICOP_4 == 8 ~ "4_a_0",  # Huevos
      # COICOP_4 == 9 Proteínas y otros lácteos -> NO incluido

      # --- 01.1.5 Aceites y grasas ---
      COICOP_2 == 1 & COICOP_3 == 5 & COICOP_4 == 1 ~ "aceite_oliva", # Aceite de oliva
      COICOP_2 == 1 & COICOP_3 == 5 & COICOP_4 == 2 ~ "10_a_5", # Otros aceites comestibles (semillas)
      # COICOP_4 == 3 Margarinas -> NO incluido
      # COICOP_4 == 4 Mantequilla -> NO incluido
      # COICOP_4 == 9 Otras grasas animales -> NO incluido

      # --- 01.1.6 Frutas ---
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 1 ~ "4_a_0",  # Cítricos
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 2 ~ "4_a_0",  # Plátanos
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 3 ~ "4_a_0",  # Manzanas
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 4 ~ "4_a_0",  # Peras
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 5 ~ "4_a_0",  # Otras frutas de hueso
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 6 ~ "4_a_0",  # Aguacate/mango/tropicales
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 7 ~ "4_a_0",  # Fresas y frutos rojos
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 8 ~ "4_a_0",  # Otras frutas frescas
      COICOP_2 == 1 & COICOP_3 == 6 & COICOP_4 == 9 ~ "4_a_0",  # Frutas congeladas

      # --- 01.1.7 Hortalizas, legumbres, tubérculos ---
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 1 ~ "4_a_0",  # Hortalizas de hoja/tallo
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 2 ~ "4_a_0",  # Bolsas de mezcla de lechugas
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 3 ~ "4_a_0",  # Coles
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 4 ~ "4_a_0",  # Hortalizas de fruto
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 5 ~ "4_a_0",  # Leguminosas verdes
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 6 ~ "4_a_0",  # Hortalizas de raíz/bulbo y setas
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 7 ~ "4_a_0",  # Patatas y tubérculos
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 8 ~ "4_a_0",  # Legumbres
      COICOP_2 == 1 & COICOP_3 == 7 & COICOP_4 == 9 ~ "4_a_0",  # Hortalizas/tubérculos secos

      # --- 01.1.8 Azúcar y confitería: NINGUNO incluido ---
      # --- 01.1.9 Otros alimentos: NINGUNO incluido ---
      # --- 01.2 Bebidas no alcohólicas: NINGUNA incluida ---

      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(tipo_iva))

#   Aceite de oliva:            5% (ene23-jun24) -> 0% (jul-sep24) -> 2% (oct-dic24)
#   Aceites de semillas y pasta: 5% (ene23-sep24) -> 7,5% (oct-dic24)
#   Alimentos básicos (0%):     0% (ene23-sep24) -> 2% (oct-dic24)  [ya lo tenías bien]

CAMBIO_ACEITE_OLIVA_JUL_SEP24 <- 1.00 / 1.05 - 1   # de 5% a 0%
CAMBIO_ACEITE_OLIVA_OCT_DIC24 <- 1.02 / 1.05 - 1   # de 5% a 2%

# Recalcula CAMBIO_10A5_PARCIAL: ahora solo aplica a aceites de semillas/pasta,
# que sí se mantienen a 5% hasta septiembre (no solo hasta junio)
CAMBIO_10A5_PARCIAL <- 1.075 / 1.10 - 1   # de 10% (tipo original) a 7,5%, oct-dic24
# (el CAMBIO_10A5_PLENO que ya tenías, de 10% a 5%, sigue aplicando ene23-sep24
#  para este grupo, ya que la exención se extendió hasta septiembre, no junio)

# Nueva fila de cambio_bruto_por_anio para el aceite de oliva en 2024,
# ponderando 6 meses a tipo pleno (5%) + 3 meses a 0% + 3 meses a 2%
cambio_bruto_por_anio <- cambio_bruto_por_anio %>%
  bind_rows(
    tribble(
      ~anio, ~tipo_iva,       ~cambio_bruto,
      "2023", "aceite_oliva", CAMBIO_10A5_PLENO,  # igual que el resto en 2023
      "2024", "aceite_oliva",
      (6/12) * CAMBIO_10A5_PLENO +
        (3/12) * CAMBIO_ACEITE_OLIVA_JUL_SEP24 +
        (3/12) * CAMBIO_ACEITE_OLIVA_OCT_DIC24
    )
  )

cambio_bruto_por_anio <- cambio_bruto_por_anio %>%
  mutate(cambio_bruto = case_when(
    anio == "2024" & tipo_iva == "10_a_5" ~
      (9/12) * CAMBIO_10A5_PLENO + (3/12) * CAMBIO_10A5_PARCIAL,
    TRUE ~ cambio_bruto
  ))

cambios_alimentos <- datos_inflacion_det %>%
  inner_join(codigos_alimentos_basicos %>% select(COICOP_1, COICOP_2, COICOP_3, COICOP_4, tipo_iva),
             by = c("COICOP_1", "COICOP_2", "COICOP_3", "COICOP_4")) %>%
  filter(anio %in% c("2023", "2024")) %>%
  left_join(cambio_bruto_por_anio, by = c("anio", "tipo_iva")) %>%
  mutate(inflacion_nueva = inflacion - (cambio_bruto * TRASLACION_IVA)) %>%
  select(anio, COICOP_1, COICOP_2, COICOP_3, COICOP_4, inflacion_nueva)

simular_escenario_detallado <- function(cambios, cesta = prop_df_det) {
  cesta %>%
    left_join(cambios, by = c("anio", "COICOP_1", "COICOP_2", "COICOP_3", "COICOP_4")) %>%
    left_join(quintiles_df %>% select(hh_id, CCAA_label)) %>%
    mutate(inflacion_nueva = if_else(CCAA_label %in% c("Canarias", "Ceuta", "Melilla"), NA_real_, inflacion_nueva),
           inflacion_final = coalesce(inflacion_nueva, inflacion)) %>%
    mutate(CONTRIBUCION = PROP * inflacion_final) %>%
    group_by(hh_id, anio) %>%
    summarise(INFLACION_HOGAR_ESCENARIO = sum(CONTRIBUCION, na.rm = TRUE), .groups = "drop") %>%
    left_join(quintiles_df %>% select(hh_id, anio, QUINTIL_label, FACTOR), by = c("hh_id", "anio")) %>%
    filter(!is.na(QUINTIL_label)) %>%
    group_by(anio, QUINTIL_label) %>%
    summarise(INFLACION_MEDIA = weighted.mean(INFLACION_HOGAR_ESCENARIO, w = FACTOR, na.rm = TRUE), .groups = "drop") %>%
    mutate(anio_num = as.integer(anio))
}

escenario_alimentos <- simular_escenario_detallado(cambios_alimentos)

brecha_desde_escenario <- function(escenario_df) {
  escenario_df %>%
    select(anio_num, QUINTIL_label, INFLACION_MEDIA) %>%
    pivot_wider(names_from = QUINTIL_label, values_from = INFLACION_MEDIA) %>%
    mutate(brecha_escenario = `Quintil 1` - `Quintil 5`)
}

brecha_alimentos <- brecha_desde_escenario(escenario_alimentos) %>%
  mutate(escenario = "Contrafactual (sin rebaja de IVA)")

lbl_real <- "Real (con rebaja de IVA)"
lbl_contrafactual <- "Contrafactual (sin rebaja de IVA)"

brecha_inflacion %>%
  select(anio_num, brecha_escenario = brecha_Q1_Q5) %>%
  mutate(escenario = lbl_real) %>%
  bind_rows(brecha_alimentos %>% select(anio_num, brecha_escenario, escenario)) %>%
  filter(anio_num >= 2021) %>%
  mutate(escenario = factor(escenario, levels = c(lbl_real, lbl_contrafactual))) %>%
  ggplot(aes(anio_num, brecha_escenario, fill = escenario)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = setNames(c("grey50", "#fc8d59"), c(lbl_real, lbl_contrafactual)), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = 2021:2025) +
  labs(title = "Brecha Q1-Q5: escenario real vs. contrafactual sin rebaja de IVA",
       subtitle = "Contrafactual calibrado con la evaluación causal de Almunia, Martínez y Martínez (2023)",
       x = NULL, y = "Brecha (Q1 − Q5)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# Comparación por año
brecha_inflacion %>%
  select(anio_num, brecha_escenario = brecha_Q1_Q5) %>%
  mutate(escenario = lbl_real) %>%
  bind_rows(brecha_alimentos %>% select(anio_num, brecha_escenario, escenario)) %>%
  filter(anio_num >= 2021) %>%
  mutate(escenario = factor(escenario, levels = c(lbl_real, lbl_contrafactual)))

# Media de los quintiles simulación y reales
brecha_alimentos %>%
  filter(anio_num %in% c(2023, 2024)) %>%
  group_by(anio_num) %>%
  summarise(mean_q1 = mean(`Quintil 1`), mean_q5 = mean(`Quintil 5`)) %>%
  mutate(dif = mean_q1-mean_q5)

brecha_inflacion %>%
  filter(anio_num %in% c(2023, 2024)) %>%
  select(anio_num, `Quintil 1`, `Quintil 5`) %>%
  mutate(dif = `Quintil 1`-`Quintil 5`)

# Análisis 2023
brecha_alimentos %>%
  filter(anio_num == 2023) %>%
  select(anio_num, `Quintil 1`, `Quintil 5`) %>%
  left_join(brecha_inflacion %>% filter(anio_num == 2023) %>% select(anio_num, `Quintil 1`, `Quintil 5`),
            by = "anio_num", suffix = c("_contrafactual", "_real")) %>%
  mutate(
    reduccion_pct_q1 = (`Quintil 1_contrafactual` - `Quintil 1_real`) / abs(`Quintil 1_contrafactual`),
    reduccion_pct_q5 = (`Quintil 5_contrafactual` - `Quintil 5_real`) / abs(`Quintil 5_contrafactual`)
  )

# Análisis 2024
brecha_alimentos %>%
  filter(anio_num == 2024) %>%
  select(anio_num, `Quintil 1`, `Quintil 5`) %>%
  left_join(brecha_inflacion %>% filter(anio_num == 2024) %>% select(anio_num, `Quintil 1`, `Quintil 5`),
            by = "anio_num", suffix = c("_contrafactual", "_real")) %>%
  mutate(
    reduccion_pct_q1 = (`Quintil 1_contrafactual` - `Quintil 1_real`) / abs(`Quintil 1_contrafactual`),
    reduccion_pct_q5 = (`Quintil 5_contrafactual` - `Quintil 5_real`) / abs(`Quintil 5_contrafactual`)
  )


# Traducir ahorro a euros
# Reconstruir la inflación de ESCENARIO a nivel de HOGAR (sin agregar a quintil)
escenario_alimentos_hogar <- prop_df_det %>%
  left_join(cambios_alimentos, by = c("anio", "COICOP_1", "COICOP_2", "COICOP_3", "COICOP_4")) %>%
  left_join(quintiles_df %>% select(hh_id, CCAA_label), by = "hh_id") %>%
  mutate(
    inflacion_nueva = if_else(CCAA_label %in% c("Canarias", "Ceuta", "Melilla"), NA_real_, inflacion_nueva),
    inflacion_final = coalesce(inflacion_nueva, inflacion)
  ) %>%
  mutate(CONTRIBUCION = PROP * inflacion_final) %>%
  group_by(hh_id, anio) %>%
  summarise(INFLACION_HOGAR_ESCENARIO = sum(CONTRIBUCION, na.rm = TRUE), .groups = "drop")

# Unir la inflación real y el gasto de cada hogar, y calcular el ahorro EN EUROS a nivel de HOGAR
ahorro_hogar <- escenario_alimentos_hogar %>%
  left_join(
    resultado_hogar %>% select(hh_id, anio, INFLACION_HOGAR, GASTO, FACTOR, QUINTIL_label),
    by = c("hh_id", "anio")
  ) %>%
  filter(!is.na(QUINTIL_label)) %>%
  mutate(
    # positivo = lo que el hogar se ahorró gracias a la política (inflación
    # contrafactual mayor que la real, multiplicado por su propio gasto)
    ahorro_euros_hogar = (INFLACION_HOGAR_ESCENARIO - INFLACION_HOGAR) * GASTO
  )

# Promediar el ahorro dentro de cada quintil, ponderando por FACTOR
ahorro_hogar %>%
  filter(QUINTIL_label %in% c("Quintil 1", "Quintil 5"), anio %in% c("2023", "2024")) %>%
  group_by(anio, QUINTIL_label) %>%
  summarise(
    ahorro_euros = weighted.mean(ahorro_euros_hogar, w = FACTOR, na.rm = TRUE),
    .groups = "drop"
  )


# Porcentaje del ahorro
ahorro_hogar <- ahorro_hogar %>%
  mutate(
    ahorro_pct_gasto_hogar = INFLACION_HOGAR_ESCENARIO - INFLACION_HOGAR
  )

ahorro_hogar %>%
  filter(QUINTIL_label %in% c("Quintil 1", "Quintil 5"), anio %in% c("2023", "2024")) %>%
  group_by(anio, QUINTIL_label) %>%
  summarise(
    ahorro_pct_gasto = weighted.mean(ahorro_pct_gasto_hogar, w = FACTOR, na.rm = TRUE) * 100,
    .groups = "drop"
  )
