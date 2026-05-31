# TRABAJO-FINAL---DATA-SCIENCE
Repositorio del trabajo final de Data Science

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# SALUD MENTAL Y EFECTO DEL INMIGRANTE SANO EN ESPAÑA (ADAPTADO ENSE 2017)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

# Carga de paquetes y preparación de los datos
library(tidyverse)  
library(haven)  
library(gtsummary)  

load("ENSE2017.RData")

df_analisis <- ENSE2017 %>%
  mutate(across(c(E1_1, E3, G25a_20, G25c_20, G25a_21, G25c_21, SEXOa, EDADa, NIVEST, FACTORADULTO), 
                ~ as.numeric(as.character(.))))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 1. RECODIFICACIÓN: PERFIL MIGRATORIO (Variable Independiente)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_analisis <- df_analisis %>%
  # Limpiamos los valores fuera de rango en la encuesta
  mutate(Anos_Res = ifelse(E3 > 97, NA, E3)) %>%
  # Creación de la variable
  mutate(Perfil_Migratorio = case_when(
    E1_1 == 1 ~ "1. Nativo",
    E1_1 == 2 & Anos_Res <= 5 ~ "2. Inmigrante Reciente (<=5 años)",
    E1_1 == 2 & Anos_Res > 5 & Anos_Res <= 15 ~ "3. Inmigrante Medio (6-15 años)",
    E1_1 == 2 & Anos_Res > 15 ~ "4. Inmigrante Asentado (>15 años)",
    TRUE ~ NA_character_
  )) %>%
  mutate(Perfil_Migratorio = as.factor(Perfil_Migratorio))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 2. RECODIFICACIÓN: SALUD MENTAL CLÍNICA (Variables Dependientes)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_analisis <- df_analisis %>%
  # Diagnóstico Clínico de Depresión
  mutate(depresion = case_when(
    G25a_20 == 1 & G25c_20 == 1 ~ 1,
    G25a_20 == 2 | G25a_20 == 6 ~ 0,
    G25a_20 == 8 | G25a_20 == 9 | is.na(G25a_20) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  # Diagnóstico Clínico de Ansiedad Crónica
  mutate(ansiedad = case_when(
    G25a_21 == 1 & G25c_21 == 1 ~ 1,
    G25a_21 == 2 | G25a_21 == 6 ~ 0,
    G25a_21 == 8 | G25a_21 == 9 | is.na(G25a_21) ~ NA_real_,
    TRUE ~ 0
  ))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 3. RECODIFICACIÓN: VARIABLES DE CONTROL (Se mantienen igual)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_analisis <- df_analisis %>%
  # Variable Sexo
  mutate(Sexo = case_when(
    SEXOa == 1 ~ "Hombre",
    SEXOa == 2 ~ "Mujer",
    TRUE ~ NA_character_
  )) %>%
  mutate(Sexo = as.factor(Sexo)) %>%
  
  # Variable Edad
  mutate(Edad = as.numeric(EDADa)) %>%
  
  # Variable Nivel de Estudios
  mutate(Nivel_Estudios = case_when(
    NIVEST %in% c(1, 2, 3, 4) ~ "Bajos (Primarios o menos)",
    NIVEST %in% c(5, 6, 7, 8) ~ "Medios (Secundaria/FP)",
    NIVEST == 9 ~ "Altos (Universitarios)",
    TRUE ~ NA_character_
  )) %>%
  mutate(Nivel_Estudios = factor(Nivel_Estudios, 
                                 levels = c("Altos (Universitarios)", 
                                            "Medios (Secundaria/FP)", 
                                            "Bajos (Primarios o menos)")))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 4. PREPARACIÓN DE LA BASE (Filtros y Pesos)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_regresion <- df_analisis %>%
  filter(!is.na(depresion), !is.na(ansiedad), !is.na(Perfil_Migratorio), 
         !is.na(Sexo), !is.na(Edad), !is.na(Nivel_Estudios), !is.na(FACTORADULTO)) %>%
  # Normalización del peso
  mutate(Peso_Normalizado = FACTORADULTO / mean(FACTORADULTO, na.rm = TRUE))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# TABLA 1: ANÁLISIS DESCRIPTIVO Y TABLA CRUZADA
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

tabla_1_viewer <- df_regresion %>%
  # Seleccionamos solo las variables que van a cruzar en la Tabla 1
  select(Perfil_Migratorio, depresion, ansiedad) %>%
  # Convertimos los 0 y 1 en palabras ("Sí" / "No")
  mutate(
    depresion = factor(depresion, levels = c(1, 0), labels = c("Sí", "No")),
    ansiedad = factor(ansiedad, levels = c(1, 0), labels = c("Sí", "No"))
  ) %>%
  # Tabla cruzada
  tbl_summary(
    by = Perfil_Migratorio, 
    label = list(
      depresion ~ "Diagnóstico Clínico de Depresión",
      ansiedad ~ "Diagnóstico Clínico de Ansiedad Crónica"
    ),
    missing = "no",
    digits = all_categorical() ~ c(0, 1)
  ) %>%
  bold_labels() %>%
  modify_caption("**Tabla 1. Diagnostico de Salud Mental por Perfil Migratorio**")

tabla_1_viewer

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# VISUALIZACIÓN DE LOS DATOS: GRÁFICO DE BARRAS Y DE TENDENCIA
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

# Preparación de los datos
datos_grafico_bruto <- df_regresion %>%
  group_by(Perfil_Migratorio) %>%
  summarise(
    Depresión = mean(depresion, na.rm = TRUE) * 100,
    Ansiedad = mean(ansiedad, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(
    cols = c(Depresión, Ansiedad), 
    names_to = "Patologia", 
    values_to = "Porcentaje"
  )

# Crear el Gráfico de Barras
grafico_barras_bruto <- ggplot(datos_grafico_bruto, aes(x = Perfil_Migratorio, y = Porcentaje, fill = Patologia)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Etiquetas numéricas exactas encima de cada barra
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            fontface = "bold", 
            size = 4) +
  scale_fill_manual(values = c("Depresión" = "#2C3E50", "Ansiedad" = "#E67E22")) +
  scale_y_continuous(limits = c(0, max(datos_grafico_bruto$Porcentaje) + 2)) +
  labs(
    title = "Salud Mental por Tiempo de Residencia",
    x = NULL,
    y = "Porcentaje de casos (%)",
    fill = "Diagnóstico Clínico"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank())

print(grafico_barras_bruto)

# Gráfico de Líneas
grafico_lineas_bruto <- ggplot(datos_grafico_bruto, aes(x = Perfil_Migratorio, y = Porcentaje, color = Patologia, group = Patologia)) +
  # Position_dodge directamente como argumento interno
  geom_line(size = 1.2, position = position_dodge(width = 0.15)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.15)) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_dodge(width = 0.50), 
            vjust = -1.2, 
            fontface = "bold", 
            size = 4,
            show.legend = FALSE) +
  scale_color_manual(values = c("Depresión" = "#2C3E50", "Ansiedad" = "#E67E22")) +
  scale_y_continuous(limits = c(0, max(datos_grafico_bruto$Porcentaje) + 2)) +
  labs(
    title = "Salud Mental por Tiempo de Residencia",
    x = NULL,
    y = "Porcentaje de casos (%)",
    color = "Diagnóstico Clínico"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
    panel.grid.minor = element_blank())

print(grafico_lineas_bruto)

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 5. ESTIMACIÓN DE MODELOS LOGÍSTICOS MULTIVARIANTES
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

modelo_depresion <- glm(depresion ~ Perfil_Migratorio + Sexo + Edad + Nivel_Estudios,
                        family = quasibinomial(link = "logit"),
                        weights = Peso_Normalizado, 
                        data = df_regresion)

modelo_ansiedad <- glm(ansiedad ~ Perfil_Migratorio + Sexo + Edad + Nivel_Estudios,
                       family = quasibinomial(link = "logit"),
                       weights = Peso_Normalizado, 
                       data = df_regresion)

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# TABLA 2: TABLA DOBLE DE REGRESIONES 
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

# Subtabla Depresión
tabla_depresion <- tbl_regression(
  modelo_depresion, exponentiate = TRUE, 
  label = list(Perfil_Migratorio ~ "Perfil migratorio (Ref: Nativo)", Sexo ~ "Sexo (Ref: Hombre)", Edad ~ "Edad (por año)", Nivel_Estudios ~ "Estudios (Ref: Universitarios)")
) %>% bold_labels() %>% italicize_levels()

# Subtabla Ansiedad
tabla_ansiedad <- tbl_regression(
  modelo_ansiedad, exponentiate = TRUE, 
  label = list(Perfil_Migratorio ~ "Perfil migratorio (Ref: Nativo)", Sexo ~ "Sexo (Ref: Hombre)", Edad ~ "Edad (por año)", Nivel_Estudios ~ "Estudios (Ref: Universitarios)")
) %>% bold_labels() %>% italicize_levels()

# Fusión de las regresiones
reporte_regresiones <- tbl_merge(
  tbls = list(tabla_depresion, tabla_ansiedad),
  tab_spanner = c("**Modelo I: Depresión**", "**Modelo II: Ansiedad Crónica**")
)

reporte_regresiones

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# SALUD MENTAL Y EFECTO DEL INMIGRANTE SANO EN ESPAÑA (ESdE 2023)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

# Carga de paquetes y preparación de los datos
library(tidyverse)  
library(haven)  
library(gtsummary)

load("ESdE2023.RData")

df_analisis <- df_micro %>%
  mutate(across(c(A1a, A3, C5a_21, C5c_21, C5a_22, C5c_22, SEXOa, EDADa, NIVEST, FACTORADULTO), 
                ~ as.numeric(as.character(.))))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 1. RECODIFICACIÓN: PERFIL MIGRATORIO (Variable Independiente)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_analisis <- df_analisis %>%
  # Limpieza de años de residencia fuera de rango en la encuesta
  mutate(Anos_Res = ifelse(A3 > 95, NA, A3)) %>%
  # Creación de la variable
  mutate(Perfil_Migratorio = case_when(
    A1a == 1 ~ "1. Nativo",
    A1a == 2 & Anos_Res <= 5 ~ "2. Inmigrante Reciente (<=5 años)",
    A1a == 2 & Anos_Res > 5 & Anos_Res <= 15 ~ "3. Inmigrante Medio (6-15 años)",
    A1a == 2 & Anos_Res > 15 ~ "4. Inmigrante Asentado (>15 años)",
    TRUE ~ NA_character_
  )) %>%
  mutate(Perfil_Migratorio = as.factor(Perfil_Migratorio))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 2. RECODIFICACIÓN: SALUD MENTAL CLÍNICA (Variables Dependientes)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_analisis <- df_analisis %>%
  # Diagnóstico Clínico de Depresión
  mutate(depresion = case_when(
    C5a_21 == 1 & C5c_21 == 1 ~ 1,
    C5a_21 == 6 ~ 0,
    C5a_21 == 9 | is.na(C5a_21) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  # Diagnóstico Clínico de Ansiedad Crónica
  mutate(ansiedad = case_when(
    C5a_22 == 1 & C5c_22 == 1 ~ 1,
    C5a_22 == 6 ~ 0,
    C5a_22 == 9 | is.na(C5a_22) ~ NA_real_,
    TRUE ~ 0
  ))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 3. RECODIFICACIÓN: VARIABLES DE CONTROL
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_analisis <- df_analisis %>%
  # Variable Sexo
  mutate(Sexo = case_when(
    SEXOa == 1 ~ "Hombre",
    SEXOa == 2 ~ "Mujer",
    TRUE ~ NA_character_
  )) %>%
  mutate(Sexo = as.factor(Sexo)) %>%
  
  # Variable Edad
  mutate(Edad = as.numeric(EDADa)) %>%
  
  # Variable Nivel de Estudios
  mutate(Nivel_Estudios = case_when(
    NIVEST %in% c(1, 2, 3, 4) ~ "Bajos (Primarios o menos)",
    NIVEST %in% c(5, 6, 7, 8) ~ "Medios (Secundaria/FP)",
    NIVEST == 9 ~ "Altos (Universitarios)",
    TRUE ~ NA_character_
  )) %>%
  mutate(Nivel_Estudios = factor(Nivel_Estudios, 
                                 levels = c("Altos (Universitarios)", 
                                            "Medios (Secundaria/FP)", 
                                            "Bajos (Primarios o menos)")))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 4. PREPARACIÓN DE LA BASE (Filtros y Pesos)
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

df_regresion <- df_analisis %>%
  filter(!is.na(depresion), !is.na(ansiedad), !is.na(Perfil_Migratorio), 
         !is.na(Sexo), !is.na(Edad), !is.na(Nivel_Estudios), !is.na(FACTORADULTO)) %>%
  # Normalización del peso
  mutate(Peso_Normalizado = FACTORADULTO / mean(FACTORADULTO, na.rm = TRUE))

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# TABLA 1: ANÁLISIS DESCRIPTIVO Y TABLA CRUZADA
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

tabla_1_viewer <- df_regresion %>%
  # Seleccionamos solo las variables que van a cruzar en la Tabla 1
  select(Perfil_Migratorio, depresion, ansiedad) %>%
  # Convertimos los 0 y 1 en palabras ("Sí" / "No")
  mutate(
    depresion = factor(depresion, levels = c(1, 0), labels = c("Sí", "No")),
    ansiedad = factor(ansiedad, levels = c(1, 0), labels = c("Sí", "No"))
  ) %>%
  # Tabla cruzada
  tbl_summary(
    by = Perfil_Migratorio, 
    label = list(
      depresion ~ "Diagnóstico Clínico de Depresión",
      ansiedad ~ "Diagnóstico Clínico de Ansiedad Crónica"
    ),
    missing = "no",
    digits = all_categorical() ~ c(0, 1)
  ) %>%
  bold_labels() %>%
  modify_caption("**Tabla 1. Diagnostico de Salud Mental por Perfil Migratorio**")

tabla_1_viewer

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# VISUALIZACIÓN DE LOS DATOS: GRÁFICO DE BARRAS Y DE TENDENCIA
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

# Preparación de los datos
datos_grafico_bruto <- df_regresion %>%
  group_by(Perfil_Migratorio) %>%
  summarise(
    Depresión = mean(depresion, na.rm = TRUE) * 100,
    Ansiedad = mean(ansiedad, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(
    cols = c(Depresión, Ansiedad), 
    names_to = "Patologia", 
    values_to = "Porcentaje"
  )

# Crear el Gráfico de Barras
grafico_barras_bruto <- ggplot(datos_grafico_bruto, aes(x = Perfil_Migratorio, y = Porcentaje, fill = Patologia)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Etiquetas numéricas exactas encima de cada barra
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            fontface = "bold", 
            size = 4) +
  scale_fill_manual(values = c("Depresión" = "#2C3E50", "Ansiedad" = "#E67E22")) +
  scale_y_continuous(limits = c(0, max(datos_grafico_bruto$Porcentaje) + 2)) +
  labs(
    title = "Salud Mental por Tiempo de Residencia",
    x = NULL,
    y = "Porcentaje de casos (%)",
    fill = "Diagnóstico Clínico"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank())

print(grafico_barras_bruto)

# Gráfico de Líneas
grafico_lineas_bruto <- ggplot(datos_grafico_bruto, aes(x = Perfil_Migratorio, y = Porcentaje, color = Patologia, group = Patologia)) +
  # Position_dodge directamente como argumento interno
  geom_line(size = 1.2, position = position_dodge(width = 0.15)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.15)) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_dodge(width = 0.50), 
            vjust = -1.2, 
            fontface = "bold", 
            size = 4,
            show.legend = FALSE) +
  scale_color_manual(values = c("Depresión" = "#2C3E50", "Ansiedad" = "#E67E22")) +
  scale_y_continuous(limits = c(0, max(datos_grafico_bruto$Porcentaje) + 2)) +
  labs(
    title = "Salud Mental por Tiempo de Residencia",
    x = NULL,
    y = "Porcentaje de casos (%)",
    color = "Diagnóstico Clínico"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
    panel.grid.minor = element_blank())

print(grafico_lineas_bruto)

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# 5. ESTIMACIÓN DE MODELOS LOGÍSTICOS MULTIVARIANTES
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

modelo_depresion <- glm(depresion ~ Perfil_Migratorio + Sexo + Edad + Nivel_Estudios,
                        family = quasibinomial(link = "logit"),
                        weights = Peso_Normalizado, 
                        data = df_regresion)

modelo_ansiedad <- glm(ansiedad ~ Perfil_Migratorio + Sexo + Edad + Nivel_Estudios,
                       family = quasibinomial(link = "logit"),
                       weights = Peso_Normalizado, 
                       data = df_regresion)

# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
# TABLA 2: TABLA DOBLE DE REGRESIONES 
# //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//

# Subtabla Depresión
tabla_depresion <- tbl_regression(
  modelo_depresion, exponentiate = TRUE, 
  label = list(Perfil_Migratorio ~ "Perfil migratorio (Ref: Nativo)", Sexo ~ "Sexo (Ref: Hombre)", Edad ~ "Edad (por año)", Nivel_Estudios ~ "Estudios (Ref: Universitarios)")
) %>% bold_labels() %>% italicize_levels()

# Subtabla Ansiedad
tabla_ansiedad <- tbl_regression(
  modelo_ansiedad, exponentiate = TRUE, 
  label = list(Perfil_Migratorio ~ "Perfil migratorio (Ref: Nativo)", Sexo ~ "Sexo (Ref: Hombre)", Edad ~ "Edad (por año)", Nivel_Estudios ~ "Estudios (Ref: Universitarios)")
) %>% bold_labels() %>% italicize_levels()

# Fusión de las regresiones
reporte_regresiones <- tbl_merge(
  tbls = list(tabla_depresion, tabla_ansiedad),
  tab_spanner = c("**Modelo I: Depresión**", "**Modelo II: Ansiedad Crónica**")
)

reporte_regresiones
