# ETAPA 6 — Gráficos de calidad, pobreza, educación e ingresos (Módulo 7).
# Entregable: PNG reutilizables en HTML y Word.
source(here::here('R','00_configuracion.R'))
h <- readRDS(here::here('resultados','05_tablas.rds'))

# Normalizar mayúsculas y espacios, conservando códigos no reconocidos.
canon_zona <- c('urbana' = 'Urbana', 'rural' = 'Rural')
canon_edu <- c(
  'sin instruccion' = 'Sin instruccion',
  'primaria'       = 'Primaria',
  'secundaria'     = 'Secundaria',
  'superior'       = 'Superior',
  'posgrado'       = 'Posgrado'
)

canonizar <- function(x, mapa) {
  key <- tolower(trimws(as.character(x)))
  propuesta <- unname(mapa[key])
  dplyr::coalesce(propuesta, as.character(x))
}

canonizar(c('URBANA', 'Rural ', '99'), canon_zona)
# Resultado: "Urbana" "Rural" "99"

# Educación: normalizar zona y sumar los grupos que ahora coinciden.
h$tablas$educacion_zona <- h$tablas$educacion_zona %>%
  mutate(
    zona = canonizar(zona, canon_zona),
    nivel_educacion = canonizar(nivel_educacion, canon_edu)
  ) %>%
  group_by(periodo_referencia, zona, nivel_educacion) %>%
  summarise(
    registros = sum(registros),
    .groups = 'drop'
  )

# Pobreza: normalizar zona y sumar los grupos que ahora coinciden.
h$tablas$pobreza_zona <- h$tablas$pobreza_zona %>%
  mutate(zona = canonizar(zona, canon_zona)) %>%
  group_by(periodo_referencia, zona, pobreza_didactica) %>%
  summarise(registros = sum(registros), .groups = 'drop')

archivo_figura <- function(nombre) here::here('resultados','figuras',paste0(nombre,'.png'))

# Gráfico 1: comparar indicadores Original/Propuesta.
g_ind <- ggplot(h$tablas$indicadores,
                aes(x=indicador,y=porcentaje,fill=escenario)) +
  scale_fill_manual(values = c(
    'Original' = '#91D6FF',
    'Propuesta' = '#11679C'
  )) +
  geom_col(position='dodge') +
  geom_text(aes(label=paste0(porcentaje,'%')),
            position=position_dodge(width=.9),vjust=-.3,size=3) +
  coord_cartesian(ylim=c(0,108)) +
  labs(title='Indicadores de calidad por escenario',x='Dimensión',y='Porcentaje') +
  theme_minimal()
ggsave(archivo_figura('06_indicadores'),g_ind,width=9,height=5,dpi=170)

# Gráfico 2: nivel educativo por zona. Los conteos NO se interpretan como tasas nacionales.
g_edu <- ggplot(h$tablas$educacion_zona,
                aes(x=nivel_educacion,y=registros,fill=zona)) +
  scale_fill_manual(values = c(
    'Urbana' = '#332EDB',
    'Rural' = '#A3A1FF',
    '99' = '#FF271F'
  )) +
  geom_col(position='dodge') +
  geom_text(
    aes(label = registros),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 3
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  facet_wrap(~periodo_referencia) +
  labs(title='Nivel educativo por zona y período',x='Nivel educativo',y='Registros') +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=32,hjust=1))
ggsave(archivo_figura('06_educacion'),g_edu,width=11,height=5,dpi=170)

# Gráfico 3: distribución de ingresos, excluyendo NA y negativos del histograma.
# coord_cartesian hace zoom para visualizar 0–3.000 USD sin alterar observaciones.
g_ing <- h$datos %>% filter(!is.na(ingreso_n),ingreso_n>=0) %>%
  ggplot(aes(x=ingreso_n)) +
  geom_histogram(
    bins = 45,
    fill = '#2FCC4A',
    color = 'white'
  ) +
  stat_bin(
    bins = 45,
    geom = 'text',
    aes(label = after_stat(ifelse(count > 0, count, ''))),
    vjust = -0.4,
    size = 2.5
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  facet_wrap(~periodo_referencia) +
  coord_cartesian(xlim=c(0,3000)) +
  labs(title='Distribución de ingresos mensuales',
       subtitle='Zoom visual: 0–3.000 USD; extremos no se borran',
       x='Ingreso personal (USD)',y='Registros') +
  theme_minimal()
ggsave(archivo_figura('06_ingresos'),g_ing,width=10,height=5,dpi=170)

# Gráfico 4: incidencias por regla; una fila puede registrar varias alertas.
g_alt <- ggplot(h$tablas$resumen_alertas,
                aes(x=reorder(regla_id,incidencias),y=incidencias)) +
  geom_col(fill = '#EB6521') + geom_text(
    aes(label = incidencias),
    hjust = -0.2,
    size = 3
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) + coord_flip() +
  labs(title='Alertas detectadas por regla',x='Regla',y='Incidencias') +
  theme_minimal()
ggsave(archivo_figura('06_alertas'),g_alt,width=9,height=7,dpi=170)

# Gráfico 5: pobreza didáctica por zona, sin pretensión de inferencia nacional.
# Calcular porcentajes dentro de cada zona y período.
datos_pobreza <- h$tablas$pobreza_zona %>%
  group_by(periodo_referencia, zona) %>%
  mutate(
    porcentaje = 100 * registros / sum(registros),
    etiqueta = paste0(
      formatC(porcentaje, format = 'f', digits = 1,
              decimal.mark = ','),
      '%'
    )
  ) %>%
  ungroup()

g_pob <- datos_pobreza %>%
  ggplot(aes(x = zona, y = registros, fill = pobreza_didactica)) +
  geom_col(position = 'fill') +
  geom_text(
    aes(
      label = etiqueta,
      color = pobreza_didactica
    ),
    position = position_fill(vjust = 0.5),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      'Pobre' = 'white',
      'No pobre' = 'black'
    ),
    guide = 'none'
  ) +  scale_fill_manual(values = c(
    'No pobre' = '#86BAFC',
    'Pobre' = '#175DBD'
  )) +
  facet_wrap(~periodo_referencia) +
  scale_y_continuous(
    labels = function(x) paste0(round(100 * x), '%')
  ) +
  labs(
    title = 'Composición según pobreza didáctica',
    subtitle = paste(
      'Umbral ficticio de USD 185 per cápita/mes;',
      'no es pobreza oficial'
    ),
    x = 'Zona',
    y = 'Porcentaje dentro de cada zona'
  ) +
  theme_minimal()
ggsave(archivo_figura('06_pobreza'),g_pob,width=9,height=5,dpi=170)
message('Gráficos guardados en resultados/figuras; abrir y revisar visualmente.')
