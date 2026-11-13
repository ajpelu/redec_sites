library(leaflet)
library(crosstalk)
library(DT)
library(htmltools)
library(tidyverse)

txt <- function(x) ifelse(is.na(x), "—", htmlEscape(as.character(x)))

publicaciones <- read_csv(here::here("data/publicaciones.csv"), show_col_types = FALSE)
sitios        <- read_csv(here::here("data/sitios.csv"), show_col_types = FALSE)

pubs <- publicaciones |>
  mutate(item = if_else(is.na(url), txt(titulo),
                        sprintf('<a href="%s" target="_blank">%s</a>', url, txt(coalesce(titulo, url)))
  )) |>
  group_by(id_site) |>
  summarise(pubs_html = paste0("<li>", item, "</li>", collapse = ""), .groups = "drop")


# Iconos 
iconos <- tibble(archivo = list.files(here::here("iconos"),
                                      pattern = "\\.png$",
                                      full.names = TRUE
)) |>
  mutate(especie = basename(archivo) |>
           str_remove("\\.png$") |>
           str_replace_all("_", " ") |>
           str_replace(" ssp ", " ssp. ") |>
           str_to_sentence())  

trees <- lapply(iconos$archivo, function(f) {
  makeIcon(
    iconUrl = f, iconWidth = 28, iconHeight = 28,
    iconAnchorX = 14, iconAnchorY = 28
  )
})
names(trees) <- iconos$especie
trees <- do.call(iconList, trees)

# Sitios: datos para popup, tabla y mapa
d <- sitios |>
  left_join(pubs, by = "id_site") |>
  mutate(
    masa = replace_na(masa, "Sin dato"),
    factor_principal = replace_na(factor_principal, "Sin dato"),
    seguimiento_activo = replace_na(seguimiento_activo, "Sin dato"),
    especie_principal = replace_na(especie_principal, "Sin dato"),
    # especie con icono propio; el resto -> "Otras"
    especie_icono = if_else(especie_principal %in% iconos$especie,
                            especie_principal, "Otras"
    ),
    contacto = txt(responsable),
    popup = paste0(
      "<b>", txt(nombre_sitio), "</b> (", id_site, ")",
      "<br><b>Responsable:</b> ", contacto,
      "<br><b>Especie principal:</b> <i>", txt(especie_principal), "</i>",
      "<br><b>Defoliación / mortalidad (sitio):</b> ", txt(defol_sitio), "% / ", txt(mort_sitio), "%",
      "<br><b>Masa:</b> ", txt(masa),
      "<br><b>Factor principal:</b> ", txt(factor_principal),
      "<br><b>Metodología:</b> ", txt(metodologia),
      "<br><b>Seguimiento:</b> ", txt(fecha_primer_muestreo), " &rarr; ", txt(fecha_ultimo_muestreo),
      " (activo: ", txt(seguimiento_activo), ")",
      if_else(is.na(pubs_html), "",
              paste0(
                "<br><b>Publicaciones:</b><ul style='margin:0;padding-left:16px'>",
                pubs_html, "</ul>"
              )
      )
    )
  ) |>
  select(
    id_site, nombre_sitio, responsable, especie_principal, defol_sitio, mort_sitio,
    masa, factor_principal, metodologia, escala, seguimiento_activo,
    fecha_primer_muestreo, fecha_ultimo_muestreo, n_publicaciones,
    lon, lat, especie_icono, popup
  )

# Avisar de especies que caen en "Otras" por no tener icono
sin_icono <- setdiff(unique(d$especie_principal), iconos$especie)
if (length(sin_icono) > 0) {
  message(
    "Sin icono (se muestran como Otras): ",
    paste(sin_icono, collapse = ", ")
  )
}

# Datos compartidos (después de calcular todo) ---------------------------
sd <- SharedData$new(d, key = ~id_site, group = "redec")

# Leyenda con las imágenes de las especies presentes ----------------------
en_mapa <- filter(iconos, especie %in% d$especie_icono)

items <- paste0(
  "<div style='white-space:nowrap;display:flex;align-items:center'>",
  "<img src='", sapply(en_mapa$archivo, knitr::image_uri),
  "' width='18' height='18' style='margin-right:4px'>",
  "<span style='font-style:italic'>", en_mapa$especie, "</span></div>",
  collapse = "")

leyenda <- paste0(
  "<div style='font-size:12px;line-height:1.3'>",
  "<b>Especie principal</b>",
  "<div style='display:grid;grid-template-columns:auto auto;",
  "column-gap:12px;row-gap:3px;margin-top:4px'>",
  items, "</div></div>")

# Mapa ------------------------------------------------------------------
mapa <- leaflet(sd) |>
  # addProviderTiles(providers$OpenStreetMap, group = "Mapa") |>
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gris") |>
  addProviderTiles(providers$Esri.WorldTopoMap,   group = "Topográfico") |>
  addProviderTiles(providers$Esri.WorldImagery,   group = "Satélite") |>
  fitBounds(-9.5, 35.8, 4.4, 43.9) |> # península
  addMarkers(
    lng = ~lon, lat = ~lat,
    icon = ~ trees[especie_icono],
    popup = ~popup, label = ~nombre_sitio
  ) |>
  addLayersControl(baseGroups = c("Gris", "Topográfico", "Satélite"),
                   options = layersControlOptions(collapsed = TRUE)) |>
  addControl(html = leyenda, position = "bottomright")

# --- filtros --------------------------------------------------------
filtros <- list(
  filter_select("f_especie", "Especie principal", sd, ~especie_principal),
  filter_select("f_masa", "Tipo de masa", sd, ~masa),
  filter_select("f_factor", "Factor principal", sd, ~factor_principal),
  filter_checkbox("f_seg", "Seguimiento activo", sd, ~seguimiento_activo, inline = TRUE),
  filter_slider("f_defol", "% defoliación (sitio)", sd, ~defol_sitio, step = 5, width = "100%"),
  filter_slider("f_mort", "% mortalidad (sitio)", sd, ~mort_sitio, step = 5, width = "100%")
)

# --- tabla ----------------------------------------------------------
ocultas <- which(names(d) %in% c("lon", "lat", "especie_icono", "popup")) - 1   # índices desde 0

# Columnas que se exportan: todas menos el popup y el icono (índices desde 0)
exportar <- which(!names(d) %in% c("popup", "especie_icono")) - 1

botones <- list(
  list(extend = "csv",   text = "CSV",   filename = "sitios_redec",
       exportOptions = list(columns = exportar)),
  list(extend = "excel", text = "Excel", filename = "sitios_redec",
       exportOptions = list(columns = exportar))
)


tabla <- datatable(
  sd, rownames = FALSE, extensions = "Buttons",
  colnames = c("Código", "Sitio", "Responsable", "Especie principal", "% defol.", "% mort.",
               "Masa", "Factor principal", "Metodología", "Escala", "Seguimiento activo",
               "Primer muestreo", "Último muestreo", "Nº publ.", "lon", "lat", "especie_icono", "popup"),
  options = list(
    dom = "Bfrtip", buttons = botones, pageLength = 10, scrollX = TRUE,
    language = list(search = "Buscar:", info = "_TOTAL_ sitios",
                    infoFiltered = "(de _MAX_)", infoEmpty = "Ningún sitio",
                    zeroRecords = "Ningún sitio cumple los filtros",
                    paginate = list(previous = "Anterior", `next` = "Siguiente")),
    columnDefs = list(list(visible = FALSE, targets = ocultas))
  )
)

