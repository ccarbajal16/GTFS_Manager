# ============================================
# GTFS Manager - Aplicación Shiny
# Curso-Taller GTFS
# ============================================

library(shiny)
library(bslib)
library(DT)
library(leaflet)
library(tidytransit)
library(gtfstools)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)
library(zip)
library(osmdata)

# ============================================
# FUNCIONES AUXILIARES
# ============================================

seconds_to_time <- function(seconds) {
  sprintf("%02d:%02d:%02d",
    as.integer(seconds %/% 3600),
    as.integer((seconds %% 3600) %/% 60),
    as.integer(seconds %% 60))
}

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  a <- sin(dLat/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(dLon/2)^2
  R * 2 * atan2(sqrt(a), sqrt(1 - a))
}

generate_trips_stop_times <- function(stops_df, route_id, service_id,
                                      direction_id, direction_name,
                                      start_hour, end_hour,
                                      freq_min, time_between_sec) {
  stop_ids <- if (direction_id == 0) stops_df$stop_id else rev(stops_df$stop_id)
  headsign <- if (direction_id == 0) tail(stops_df$stop_name, 1) else stops_df$stop_name[1]
  start_secs <- seq(start_hour * 3600, end_hour * 3600, by = freq_min * 60)

  trips_out <- vector("list", length(start_secs))
  st_out    <- list()

  for (i in seq_along(start_secs)) {
    tid <- sprintf("T%s_%s_%s_%03d", route_id, service_id, direction_name, i)
    trips_out[[i]] <- tibble(trip_id = tid, route_id, service_id,
                              trip_headsign = headsign, direction_id)
    cur <- start_secs[i]
    for (j in seq_along(stop_ids)) {
      st_out[[length(st_out) + 1]] <- tibble(
        trip_id = tid,
        arrival_time   = seconds_to_time(cur),
        departure_time = seconds_to_time(cur),
        stop_id        = stop_ids[j],
        stop_sequence  = j,
        pickup_type    = if (j == length(stop_ids)) 1L else 0L,
        drop_off_type  = if (j == 1L) 1L else 0L
      )
      cur <- cur + time_between_sec
    }
  }
  list(trips = bind_rows(trips_out), stop_times = bind_rows(st_out))
}

build_and_zip_gtfs <- function(tables, output_zip) {
  tmp <- tempfile()
  dir.create(tmp, recursive = TRUE)
  for (nm in names(tables))
    write_csv(tables[[nm]], file.path(tmp, paste0(nm, ".txt")), na = "")
  files <- list.files(tmp, "\\.txt$", full.names = TRUE)
  zip::zip(output_zip, files, mode = "cherry-pick")
  output_zip
}

generate_html_report <- function(gtfs) {
  agency_name <- tryCatch(gtfs$agency$agency_name[1], error = function(e) "Feed GTFS")

  route_rows <- if (!is.null(gtfs$routes) && nrow(gtfs$routes) > 0) {
    paste(apply(gtfs$routes, 1, function(r) {
      n <- if (!is.null(gtfs$trips)) sum(gtfs$trips$route_id == r["route_id"]) else 0
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%d</td></tr>",
              r["route_id"], r["route_short_name"], r["route_long_name"], n)
    }), collapse = "\n")
  } else ""

  sprintf(
'<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <title>Reporte GTFS - %s</title>
  <style>
    body{font-family:Arial,sans-serif;margin:40px;background:#f5f5f5}
    .box{background:#fff;padding:30px;border-radius:10px;box-shadow:0 2px 8px rgba(0,0,0,.1);max-width:900px;margin:auto}
    h1{color:#1a73e8;margin-top:0} h2{color:#333;border-bottom:2px solid #1a73e8;padding-bottom:6px}
    .metrics{display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:14px;margin:20px 0}
    .m{background:#e8f0fe;border-radius:8px;padding:16px;text-align:center}
    .mv{font-size:2rem;font-weight:700;color:#1a73e8} .ml{font-size:.8rem;color:#555;margin-top:4px}
    table{border-collapse:collapse;width:100%%;margin:14px 0}
    th{background:#1a73e8;color:#fff;padding:9px 12px;text-align:left}
    td{border:1px solid #ddd;padding:8px 12px}
    tr:nth-child(even){background:#f0f4ff}
    .footer{margin-top:28px;color:#aaa;font-size:.8rem;text-align:center}
  </style>
</head>
<body>
<div class="box">
  <h1>Reporte Feed GTFS</h1>
  <p><strong>Agencia:</strong> %s &nbsp;&bull;&nbsp; <strong>Generado:</strong> %s</p>
  <h2>Resumen</h2>
  <div class="metrics">
    <div class="m"><div class="mv">%d</div><div class="ml">Rutas</div></div>
    <div class="m"><div class="mv">%d</div><div class="ml">Paradas</div></div>
    <div class="m"><div class="mv">%d</div><div class="ml">Viajes</div></div>
    <div class="m"><div class="mv">%s</div><div class="ml">Stop times</div></div>
  </div>
  <h2>Rutas</h2>
  <table><tr><th>ID</th><th>Nombre corto</th><th>Nombre largo</th><th>Viajes</th></tr>
  %s
  </table>
  <div class="footer">Generado con GTFS Manager &mdash; Curso-Taller GTFS </div>
</div>
</body>
</html>',
    agency_name, agency_name,
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    nrow(gtfs$routes), nrow(gtfs$stops), nrow(gtfs$trips),
    format(nrow(gtfs$stop_times), big.mark = ","),
    route_rows
  )
}

# Paradas de ejemplo (zona Trujillo)
DEFAULT_STOPS <- tibble(
  stop_id   = sprintf("S%03d", 1:6),
  stop_name = c("Plaza de Armas", "Mercado Central", "Hospital Regional",
                "Estadio Municipal", "Terminal Terrestre", "Universidad"),
  stop_lat  = c(-8.1116, -8.1132, -8.1089, -8.1042, -8.1012, -8.0985),
  stop_lon  = c(-79.0289, -79.0276, -79.0254, -79.0231, -79.0208, -79.0186),
  stop_code = sprintf("PA%03d", 1:6)
)

OSM_MIRRORS <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter",
  "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
  "https://overpass.openstreetmap.ru/api/interpreter"
)

fetch_osm_stops <- function(bbox, mirrors = OSM_MIRRORS, max_attempts = 3, base_wait = 30) {
  for (mirror in mirrors) {
    set_overpass_url(mirror)
    query <- opq(bbox = bbox, timeout = 90) |>
      add_osm_feature(key = "highway", value = "bus_stop")
    for (attempt in seq_len(max_attempts)) {
      result <- tryCatch(osmdata_sf(query), error = function(e) {
        if (grepl("429|Too Many Requests", conditionMessage(e), ignore.case = TRUE))
          Sys.sleep(base_wait * 2^(attempt - 1))
        NULL
      })
      if (!is.null(result)) {
        pts <- result$osm_points
        if (!is.null(pts) && nrow(pts) > 0) {
          pts <- pts[!is.na(pts$highway) & pts$highway == "bus_stop", ]
          coords <- st_coordinates(pts)
          keep <- intersect(
            c("osm_id", "name", "ref", "operator", "network", "shelter", "bench", "wheelchair"),
            names(pts)
          )
          pts <- pts[, c(keep, "geometry")]
          pts$lon <- coords[, "X"]
          pts$lat <- coords[, "Y"]
          return(pts)
        }
        return(st_sf(geometry = st_sfc(crs = 4326)))
      }
    }
  }
  stop("All Overpass mirrors exhausted. Try again later.")
}

fetch_osm_routes <- function(bbox, route_type = "bus", mirrors = OSM_MIRRORS,
                             max_attempts = 3, base_wait = 30) {
  for (mirror in mirrors) {
    set_overpass_url(mirror)
    query <- opq(bbox = bbox, timeout = 90) |>
      add_osm_feature(key = "route", value = route_type)
    for (attempt in seq_len(max_attempts)) {
      result <- tryCatch(osmdata_sf(query), error = function(e) {
        if (grepl("429|Too Many Requests", conditionMessage(e), ignore.case = TRUE))
          Sys.sleep(base_wait * 2^(attempt - 1))
        NULL
      })
      if (!is.null(result)) {
        lines <- result$osm_multilines
        if (!is.null(lines) && nrow(lines) > 0) {
          keep <- intersect(
            c("osm_id", "name", "ref", "operator", "network", "from", "to", "colour", "route", "wheelchair"),
            names(lines)
          )
          lines <- lines[, c(keep, "geometry")]
          return(lines)
        }
        return(st_sf(geometry = st_sfc(crs = 4326)))
      }
    }
  }
  stop("All Overpass mirrors exhausted. Try again later.")
}

# ============================================
# UI
# ============================================

ui <- page_navbar(
  title = tags$span(icon("bus-simple"), " GTFS Manager"),
  theme = bs_theme(bootswatch = "flatly", primary = "#1a73e8", font_scale = 0.95),
  bg = "#1a73e8",
  inverse = TRUE,

  tags$head(tags$style(HTML("
    .sidebar { border-right: 1px solid #dee2e6; }
    .value-box .value-box-value { font-size: 1.8rem; }
    .nav-tabs .nav-link { font-size: .9rem; }
    .card-header { font-weight: 600; }
    .placeholder-msg { color: #6c757d; text-align: center; padding: 60px 20px; }
    .placeholder-msg .fa { font-size: 3rem; display: block; margin-bottom: 12px; opacity: .4; }
  "))),

  # ─────────────────────────────────────────────────────────────
  # TAB 1: ANÁLISIS
  # ─────────────────────────────────────────────────────────────
  nav_panel(
    "Análisis GTFS", icon = icon("chart-bar"),

    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = "#f8f9fa",
        tags$p(class = "fw-bold text-primary mb-1 small", "CARGAR FEED"),
        fileInput("gtfs_zip", NULL, accept = ".zip",
                  buttonLabel = "Seleccionar ZIP",
                  placeholder = "Ningún archivo seleccionado"),
        actionButton("analizar_btn", "Analizar Feed",
                     icon = icon("magnifying-glass"),
                     class = "btn-primary w-100 mb-3"),
        hr(),
        tags$p(class = "fw-bold text-muted mb-1 small", "DESCARGAS"),
        downloadButton("dl_freq_chart", " Gráfico Frecuencia",
                       class = "btn-outline-primary btn-sm w-100 mb-1"),
        downloadButton("dl_map_png",    " Mapa Paradas (PNG)",
                       class = "btn-outline-primary btn-sm w-100 mb-1"),
        downloadButton("dl_stops_gpkg", " Paradas GeoPackage",
                       class = "btn-outline-success btn-sm w-100 mb-1"),
        downloadButton("dl_report_an",  " Reporte HTML",
                       class = "btn-outline-info btn-sm w-100")
      ),
      uiOutput("analisis_ui")
    )
  ),

  # ─────────────────────────────────────────────────────────────
  # TAB 2: CREAR GTFS
  # ─────────────────────────────────────────────────────────────
  nav_panel(
    "Crear GTFS", icon = icon("pen-to-square"),

    layout_sidebar(
      sidebar = sidebar(
        width = 250, bg = "#f8f9fa",
        tags$p(class = "fw-bold text-primary mb-1 small", "ESTADO DEL FEED"),
        uiOutput("creator_status"),
        hr(),
        actionButton("generar_gtfs", "Generar GTFS",
                     icon = icon("gears"),
                     class = "btn-success w-100 mb-2"),
        uiOutput("dl_gtfs_ui"),
        uiOutput("dl_individual_ui")
      ),

      navset_card_tab(

        # ── Agencia ──────────────────────────────────────────
        nav_panel("Agencia", icon = icon("building"),
          card(card_header("Información de la Agencia"), card_body(
            fluidRow(
              column(6, textInput("ag_id",   "ID Agencia *",   "AG001")),
              column(6, textInput("ag_name", "Nombre *", "Empresa de Transporte"))
            ),
            fluidRow(
              column(6, textInput("ag_url", "URL", "http://transporte.pe")),
              column(6, selectInput("ag_tz", "Zona Horaria *",
                choices = c("America/Lima", "America/Bogota", "America/Santiago",
                            "America/Guayaquil", "America/La_Paz"),
                selected = "America/Lima"))
            ),
            fluidRow(
              column(4, textInput("ag_lang", "Idioma", "es")),
              column(4, selectInput("ag_currency", "Moneda",
                choices = c("PEN","COP","CLP","USD"), selected = "PEN")),
              column(4, textInput("ag_phone", "Teléfono (opt.)", ""))
            )
          ))
        ),

        # ── Paradas ──────────────────────────────────────────
        nav_panel("Paradas", icon = icon("map-pin"),
          card(card_header("Paradas del Sistema"), card_body(
            fluidRow(
              column(7,
                fileInput("stops_csv", "Cargar desde CSV",
                          accept = ".csv",
                          buttonLabel = "Seleccionar CSV",
                          placeholder = "O edite la tabla directamente")),
              column(5, div(class = "mt-4 pt-2",
                downloadButton("dl_stops_tpl", "Plantilla CSV",
                               class = "btn-outline-secondary btn-sm w-100")))
            ),
            DTOutput("stops_table"),
            div(class = "mt-2 d-flex gap-2",
              actionButton("add_stop", "Agregar parada",
                           icon = icon("plus"), class = "btn-sm btn-outline-primary"),
              actionButton("del_stop", "Eliminar seleccionada",
                           icon = icon("trash"), class = "btn-sm btn-outline-danger")
            )
          ))
        ),

        # ── Rutas ────────────────────────────────────────────
        nav_panel("Rutas", icon = icon("route"),
          card(card_header("Configuración de la Ruta"), card_body(
            fluidRow(
              column(3, textInput("rt_id",    "ID Ruta *",     "R010")),
              column(3, textInput("rt_short", "Nombre corto *","10")),
              column(6, textInput("rt_long",  "Nombre largo *","Plaza de Armas - Universidad"))
            ),
            fluidRow(
              column(3, selectInput("rt_type", "Tipo *",
                choices = c("Bus (3)"=3,"Metro (1)"=1,"Tren (2)"=2,
                            "Ferry (4)"=4,"Cable (5)"=5),
                selected = 3)),
              column(3, textInput("rt_color",      "Color ruta (hex)",  "0066CC")),
              column(3, textInput("rt_text_color", "Color texto (hex)", "FFFFFF")),
              column(3, textInput("rt_url", "URL ruta (opt.)", ""))
            ),
            fluidRow(
              column(6, textInput("rt_head_out", "Letrero destino (ida)",    "Universidad")),
              column(6, textInput("rt_head_in",  "Letrero destino (vuelta)", "Plaza de Armas"))
            )
          ))
        ),

        # ── Calendario ───────────────────────────────────────
        nav_panel("Calendario", icon = icon("calendar-days"),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Servicio Días Laborables"), card_body(
              checkboxGroupInput("wd_days", "Días activos",
                choices = c("Lun"=1,"Mar"=2,"Mié"=3,"Jue"=4,"Vie"=5,"Sáb"=6,"Dom"=7),
                selected = c(1,2,3,4,5), inline = TRUE),
              fluidRow(
                column(6, numericInput("wd_start", "Hora inicio", 6,  0, 23)),
                column(6, numericInput("wd_end",   "Hora fin",   22,  1, 30))
              ),
              fluidRow(
                column(6, numericInput("wd_freq", "Frecuencia (min)",       15, 1, 120)),
                column(6, numericInput("wd_betw", "Tiempo entre paradas (seg)", 300, 30, 3600))
              )
            )),
            card(card_header("Servicio Fin de Semana"), card_body(
              checkboxGroupInput("we_days", "Días activos",
                choices = c("Lun"=1,"Mar"=2,"Mié"=3,"Jue"=4,"Vie"=5,"Sáb"=6,"Dom"=7),
                selected = c(6,7), inline = TRUE),
              fluidRow(
                column(6, numericInput("we_start", "Hora inicio",  7,  0, 23)),
                column(6, numericInput("we_end",   "Hora fin",    21,  1, 30))
              ),
              fluidRow(
                column(6, numericInput("we_freq", "Frecuencia (min)",       20, 1, 120)),
                column(6, numericInput("we_betw", "Tiempo entre paradas (seg)", 300, 30, 3600))
              )
            ))
          ),
          card(card_header("Período de Validez"), card_body(
            fluidRow(
              column(4, dateInput("cal_start", "Fecha inicio", "2024-01-01")),
              column(4, dateInput("cal_end",   "Fecha fin",    "2024-12-31")),
              column(4, selectInput("cal_svc", "Servicios a incluir",
                choices = c("Laborables y fin de semana" = "both",
                            "Solo días laborables"        = "weekday",
                            "Solo fin de semana"          = "weekend"),
                selected = "both"))
            )
          ))
        ),

        # ── Tarifas ──────────────────────────────────────────
        nav_panel("Tarifas", icon = icon("coins"),
          card(card_header("Configuración de Tarifas"), card_body(
            checkboxInput("include_fares", "Incluir tarifas en el feed", TRUE),
            conditionalPanel("input.include_fares",
              fluidRow(
                column(4, numericInput("fare_adult",   "Tarifa adulto",    1.50, 0, step = 0.10)),
                column(4, numericInput("fare_student", "Tarifa estudiante",0.75, 0, step = 0.10)),
                column(4, numericInput("fare_senior",  "Tarifa 3ª edad",  0.75, 0, step = 0.10))
              ),
              selectInput("fare_pay", "Método de pago",
                choices = c("Al abordar (0)" = 0, "Antes de abordar (1)" = 1),
                selected = 0)
            )
          ))
        ),

        # ── Viajes y Horarios ────────────────────────────────
        nav_panel("Viajes y Horarios", icon = icon("clock"),
          uiOutput("trips_preview_ui")
        ),

        # ── Shapes / Rutas ───────────────────────────────────
        nav_panel("Shapes / Rutas", icon = icon("draw-polygon"),
          card(card_header("Trazado de Rutas (shapes.txt)"), card_body(
            fluidRow(
              column(7,
                fileInput("shapes_csv_upload", "Cargar shapes desde CSV/TXT",
                          accept = c(".csv", ".txt"),
                          buttonLabel = "Seleccionar archivo",
                          placeholder = "shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence")),
              column(5, div(class = "mt-4 pt-2",
                downloadButton("dl_shapes_tpl", "Plantilla CSV",
                               class = "btn-outline-secondary btn-sm w-100")))
            ),
            uiOutput("shapes_info_ui"),
            leafletOutput("map_shapes", height = "460px")
          ))
        )

      ) # navset_card_tab
    )
  ),

  # ─────────────────────────────────────────────────────────────
  # TAB 3: FUNCIONES AVANZADAS
  # ─────────────────────────────────────────────────────────────
  nav_panel(
    "Funciones Avanzadas", icon = icon("gears"),

    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = "#f8f9fa",
        tags$p(class = "fw-bold text-primary mb-1 small", "CARGAR FEED"),
        fileInput("gtfs_adv_zip", NULL, accept = ".zip",
                  buttonLabel = "Seleccionar ZIP",
                  placeholder = "Ningún archivo seleccionado"),
        hr(),
        tags$p(class = "fw-bold text-muted mb-1 small", "ANÁLISIS"),
        actionButton("btn_validate", "Validar Feed",
                     icon = icon("circle-check"),
                     class = "btn-outline-primary w-100 mb-1"),
        actionButton("btn_freq_adv", "Análisis de Frecuencias",
                     icon = icon("chart-line"),
                     class = "btn-outline-primary w-100 mb-3"),
        hr(),
        tags$p(class = "fw-bold text-muted mb-1 small", "EXPORTAR"),
        downloadButton("dl_adv_stops",  " Paradas (GeoPackage)",
                       class = "btn-outline-success btn-sm w-100 mb-1"),
        downloadButton("dl_adv_routes", " Rutas (GeoPackage)",
                       class = "btn-outline-success btn-sm w-100 mb-1"),
        downloadButton("dl_adv_report", " Reporte HTML",
                       class = "btn-outline-info btn-sm w-100")
      ),
      uiOutput("avanzado_ui")
    )
  ),

  # ─────────────────────────────────────────────────────────────
  # TAB 4: EXTRAER DESDE OSM
  # ─────────────────────────────────────────────────────────────
  nav_panel(
    "Extraer OSM", icon = icon("map-location-dot"),

    layout_sidebar(
      sidebar = sidebar(
        width = 290, bg = "#f8f9fa",

        tags$p(class = "fw-bold text-primary mb-1 small", "ÁREA DE INTERÉS"),
        textInput("osm_place", "Ciudad o lugar",
                  value = "Trujillo, Peru",
                  placeholder = "Ej. Trujillo, Peru"),
        actionButton("osm_geocode_btn", "Geocodificar",
                     icon = icon("magnifying-glass"),
                     class = "btn-outline-secondary btn-sm w-100 mb-2"),
        tags$p(class = "text-muted small mb-1", "— o ingresa bbox manualmente —"),
        fluidRow(
          column(6, numericInput("bbox_xmin", "Lon mín (W)", value = -79.094, step = 0.001)),
          column(6, numericInput("bbox_xmax", "Lon máx (E)", value = -78.953, step = 0.001))
        ),
        fluidRow(
          column(6, numericInput("bbox_ymin", "Lat mín (S)", value = -8.163, step = 0.001)),
          column(6, numericInput("bbox_ymax", "Lat máx (N)", value = -8.034, step = 0.001))
        ),
        hr(),

        tags$p(class = "fw-bold text-primary mb-1 small", "TIPO DE RUTA OSM"),
        selectInput("osm_route_type", NULL,
                    choices = c("bus", "share_taxi", "trolleybus"),
                    selected = "bus"),
        hr(),

        tags$p(class = "fw-bold text-muted mb-1 small", "EXTRACCIÓN"),
        actionButton("btn_extract_stops", "Extraer Paradas",
                     icon = icon("circle-dot"),
                     class = "btn-primary w-100 mb-1"),
        actionButton("btn_extract_routes", "Extraer Rutas",
                     icon = icon("route"),
                     class = "btn-primary w-100 mb-3"),
        hr(),

        tags$p(class = "fw-bold text-muted mb-1 small", "ACCIONES"),
        actionButton("btn_send_stops_to_gtfs", "Enviar paradas a Crear GTFS",
                     icon = icon("arrow-right"),
                     class = "btn-success w-100 mb-2"),
        downloadButton("dl_osm_stops_geojson", " Paradas (GeoJSON)",
                       class = "btn-outline-success btn-sm w-100 mb-1"),
        downloadButton("dl_osm_routes_geojson", " Rutas (GeoJSON)",
                       class = "btn-outline-success btn-sm w-100")
      ),

      uiOutput("osm_ui")
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text text-white-50 small",
                     "Curso-Taller GTFS"))
)

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {

  # ── TAB 1: ANÁLISIS ──────────────────────────────────────────

  gtfs_an <- eventReactive(input$analizar_btn, {
    req(input$gtfs_zip)
    withProgress(message = "Leyendo feed GTFS...", value = 0.6, {
      tryCatch(
        tidytransit::read_gtfs(input$gtfs_zip$datapath),
        error = function(e) { showNotification(e$message, type = "error", duration = 8); NULL }
      )
    })
  })

  freq_plot <- reactive({
    req(gtfs_an())
    df <- gtfs_an()$stop_times %>%
      mutate(hour = as.integer(substr(arrival_time, 1, 2))) %>%
      group_by(hour) %>%
      summarise(viajes = n_distinct(trip_id), .groups = "drop") %>%
      arrange(hour)

    ggplot(df, aes(x = hour, y = viajes)) +
      geom_col(fill = "#1a73e8", alpha = 0.85, width = 0.75) +
      geom_line(color = "#ea4335", linewidth = 1.2, group = 1) +
      geom_point(color = "#ea4335", size = 3) +
      scale_x_continuous(breaks = seq(0, 30, 2)) +
      labs(
        title    = "Frecuencia de Servicio por Hora",
        subtitle = paste("Agencia:", gtfs_an()$agency$agency_name[1]),
        x = "Hora del día", y = "Número de viajes"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", size = 15))
  })

  output$analisis_ui <- renderUI({
    if (is.null(gtfs_an())) {
      return(div(class = "placeholder-msg",
                 icon("cloud-arrow-up"),
                 "Cargue un archivo ZIP de GTFS y haga clic en ",
                 tags$b("Analizar Feed")))
    }
    gtfs <- gtfs_an()
    tagList(
      layout_columns(col_widths = c(2, 2, 3, 3, 2),
        value_box("Agencias",   nrow(gtfs$agency),
                  showcase = icon("building"),  theme = "primary"),
        value_box("Rutas",      nrow(gtfs$routes),
                  showcase = icon("route"),     theme = "info"),
        value_box("Paradas",    nrow(gtfs$stops),
                  showcase = icon("map-pin"),   theme = "success"),
        value_box("Viajes",     nrow(gtfs$trips),
                  showcase = icon("bus"),       theme = "warning"),
        value_box("Stop times", format(nrow(gtfs$stop_times), big.mark = ","),
                  showcase = icon("clock"),     theme = "secondary")
      ),
      navset_card_tab(
        nav_panel("Rutas",              DTOutput("tbl_routes")),
        nav_panel("Frecuencia Horaria", plotOutput("plt_freq", height = "420px")),
        nav_panel("Mapa de Paradas",    leafletOutput("map_stops", height = "500px")),
        nav_panel("Paradas",            DTOutput("tbl_stops")),
        nav_panel("Agencias",           DTOutput("tbl_agency"))
      )
    )
  })

  output$tbl_routes <- renderDT({
    req(gtfs_an())
    cols <- intersect(
      c("route_id", "route_short_name", "route_long_name", "route_type", "route_color"),
      names(gtfs_an()$routes))
    datatable(gtfs_an()$routes[, cols], rownames = FALSE,
              options = list(pageLength = 20, dom = "tip"))
  })

  output$plt_freq <- renderPlot({ freq_plot() })

  output$map_stops <- renderLeaflet({
    req(gtfs_an())
    gtfs   <- gtfs_an()
    stops  <- gtfs$stops
    shapes <- gtfs$shapes
    pal_colors <- c("#1a73e8","#ea4335","#34a853","#fbbc04","#9c27b0","#ff6d00","#00bcd4")

    m <- leaflet() %>% addProviderTiles("CartoDB.Positron")

    if (!is.null(shapes) && nrow(shapes) > 0) {
      ordered <- shapes[order(shapes$shape_id, as.integer(shapes$shape_pt_sequence)), ]
      ids     <- unique(ordered$shape_id)
      for (i in seq_along(ids)) {
        pts   <- ordered[ordered$shape_id == ids[i], ]
        color <- pal_colors[((i - 1L) %% length(pal_colors)) + 1L]
        m <- m %>% addPolylines(
          lng     = pts$shape_pt_lon,
          lat     = pts$shape_pt_lat,
          color   = color, weight = 3, opacity = 0.8,
          label   = ids[i], group = "Rutas"
        )
      }
    }

    m %>%
      addCircleMarkers(
        data = stops, lng = ~stop_lon, lat = ~stop_lat, radius = 5,
        color = "#1a73e8", fillColor = "#1a73e8",
        fillOpacity = 0.8, weight = 1,
        popup = ~paste0("<b>", stop_name, "</b><br><small>ID: ", stop_id, "</small>"),
        group = "Paradas"
      ) %>%
      addMeasure(primaryLengthUnit = "kilometers") %>%
      addLayersControl(
        overlayGroups = c("Rutas", "Paradas"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$tbl_stops <- renderDT({
    req(gtfs_an())
    datatable(gtfs_an()$stops, rownames = FALSE,
              options = list(pageLength = 20, dom = "tip"))
  })

  output$tbl_agency <- renderDT({
    req(gtfs_an())
    datatable(gtfs_an()$agency, rownames = FALSE, options = list(dom = "t"))
  })

  # Descargas Tab 1
  output$dl_freq_chart <- downloadHandler(
    filename = "frecuencia_horaria.png",
    content  = function(f) ggsave(f, freq_plot(), width = 10, height = 6, dpi = 150)
  )

  output$dl_map_png <- downloadHandler(
    filename = "mapa_paradas.png",
    content  = function(f) {
      req(gtfs_an())
      p <- ggplot(gtfs_an()$stops, aes(stop_lon, stop_lat)) +
        geom_point(color = "#ea4335", size = 1.8, alpha = 0.7) +
        coord_fixed() +
        labs(title = "Mapa de Paradas - GTFS",
             x = "Longitud", y = "Latitud") +
        theme_minimal(base_size = 12)
      ggsave(f, p, width = 10, height = 8, dpi = 150)
    }
  )

  output$dl_stops_gpkg <- downloadHandler(
    filename = "paradas.gpkg",
    content  = function(f) {
      req(gtfs_an())
      gtfs_an()$stops %>%
        st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
        st_write(f, delete_dsn = TRUE, quiet = TRUE)
    }
  )

  output$dl_report_an <- downloadHandler(
    filename = function() paste0("reporte_gtfs_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content  = function(f) {
      req(gtfs_an())
      writeLines(generate_html_report(gtfs_an()), f)
    }
  )

  # ── TAB 2: CREAR GTFS ────────────────────────────────────────

  stops_rv            <- reactiveVal(DEFAULT_STOPS)
  gtfs_zip_path       <- reactiveVal(NULL)
  generated_tables_rv <- reactiveVal(NULL)
  shapes_rv           <- reactiveVal(NULL)

  # Cargar paradas desde CSV
  observeEvent(input$stops_csv, {
    req(input$stops_csv)
    tryCatch({
      df <- read_csv(input$stops_csv$datapath, show_col_types = FALSE)
      req_cols <- c("stop_name", "stop_lat", "stop_lon")
      if (!all(req_cols %in% names(df))) {
        showNotification(
          paste("El CSV debe tener columnas:", paste(req_cols, collapse = ", ")),
          type = "error")
        return()
      }
      if (!"stop_id"   %in% names(df)) df$stop_id   <- sprintf("S%03d", seq_len(nrow(df)))
      if (!"stop_code" %in% names(df)) df$stop_code <- sprintf("PA%03d", seq_len(nrow(df)))
      stops_rv(df)
      showNotification(sprintf("%d paradas cargadas correctamente", nrow(df)), type = "message")
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
  })

  # Agregar fila
  observeEvent(input$add_stop, {
    df <- stops_rv()
    n  <- nrow(df) + 1
    stops_rv(bind_rows(df, tibble(
      stop_id   = sprintf("S%03d", n),
      stop_name = "Nueva parada",
      stop_lat  = round(mean(df$stop_lat), 4),
      stop_lon  = round(mean(df$stop_lon), 4),
      stop_code = sprintf("PA%03d", n)
    )))
  })

  # Eliminar fila seleccionada
  observeEvent(input$del_stop, {
    req(input$stops_table_rows_selected)
    df <- stops_rv()
    stops_rv(df[-input$stops_table_rows_selected, ])
  })

  output$stops_table <- renderDT({
    datatable(
      stops_rv(),
      editable  = "cell",
      selection = "single",
      rownames  = FALSE,
      options   = list(pageLength = 15, dom = "tip",
                       columnDefs = list(list(className = "dt-center", targets = "_all")))
    )
  })

  # Guardar edición de celda
  observeEvent(input$stops_table_cell_edit, {
    info <- input$stops_table_cell_edit
    df   <- stops_rv()
    df[info$row, info$col + 1] <- DT::coerceValue(info$value, df[info$row, info$col + 1])
    stops_rv(df)
  })

  output$dl_stops_tpl <- downloadHandler(
    filename = "plantilla_paradas.csv",
    content  = function(f) write_csv(DEFAULT_STOPS[1, ], f)
  )

  # Cargar shapes desde CSV / TXT
  observeEvent(input$shapes_csv_upload, {
    req(input$shapes_csv_upload)
    tryCatch({
      df <- read_csv(input$shapes_csv_upload$datapath, show_col_types = FALSE)
      req_cols <- c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence")
      if (!all(req_cols %in% names(df))) {
        showNotification(
          paste("El archivo debe tener columnas:", paste(req_cols, collapse = ", ")),
          type = "error")
        return()
      }
      shapes_rv(df %>%
        mutate(shape_pt_sequence = as.integer(shape_pt_sequence)) %>%
        arrange(shape_id, shape_pt_sequence))
      showNotification(
        sprintf("%d puntos cargados (%d shapes / rutas)",
                nrow(df), length(unique(df$shape_id))),
        type = "message")
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
  })

  output$dl_shapes_tpl <- downloadHandler(
    filename = "plantilla_shapes.csv",
    content  = function(f) {
      write_csv(tibble(
        shape_id          = c("RUTA_01","RUTA_01","RUTA_01","RUTA_01",
                              "RUTA_02","RUTA_02","RUTA_02"),
        shape_pt_lat      = c(-8.1116,-8.1132,-8.1089,-8.1042,
                              -8.1042,-8.1089,-8.1116),
        shape_pt_lon      = c(-79.0289,-79.0276,-79.0254,-79.0231,
                              -79.0231,-79.0254,-79.0289),
        shape_pt_sequence = c(0L,1L,2L,3L, 0L,1L,2L)
      ), f)
    }
  )

  output$shapes_info_ui <- renderUI({
    if (is.null(shapes_rv())) return(NULL)
    sh <- shapes_rv()
    div(class = "alert alert-success py-2 small mb-2",
      icon("circle-check"),
      sprintf(" %d shapes cargados · %d puntos totales",
              length(unique(sh$shape_id)), nrow(sh))
    )
  })

  output$map_shapes <- renderLeaflet({
    stops  <- stops_rv()
    shapes <- shapes_rv()
    pal_colors <- c("#1a73e8","#ea4335","#34a853","#fbbc04","#9c27b0","#ff6d00","#00bcd4")

    m <- leaflet() %>% addProviderTiles("CartoDB.Positron")

    if (!is.null(shapes) && nrow(shapes) > 0) {
      ids            <- unique(shapes$shape_id)
      pts_per_shape  <- tabulate(match(shapes$shape_id, ids))

      if (all(pts_per_shape == 1L)) {
        # Every row has a unique shape_id (point-per-row format):
        # connect ALL points as one polyline ordered by shape_pt_sequence
        pts <- shapes[order(shapes$shape_pt_sequence), ]
        m <- m %>% addPolylines(
          lng     = pts$shape_pt_lon,
          lat     = pts$shape_pt_lat,
          color   = pal_colors[1], weight = 4, opacity = 0.85,
          label   = paste(nrow(pts), "puntos"), group = "Shapes"
        )
      } else {
        # Standard GTFS: group by shape_id, draw one polyline per shape
        for (i in seq_along(ids)) {
          pts <- shapes[shapes$shape_id == ids[i], ]
          pts <- pts[order(pts$shape_pt_sequence), ]
          if (nrow(pts) < 2L) next
          color <- pal_colors[((i - 1L) %% length(pal_colors)) + 1L]
          m <- m %>% addPolylines(
            lng     = pts$shape_pt_lon,
            lat     = pts$shape_pt_lat,
            color   = color, weight = 4, opacity = 0.85,
            label   = ids[i], group = "Shapes"
          )
        }
      }
    }

    if (!is.null(stops) && nrow(stops) > 0) {
      m <- m %>% addCircleMarkers(
        data = stops, lng = ~stop_lon, lat = ~stop_lat,
        radius = 6, color = "#ea4335", fillColor = "#ea4335",
        fillOpacity = 0.9, weight = 2,
        popup = ~paste0("<b>", stop_name, "</b><br><small>ID: ", stop_id, "</small>"),
        group = "Paradas"
      )
    }

    m %>%
      addLayersControl(
        overlayGroups = c("Shapes", "Paradas"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addMeasure(primaryLengthUnit = "kilometers")
  })

  output$creator_status <- renderUI({
    sh_n <- if (!is.null(shapes_rv())) length(unique(shapes_rv()$shape_id)) else 0L
    tags$ul(class = "list-unstyled small text-muted mb-0",
      tags$li(icon("map-pin",       class = "text-success"),
              sprintf(" %d paradas cargadas", nrow(stops_rv()))),
      tags$li(icon("draw-polygon",  class = if (sh_n > 0) "text-warning" else "text-muted"),
              sprintf(" %d shapes cargados", sh_n)),
      tags$li(icon("route",         class = "text-info"),
              sprintf(" Ruta: %s (%s)", input$rt_short, input$rt_id)),
      tags$li(icon("calendar",      class = "text-primary"),
              sprintf(" Servicios: %s", input$cal_svc))
    )
  })

  # Generar feed GTFS
  observeEvent(input$generar_gtfs, {
    stops <- stops_rv()
    if (nrow(stops) < 2) {
      showNotification("Se necesitan al menos 2 paradas para generar el feed", type = "error")
      return()
    }
    withProgress(message = "Generando feed GTFS...", {

      # Agency
      agency <- tibble(
        agency_id       = input$ag_id,
        agency_name     = input$ag_name,
        agency_url      = input$ag_url,
        agency_timezone = input$ag_tz,
        agency_lang     = input$ag_lang
      )
      if (nchar(trimws(input$ag_phone)) > 0)
        agency$agency_phone <- trimws(input$ag_phone)

      # Routes
      routes <- tibble(
        route_id         = input$rt_id,
        route_short_name = input$rt_short,
        route_long_name  = input$rt_long,
        route_type       = as.integer(input$rt_type),
        route_color      = input$rt_color,
        route_text_color = input$rt_text_color,
        agency_id        = input$ag_id
      )

      # Calendar
      start_dt <- as.integer(format(input$cal_start, "%Y%m%d"))
      end_dt   <- as.integer(format(input$cal_end,   "%Y%m%d"))
      wd       <- as.integer(1:7 %in% as.integer(input$wd_days))
      we       <- as.integer(1:7 %in% as.integer(input$we_days))

      calendar <- tibble(
        service_id = c("WEEKDAY", "WEEKEND"),
        monday     = c(wd[1], we[1]), tuesday   = c(wd[2], we[2]),
        wednesday  = c(wd[3], we[3]), thursday  = c(wd[4], we[4]),
        friday     = c(wd[5], we[5]), saturday  = c(wd[6], we[6]),
        sunday     = c(wd[7], we[7]),
        start_date = start_dt, end_date = end_dt
      )

      # Configuraciones de servicio
      svc_all <- list(
        list(id="WEEKDAY", did=0L, dn="IDA", sh=input$wd_start, eh=input$wd_end, fm=input$wd_freq, bs=input$wd_betw),
        list(id="WEEKDAY", did=1L, dn="VTA", sh=input$wd_start, eh=input$wd_end, fm=input$wd_freq, bs=input$wd_betw),
        list(id="WEEKEND", did=0L, dn="IDA", sh=input$we_start, eh=input$we_end, fm=input$we_freq, bs=input$we_betw),
        list(id="WEEKEND", did=1L, dn="VTA", sh=input$we_start, eh=input$we_end, fm=input$we_freq, bs=input$we_betw)
      )
      svcs <- switch(input$cal_svc,
        weekday = svc_all[1:2],
        weekend = svc_all[3:4],
        svc_all)

      results <- lapply(svcs, function(s) {
        generate_trips_stop_times(stops, input$rt_id, s$id, s$did, s$dn,
                                  s$sh, s$eh, s$fm, s$bs)
      })

      tables <- list(
        agency     = agency,
        stops      = stops,
        routes     = routes,
        calendar   = calendar,
        trips      = bind_rows(lapply(results, `[[`, "trips")),
        stop_times = bind_rows(lapply(results, `[[`, "stop_times"))
      )

      if (input$include_fares) {
        tables$fare_attributes <- tibble(
          fare_id      = c("FARE_ADULT", "FARE_STUDENT", "FARE_SENIOR"),
          price        = c(input$fare_adult, input$fare_student, input$fare_senior),
          currency_type = input$ag_currency,
          payment_method = as.integer(input$fare_pay),
          transfers    = 0L
        )
        tables$fare_rules <- tibble(
          fare_id  = c("FARE_ADULT", "FARE_STUDENT", "FARE_SENIOR"),
          route_id = input$rt_id
        )
      }

      if (!is.null(shapes_rv()) && nrow(shapes_rv()) > 0)
        tables$shapes <- shapes_rv()

      tmp <- tempfile(fileext = ".zip")
      build_and_zip_gtfs(tables, tmp)
      gtfs_zip_path(tmp)
      generated_tables_rv(tables)

      showNotification(
        paste0("Feed generado: ", nrow(tables$trips), " viajes, ",
               format(nrow(tables$stop_times), big.mark = ","), " stop times"),
        type = "message", duration = 6
      )
    })
  })

  output$dl_gtfs_ui <- renderUI({
    req(gtfs_zip_path())
    downloadButton("dl_gtfs_creado",
                   paste0(" Descargar ", toupper(input$rt_id), ".zip"),
                   class = "btn-primary w-100")
  })

  output$dl_gtfs_creado <- downloadHandler(
    filename = function()
      paste0("gtfs_", input$rt_id, "_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content = function(f) {
      req(gtfs_zip_path())
      file.copy(gtfs_zip_path(), f)
    }
  )

  # Botones de descarga individual (trips.txt / stop_times.txt)
  output$dl_individual_ui <- renderUI({
    req(generated_tables_rv())
    tbl <- generated_tables_rv()
    tagList(
      hr(),
      tags$p(class = "fw-bold text-muted mb-1 small", "ARCHIVOS INDIVIDUALES"),
      downloadButton("dl_trips_txt", icon("table-cells", class = "me-1"),
                     " trips.txt",
                     class = "btn-outline-secondary btn-sm w-100 mb-1"),
      downloadButton("dl_st_txt",    icon("list-ul",     class = "me-1"),
                     " stop_times.txt",
                     class = "btn-outline-secondary btn-sm w-100 mb-1"),
      tags$p(class = "text-muted small mt-1 mb-0",
             icon("circle-info"),
             sprintf(" %d viajes · %s horarios",
                     nrow(tbl$trips),
                     format(nrow(tbl$stop_times), big.mark = ",")))
    )
  })

  output$dl_trips_txt <- downloadHandler(
    filename = "trips.txt",
    content  = function(f) {
      req(generated_tables_rv())
      write_csv(generated_tables_rv()$trips, f, na = "")
    }
  )

  output$dl_st_txt <- downloadHandler(
    filename = "stop_times.txt",
    content  = function(f) {
      req(generated_tables_rv())
      write_csv(generated_tables_rv()$stop_times, f, na = "")
    }
  )

  # Pestaña "Viajes y Horarios" — previsualización de trips + stop_times
  output$trips_preview_ui <- renderUI({
    if (is.null(generated_tables_rv())) {
      return(div(class = "placeholder-msg",
                 icon("clock"),
                 tags$p("Complete los formularios y haga clic en"),
                 tags$p(tags$b("Generar GTFS")),
                 tags$p("para previsualizar los viajes y horarios generados.")))
    }
    tbl <- generated_tables_rv()
    tagList(
      layout_columns(col_widths = c(4, 4, 4),
        value_box("Viajes generados",
                  nrow(tbl$trips),
                  showcase = icon("bus"),   theme = "primary"),
        value_box("Stop times",
                  format(nrow(tbl$stop_times), big.mark = ","),
                  showcase = icon("clock"), theme = "info"),
        value_box("Paradas por viaje",
                  nrow(tbl$stops),
                  showcase = icon("map-pin"), theme = "success")
      ),
      navset_card_tab(
        nav_panel("Viajes (trips.txt)",
          DTOutput("tbl_trips_preview")
        ),
        nav_panel("Horarios (stop_times.txt)",
          DTOutput("tbl_st_preview")
        )
      )
    )
  })

  output$tbl_trips_preview <- renderDT({
    req(generated_tables_rv())
    datatable(generated_tables_rv()$trips,
              rownames = FALSE,
              options  = list(pageLength = 15, dom = "tip", scrollX = TRUE))
  })

  output$tbl_st_preview <- renderDT({
    req(generated_tables_rv())
    datatable(generated_tables_rv()$stop_times,
              rownames = FALSE,
              options  = list(pageLength = 20, dom = "tip", scrollX = TRUE))
  })

  # ── TAB 3: FUNCIONES AVANZADAS ────────────────────────────────

  gtfs_adv <- reactive({
    req(input$gtfs_adv_zip)
    tryCatch(
      gtfstools::read_gtfs(input$gtfs_adv_zip$datapath),
      error = function(e) { showNotification(e$message, type = "error", duration = 8); NULL }
    )
  })

  # reactiveVals para almacenar resultados (evita el problema de dependencia con eventReactive)
  val_rv  <- reactiveVal(NULL)
  freq_rv <- reactiveVal(NULL)

  # Limpiar resultados al cargar un nuevo archivo
  observeEvent(input$gtfs_adv_zip, { val_rv(NULL); freq_rv(NULL) })

  observeEvent(input$btn_validate, {
    req(gtfs_adv())
    withProgress(message = "Validando feed...", {
      g <- gtfs_adv()
      coords <- g$stops %>%
        summarise(lat_min = min(stop_lat, na.rm=TRUE), lat_max = max(stop_lat, na.rm=TRUE),
                  lon_min = min(stop_lon, na.rm=TRUE), lon_max = max(stop_lon, na.rm=TRUE))
      result <- list(
        stats = list(agencies=nrow(g$agency), routes=nrow(g$routes),
                     stops=nrow(g$stops), trips=nrow(g$trips), st=nrow(g$stop_times)),
        calendar   = if (!is.null(g$calendar))
          g$calendar %>%
            mutate(dias_activos = monday+tuesday+wednesday+thursday+friday+saturday+sunday) %>%
            select(service_id, dias_activos, start_date, end_date)
          else NULL,
        trips_svc  = {
          grp_cols <- intersect(c("service_id", "direction_id", "route_id"), names(g$trips))
          g$trips %>% count(across(all_of(grp_cols)), name = "viajes")
        },
        coords     = coords,
        coords_ok  = coords$lat_min >= -18.5 && coords$lat_max <= 0 &&
                     coords$lon_min >= -81.5 && coords$lon_max <= -68,
        routes_ok  = sum(g$routes$route_id %in% g$trips$route_id),
        total_rt   = nrow(g$routes),
        stops_used = length(unique(g$stop_times$stop_id)),
        total_stops= nrow(g$stops),
        has_shapes = !is.null(g$shapes) && nrow(g$shapes) > 0
      )
      val_rv(result)
      showNotification(
        paste0("Validación completada: ", result$stats$routes, " rutas, ",
               result$stats$stops, " paradas, ", result$stats$trips, " viajes"),
        type = "message", duration = 5
      )
    })
  })

  observeEvent(input$btn_freq_adv, {
    req(gtfs_adv())
    withProgress(message = "Calculando frecuencias...", {
      result <- gtfs_adv()$stop_times %>%
        mutate(hour = as.integer(substr(arrival_time, 1, 2))) %>%
        group_by(hour) %>%
        summarise(viajes = n_distinct(trip_id), .groups = "drop") %>%
        arrange(hour)
      freq_rv(result)
      showNotification(
        paste0("Frecuencias calculadas: ", nrow(result), " horas con servicio"),
        type = "message", duration = 4
      )
    })
  })

  output$avanzado_ui <- renderUI({
    if (is.null(input$gtfs_adv_zip)) {
      return(div(class = "placeholder-msg",
                 icon("cloud-arrow-up"),
                 "Cargue un archivo ZIP de GTFS para comenzar"))
    }

    vr <- val_rv()
    fa <- freq_rv()

    if (is.null(vr) && is.null(fa)) {
      return(div(class = "placeholder-msg",
                 icon("circle-info"),
                 tags$p("Feed cargado correctamente."),
                 tags$p("Use los botones del panel izquierdo para ejecutar los análisis.")))
    }

    tagList(
      # Resultados de validación
      if (!is.null(vr)) tagList(
        layout_columns(col_widths = c(4, 4, 4),
          value_box(
            "Rutas con viajes",
            sprintf("%d / %d", vr$routes_ok, vr$total_rt),
            showcase = icon("route"),
            theme    = if (vr$routes_ok == vr$total_rt) "success" else "warning"
          ),
          value_box(
            "Paradas usadas",
            sprintf("%d / %d", vr$stops_used, vr$total_stops),
            showcase = icon("map-pin"),
            theme    = if (vr$stops_used == vr$total_stops) "success" else "info"
          ),
          value_box(
            "Coordenadas",
            if (vr$coords_ok) "Dentro de Per\u00fa" else "Verificar rango",
            showcase = icon(if (vr$coords_ok) "circle-check" else "triangle-exclamation"),
            theme    = if (vr$coords_ok) "success" else "warning"
          )
        ),
        layout_columns(col_widths = c(4, 4, 4),
          card(card_header("Estadísticas del Feed"), card_body(
            tags$table(class = "table table-sm table-hover mb-0",
              tags$tr(tags$td("Agencias"),   tags$td(class="fw-bold text-end", vr$stats$agencies)),
              tags$tr(tags$td("Rutas"),      tags$td(class="fw-bold text-end", vr$stats$routes)),
              tags$tr(tags$td("Paradas"),    tags$td(class="fw-bold text-end", vr$stats$stops)),
              tags$tr(tags$td("Viajes"),     tags$td(class="fw-bold text-end", vr$stats$trips)),
              tags$tr(tags$td("Stop times"), tags$td(class="fw-bold text-end",
                                                     format(vr$stats$st, big.mark=",")))
            )
          )),
          card(card_header("Cobertura Geográfica"), card_body(
            tags$table(class = "table table-sm mb-0",
              tags$tr(tags$td("Lat mín"), tags$td(class="text-end", round(vr$coords$lat_min, 5))),
              tags$tr(tags$td("Lat máx"), tags$td(class="text-end", round(vr$coords$lat_max, 5))),
              tags$tr(tags$td("Lon mín"), tags$td(class="text-end", round(vr$coords$lon_min, 5))),
              tags$tr(tags$td("Lon máx"), tags$td(class="text-end", round(vr$coords$lon_max, 5)))
            )
          )),
          card(card_header("Integridad Referencial"), card_body(
            tags$ul(class = "list-unstyled small mb-0",
              tags$li(
                icon(if(vr$routes_ok==vr$total_rt)"check"else"exclamation",
                     class=if(vr$routes_ok==vr$total_rt)"text-success"else"text-warning"),
                sprintf(" %d/%d rutas tienen viajes", vr$routes_ok, vr$total_rt)
              ),
              tags$li(
                icon(if(vr$stops_used==vr$total_stops)"check"else"info",
                     class=if(vr$stops_used==vr$total_stops)"text-success"else"text-info"),
                sprintf(" %d/%d paradas en stop_times", vr$stops_used, vr$total_stops)
              ),
              tags$li(
                icon(if(vr$has_shapes)"check"else"minus", class="text-muted"),
                if(vr$has_shapes)" Shapes disponibles" else " Sin datos de shapes"
              )
            )
          ))
        ),
        layout_columns(col_widths = c(6, 6),
          card(card_header("Viajes por Servicio"), card_body(
            DTOutput("tbl_trips_svc")
          )),
          if (!is.null(vr$calendar))
            card(card_header("Calendario de Servicios"), card_body(
              DTOutput("tbl_calendar")
            ))
        )
      ),

      # Gráfico frecuencia avanzado
      if (!is.null(fa))
        card(card_header("Análisis de Frecuencias por Hora"), card_body(
          plotOutput("plt_freq_adv", height = "360px")
        ))
    )
  })

  output$tbl_trips_svc <- renderDT({
    req(val_rv())
    datatable(val_rv()$trips_svc, rownames = FALSE,
              options = list(dom = "tip", pageLength = 20))
  })

  output$tbl_calendar <- renderDT({
    req(val_rv())
    req(val_rv()$calendar)
    datatable(val_rv()$calendar, rownames = FALSE,
              options = list(dom = "t"))
  })

  output$plt_freq_adv <- renderPlot({
    req(freq_rv())
    df <- freq_rv()
    ggplot(df, aes(hour, viajes)) +
      geom_col(fill = "#1a73e8", alpha = 0.85, width = 0.75) +
      geom_line(color = "#ea4335", linewidth = 1.2, group = 1) +
      geom_point(color = "#ea4335", size = 3) +
      scale_x_continuous(breaks = seq(0, 30, 2)) +
      labs(title = "Frecuencia de Servicio por Hora",
           x = "Hora del día", y = "Número de viajes") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", size = 15))
  })

  # Descargas Tab 3
  output$dl_adv_stops <- downloadHandler(
    filename = "paradas_avanzado.gpkg",
    content  = function(f) {
      req(gtfs_adv())
      gtfs_adv()$stops %>%
        st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
        st_write(f, delete_dsn = TRUE, quiet = TRUE)
    }
  )

  output$dl_adv_routes <- downloadHandler(
    filename = "rutas_avanzado.gpkg",
    content  = function(f) {
      req(gtfs_adv())
      g <- gtfs_adv()
      if (is.null(g$shapes) || nrow(g$shapes) == 0) {
        showNotification("El feed no contiene shapes", type = "warning")
        return()
      }
      ordered <- g$shapes[order(g$shapes$shape_id, g$shapes$shape_pt_sequence), ]
      ids     <- unique(ordered$shape_id)
      lines   <- lapply(ids, function(sid) {
        pts <- ordered[ordered$shape_id == sid, ]
        st_linestring(cbind(pts$shape_pt_lon, pts$shape_pt_lat))
      })
      st_sf(shape_id = ids, geometry = st_sfc(lines, crs = 4326)) %>%
        st_write(f, delete_dsn = TRUE, quiet = TRUE)
    }
  )

  output$dl_adv_report <- downloadHandler(
    filename = function()
      paste0("reporte_avanzado_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content  = function(f) {
      req(gtfs_adv())
      writeLines(generate_html_report(gtfs_adv()), f)
    }
  )

  # ── TAB 4: EXTRAER DESDE OSM ─────────────────────────────────

  osm_stops_rv  <- reactiveVal(NULL)
  osm_routes_rv <- reactiveVal(NULL)
  osm_log_rv    <- reactiveVal(character(0))

  osm_log <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
    osm_log_rv(c(osm_log_rv(), msg))
  }

  observeEvent(input$osm_geocode_btn, {
    req(nzchar(input$osm_place))
    withProgress(message = "Geocodificando...", value = 0.5, {
      tryCatch({
        bb <- getbb(input$osm_place, format_out = "matrix")
        if (is.null(bb) || !is.matrix(bb) || any(is.na(bb))) {
          showNotification(
            paste0("Lugar '", input$osm_place, "' no encontrado. Verifica el nombre o usa bbox manual."),
            type = "warning"
          )
          osm_log("Geocodificación sin resultado para '", input$osm_place, "'.")
        } else {
          updateNumericInput(session, "bbox_xmin", value = round(bb["x", "min"], 6))
          updateNumericInput(session, "bbox_xmax", value = round(bb["x", "max"], 6))
          updateNumericInput(session, "bbox_ymin", value = round(bb["y", "min"], 6))
          updateNumericInput(session, "bbox_ymax", value = round(bb["y", "max"], 6))
          osm_log("Bbox para '", input$osm_place, "' obtenido correctamente.")
        }
      }, error = function(e) {
        osm_log("Error al geocodificar: ", conditionMessage(e))
        showNotification(paste("No se pudo geocodificar:", conditionMessage(e)),
                         type = "error")
      })
    })
  })

  current_bbox <- reactive({
    c(xmin = input$bbox_xmin, ymin = input$bbox_ymin,
      xmax = input$bbox_xmax, ymax = input$bbox_ymax)
  })

  observeEvent(input$btn_extract_stops, {
    osm_log_rv(character(0))
    osm_stops_rv(NULL)
    withProgress(message = "Consultando Overpass API...", value = 0, {
      setProgress(0.2, detail = "Descargando paradas de bus...")
      osm_log("Iniciando extracción de paradas para bbox: ",
              paste(round(current_bbox(), 4), collapse = ", "))
      tryCatch({
        stops <- fetch_osm_stops(current_bbox())
        osm_stops_rv(stops)
        osm_log(nrow(stops), " paradas encontradas.")
        setProgress(1, detail = "Listo.")
      }, error = function(e) {
        osm_log("Error: ", conditionMessage(e))
        showNotification(conditionMessage(e), type = "error")
      })
    })
  })

  observeEvent(input$btn_extract_routes, {
    osm_routes_rv(NULL)
    withProgress(message = "Consultando Overpass API...", value = 0, {
      setProgress(0.2, detail = paste("Descargando rutas:", input$osm_route_type))
      osm_log("Iniciando extracción de rutas '", input$osm_route_type,
              "' para bbox: ", paste(round(current_bbox(), 4), collapse = ", "))
      tryCatch({
        routes <- fetch_osm_routes(current_bbox(), route_type = input$osm_route_type)
        osm_routes_rv(routes)
        osm_log(nrow(routes), " rutas encontradas.")
        setProgress(1, detail = "Listo.")
      }, error = function(e) {
        osm_log("Error: ", conditionMessage(e))
        showNotification(conditionMessage(e), type = "error")
      })
    })
  })

  observeEvent(input$btn_send_stops_to_gtfs, {
    stops <- osm_stops_rv()
    req(!is.null(stops) && nrow(stops) > 0)
    coords <- st_coordinates(stops)
    new_stops <- data.frame(
      stop_id   = if ("osm_id" %in% names(stops)) as.character(stops$osm_id) else sprintf("S%03d", seq_len(nrow(stops))),
      stop_name = if ("name" %in% names(stops)) ifelse(is.na(stops$name), paste("Parada", seq_len(nrow(stops))), stops$name) else paste("Parada", seq_len(nrow(stops))),
      stop_lat  = round(coords[, "Y"], 7),
      stop_lon  = round(coords[, "X"], 7),
      stop_code = if ("ref" %in% names(stops)) ifelse(is.na(stops$ref), sprintf("OSM%03d", seq_len(nrow(stops))), stops$ref) else sprintf("OSM%03d", seq_len(nrow(stops))),
      stringsAsFactors = FALSE
    )
    stops_rv(new_stops)
    showNotification(
      paste(nrow(new_stops), "paradas enviadas a 'Crear GTFS' → pestaña Paradas."),
      type = "message"
    )
    osm_log(nrow(new_stops), " paradas enviadas a la pestaña Crear GTFS.")
  })

  output$osm_ui <- renderUI({
    stops  <- osm_stops_rv()
    routes <- osm_routes_rv()
    log    <- osm_log_rv()

    has_stops  <- !is.null(stops)  && nrow(stops)  > 0
    has_routes <- !is.null(routes) && nrow(routes) > 0

    tagList(
      if (length(log) > 0) {
        card(
          card_header(icon("terminal"), " Registro"),
          tags$pre(style = "font-size:.8rem; max-height:120px; overflow-y:auto; margin:0;",
                   paste(log, collapse = "\n"))
        )
      },

      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "Paradas OSM",
          value = if (has_stops) nrow(stops) else "—",
          showcase = icon("circle-dot"),
          theme = if (has_stops) "primary" else "secondary"
        ),
        value_box(
          title = "Rutas OSM",
          value = if (has_routes) nrow(routes) else "—",
          showcase = icon("route"),
          theme = if (has_routes) "primary" else "secondary"
        ),
        value_box(
          title = "Bbox activa",
          value = paste0(
            round(input$bbox_ymin, 3), " / ",
            round(input$bbox_ymax, 3)
          ),
          showcase = icon("crop"),
          theme = "light"
        )
      ),

      card(
        card_header(icon("map"), " Mapa de Resultados"),
        leafletOutput("osm_map", height = 420)
      ),

      if (has_stops) {
        card(
          card_header(icon("table"), " Paradas (", nrow(stops), ")"),
          DTOutput("osm_stops_table", height = "260px")
        )
      },

      if (has_routes) {
        card(
          card_header(icon("table"), " Rutas (", nrow(routes), ")"),
          DTOutput("osm_routes_table", height = "260px")
        )
      },

      if (!has_stops && !has_routes && length(log) == 0) {
        tags$div(class = "placeholder-msg",
          icon("map-location-dot"),
          tags$p("Ingresa un área de interés y extrae paradas o rutas desde OpenStreetMap.")
        )
      }
    )
  })

  output$osm_map <- renderLeaflet({
    stops  <- osm_stops_rv()
    routes <- osm_routes_rv()
    bbox   <- current_bbox()

    valid_bbox <- !any(is.na(bbox)) &&
      isTRUE(bbox["xmin"] < bbox["xmax"]) &&
      isTRUE(bbox["ymin"] < bbox["ymax"]) &&
      isTRUE(bbox["xmin"] >= -180) && isTRUE(bbox["xmax"] <= 180) &&
      isTRUE(bbox["ymin"] >= -90)  && isTRUE(bbox["ymax"] <= 90)

    m <- leaflet() |> addProviderTiles("CartoDB.Positron")
    m <- if (valid_bbox)
      fitBounds(m, bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
    else
      setView(m, lng = -79.02, lat = -8.1, zoom = 12)

    if (!is.null(routes) && nrow(routes) > 0) {
      m <- m |> addPolylines(
        data = routes,
        color = "#e74c3c", weight = 2.5, opacity = 0.7,
        label  = ~if ("name" %in% names(routes)) name else osm_id,
        group  = "Rutas"
      )
    }

    if (!is.null(stops) && nrow(stops) > 0) {
      lbl <- if ("name" %in% names(stops)) stops$name else stops$osm_id
      lbl[is.na(lbl)] <- "Sin nombre"
      m <- m |> addCircleMarkers(
        data = stops,
        radius = 5, color = "#1a73e8", fillOpacity = 0.85,
        stroke = FALSE, label = lbl,
        group  = "Paradas"
      )
    }

    m |> addLayersControl(
      overlayGroups = c("Paradas", "Rutas"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })

  output$osm_stops_table <- renderDT({
    req(osm_stops_rv())
    df <- st_drop_geometry(osm_stops_rv())
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$osm_routes_table <- renderDT({
    req(osm_routes_rv())
    df <- st_drop_geometry(osm_routes_rv())
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_osm_stops_geojson <- downloadHandler(
    filename = function() paste0("osm_stops_", format(Sys.Date(), "%Y%m%d"), ".geojson"),
    content  = function(f) {
      req(osm_stops_rv())
      st_write(osm_stops_rv(), f, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    }
  )

  output$dl_osm_routes_geojson <- downloadHandler(
    filename = function() paste0("osm_routes_", format(Sys.Date(), "%Y%m%d"), ".geojson"),
    content  = function(f) {
      req(osm_routes_rv())
      st_write(osm_routes_rv(), f, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    }
  )
}

shinyApp(ui, server)
