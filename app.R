# VirtuaLAB XXI (Shinylive-safe)
# NOTA: Solo CSV para descargas (sin Excel). Sin dependencias no soportadas.

library(shiny)
library(ggplot2)
if (FALSE) {
  library(munsell)
}
if (FALSE) {
  library(tibble)
  library(munsell)
}

library(shiny)
library(ggplot2)

ui <- fluidPage(
  withMathJax(),
  tags$head(
    tags$style(HTML("
      body { background:#f4f6f9; margin:0; }
      .app-header{
        background:#3c8dbc; color:white; padding:14px 18px;
        font-size:20px; font-weight:700; border-radius:8px;
        margin:12px 12px 0 12px;
      }
      .app-header .alpha{ color:#FFD700; font-size:26px; font-weight:800; margin-left:6px; }

      .sidebar{
        background:#222d32; color:#b8c7ce;
        min-height: calc(100vh - 92px);
        border-radius:8px;
        padding:10px 0;
      }
      .sidebar a{
        display:block; padding:10px 16px;
        color:#b8c7ce; text-decoration:none;
        font-size:14px;
      }
      .sidebar a:hover{ background:#1e282c; color:white; }
      .sidebar a.active{ background:#1e282c; color:white; border-left:4px solid #3c8dbc; padding-left:12px; }

      .content-wrap{ padding:12px; }

      .box{
        background:white; border-radius:8px;
        border:1px solid #d9e6f5;
        box-shadow:0 1px 2px rgba(0,0,0,.05);
        margin-bottom:14px;
      }
      .box .box-header{
        padding:10px 14px;
        border-bottom:1px solid #e6f0ff;
        font-weight:700;
        background:#3c8dbc;
        color:white;
        border-top-left-radius:8px;
        border-top-right-radius:8px;
      }
      .box .box-body{ padding:14px; }

      /* Oculta la barra de tabs (pero sin romper su funcionamiento) */
      .nav-tabs{
        visibility: hidden;
        height: 0 !important;
        overflow: hidden;
        margin: 0 !important;
        padding: 0 !important;
        border: 0 !important;
      }

      .result-box { margin-top:10px; padding:14px; background:#f5faff; border:1px solid #cfe3ff; border-radius:8px; }

      .sample-scroll {
        height: 340px;
        overflow-y: scroll;
        border: 1px solid #cfe3ff;
        background: white;
        padding: 8px;
        border-radius: 8px;
      }

      .formula-box { background: #ffffff; padding: 14px; border-radius: 8px; border: 1px solid #d9e6f5; margin-top: 10px; }

      mjx-container { font-family: 'Times New Roman', Times, serif !important; }
      
      /* === X. Glosario y simbología === */
.glossary-wrap { font-family: 'Times New Roman', Times, serif; }
table.glossary-table { width:100%; border-collapse:collapse; font-size:15px; background:white; }
table.glossary-table th, table.glossary-table td { border:1px solid #cfe3ff; padding:10px; vertical-align:top; }
table.glossary-table th { background:#f5faff; font-weight:700; text-align:left; }
.math-serif-bold { font-family: serif; font-weight:700; }
.math-serif-bold-italic { font-family: serif; font-weight:700; font-style:italic; }
.pedagogic-note { margin-top:14px; padding:12px 14px; border-left:5px solid #777; background:#fafafa; border-radius:8px; }
.pedagogic-note .title { font-weight:700; margin-bottom:8px; }
.pedagogic-note .mathline { margin:6px 0; text-align:center; font-size:16px; }
      
    ")),
    tags$script(HTML("
  // Descarga de texto (CSV) sin servidor: crea un Blob y fuerza download
  Shiny.addCustomMessageHandler('download_text_file', function(msg) {
    try {
      var blob = new Blob([msg.content], { type: msg.mime || 'text/csv;charset=utf-8' });
      var url = URL.createObjectURL(blob);

      var a = document.createElement('a');
      a.href = url;
      a.download = msg.filename || 'archivo.csv';
      document.body.appendChild(a);
      a.click();

      setTimeout(function() {
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      }, 0);
    } catch (e) {
      console.error('download_text_file failed', e);
      alert('No se pudo generar la descarga en este navegador.');
    }
  });
"))
    
  ),
  
  div(class="app-header", HTML("VirtuaLAB XXI <span class='alpha'>𝛼</span>")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        class="sidebar",
        tags$a("I. Presentación", href="#", `data-tab`="intro", class="active"),
        tags$a("II. Población", href="#", `data-tab`="poblacion"),
        tags$a("III. Variable cualitativa", href="#", `data-tab`="var_cuali"),
        tags$a("IV. Variable cuantitativa", href="#", `data-tab`="var_cuanti"),
        tags$a("V. Marco Muestral", href="#", `data-tab`="marco"),
        tags$a("VI. Noción de muestreo", href="#", `data-tab`="nocion"),
        tags$a("VII. Muestreo aleatorio", href="#", `data-tab`="muestreo_vii"),
        tags$a("VIII. Análisis descriptivo e Inferencia", href="#", `data-tab`="inferencia_viii"),
        tags$a("IX. Interpretación de la inferencia", href="#", `data-tab`="interpretacion_ix"),
        tags$a("X. Glosario y simbología", href="#", `data-tab`="glosario_x")
      ),
      tags$script(HTML("
        $(document).on('click', '.sidebar a', function(e){
          e.preventDefault();
          $('.sidebar a').removeClass('active');
          $(this).addClass('active');
          var tab = $(this).data('tab');
          $('.nav-tabs a[data-value=' + tab + ']').tab('show');
        });

        // Forzar tab inicial al cargar
        $(function(){
          $('.nav-tabs a[data-value=\"intro\"]').tab('show');
        });
      "))
    ),
    
    mainPanel(
      width = 9,
      div(
        class="content-wrap",
        tabsetPanel(
          id = "tabs_main",
          selected = "intro",
          
          # =========================
          # I. PRESENTACIÓN
          # =========================
          tabPanel(
            title = "I. Presentación",
            value = "intro",
            div(
              class="box",
              div(class="box-header", "I. Introducción"),
              div(
                class="box-body",
                tags$div(
                  style = "
                    background:#ffffff;
                    border:1px solid #d9e6f5;
                    border-radius:10px;
                    padding:40px 60px;
                    font-family: Arial, Helvetica, sans-serif;
                    font-size:22px;
                    line-height:1.8;
                  ",
                  tags$div(
                    style = "font-size:40px; font-weight:400; margin-bottom:14px; color:#1F3A5F;",
                    HTML("VirtuaLAB XXI <span style='font-weight:400;'>(Alpha)</span>")
                  ),
                  tags$div(
                    style = "font-size:26px; margin-bottom:24px; color:#4682B4;",
                    "Laboratorio virtual para el estudio de la estadística."
                  ),
                  tags$hr(style = "margin:20px 0 26px 0; border-top:1px solid #e6f0ff;"),
                  tags$div(
                    style = "font-size:22px; color:#4682B4;",
                    HTML(
                      "Desarrollado por <b>Verónica del Carmen Quijada Monroy</b> y <b>Víctor Manuel Ulloa</b> como parte del <b>Proyecto PE14226</b>, dentro del <b>Programa de Apoyo a Proyectos para Innovar y Mejorar la Educación</b>, de la <b>Universidad Nacional Autónoma de México</b>."
                    )
                  )
                )
              )
            )
          ),
          
          # =========================
          # II. POBLACIÓN
          # =========================
          tabPanel(
            title = "II. Población",
            value = "poblacion",
            div(
              class="box",
              div(class="box-header", "II. Población"),
              div(class="box-body",
                  plotOutput("poblacionPlot", height = "520px")
              )
            )
          ),
          
          # =========================
          # III. VARIABLE CUALITATIVA
          # =========================
          tabPanel(
            title = "III. Variable cualitativa",
            value = "var_cuali",
            div(
              class="box",
              div(class="box-header", "III. Variable cualitativa"),
              div(
                class="box-body",
                fluidRow(
                  column(6, plotOutput("poblacionPlot_cuali", height = "520px")),
                  column(6, plotOutput("donaPob_cuali", height = "520px"))
                ),
                uiOutput("tablaProps_cuali")
              )
            )
          ),
          
          # =========================
          # IV. VARIABLE CUANTITATIVA
          # =========================
          tabPanel(
            title = "IV. Variable cuantitativa",
            value = "var_cuanti",
            div(
              class="box",
              div(class="box-header", "IV. Variable cuantitativa"),
              div(
                class="box-body",
                fluidRow(
                  column(6, plotOutput("poblacionPlot_cuanti", height = "520px")),
                  column(6, plotOutput("histPob_cuanti", height = "520px"))
                ),
                uiOutput("tablaMediaVar_cuanti")
              )
            )
          ),
          
          # =========================
          # V. MARCO MUESTRAL
          # =========================
          tabPanel(
            title = "V. Marco Muestral",
            value = "marco",
            div(
              class="box",
              div(class="box-header", "V. Marco Muestral"),
              div(
                class="box-body",
                fluidRow(
                  column(
                    4,
                    h4("Explorar registros"),
                    tags$div(
                      style="font-size:13px; color:#555; margin-top:6px;",
                      "Deslice verticalmente para recorrer el marco muestral."
                    ),
                    hr(),
                    actionButton("desc_poblacion_marco", "Descargar población (CSV)", class = "btn-block")
                  ),
                  column(
                    8,
                    div(
                      class = "sample-scroll",
                      tableOutput("tabla_marco")
                    )
                  )
                )
              )
            )
          ),
          
          # =========================
          # VI. NOCIÓN DE MUESTREO
          # =========================
          tabPanel(
            title = "VI. Noción de muestreo",
            value = "nocion",
            div(
              class="box",
              div(class="box-header", "VI. Noción de muestreo"),
              div(
                class="box-body",
                sidebarLayout(
                  sidebarPanel(
                    h4("Tipo de variable"),
                    radioButtons(
                      "tipo_var_vi",
                      label = NULL,
                      choices = c("Cualitativa (color)" = "cuali", "Cuantitativa (diámetro)" = "cuanti"),
                      selected = "cuali"
                    ),
                    hr(),
                    
                    conditionalPanel(
                      condition = "input.tipo_var_vi == 'cuali'",
                      h4("Muestreo"),
                      sliderInput("n_muestra_cuali_vi", "Tamaño de muestra (n):",
                                  min = 10, max = 2000, value = 10, step = 10),
                      actionButton("nueva_muestra_cuali_vi", "Nueva muestra")
                    ),
                    
                    conditionalPanel(
                      condition = "input.tipo_var_vi == 'cuanti'",
                      h4("Muestreo"),
                      sliderInput("n_muestra_cuanti_vi", "Tamaño de muestra (n):",
                                  min = 10, max = 2000, value = 10, step = 10),
                      actionButton("nueva_muestra_cuanti_vi", "Nueva muestra")
                    )
                  ),
                  
                  mainPanel(
                    conditionalPanel(
                      condition = "input.tipo_var_vi == 'cuali'",
                      fluidRow(
                        column(6, plotOutput("poblacionPlot_cuali_vi", height = "520px")),
                        column(6, plotOutput("muestraPlot_cuali_vi", height = "520px"))
                      ),
                      hr(),
                      fluidRow(
                        column(6, plotOutput("donaPob_cuali_vi", height = "320px")),
                        column(6, plotOutput("donaMue_cuali_vi", height = "320px"))
                      ),
                      uiOutput("tablaProps_cuali_vi")
                    ),
                    
                    conditionalPanel(
                      condition = "input.tipo_var_vi == 'cuanti'",
                      fluidRow(
                        column(6, plotOutput("poblacionPlot_cuanti_vi", height = "520px")),
                        column(6, plotOutput("muestraPlot_cuanti_vi", height = "520px"))
                      ),
                      hr(),
                      fluidRow(
                        column(6, plotOutput("histPob_cuanti_vi", height = "320px")),
                        column(6, plotOutput("histMue_cuanti_vi", height = "320px"))
                      ),
                      uiOutput("tablaMediaVar_cuanti_vi")
                    )
                  )
                )
              )
            )
          ),
          
          # =========================
          # VII. MUESTREO ALEATORIO
          # =========================
          tabPanel(
            title = "VII. Muestreo aleatorio",
            value = "muestreo_vii",
            div(
              class="box",
              div(class="box-header", "VII. Muestreo aleatorio"),
              div(
                class="box-body",
                sidebarLayout(
                  sidebarPanel(
                    h4("Tipo de variable"),
                    radioButtons(
                      "tipo_var_vii",
                      label = NULL,
                      choices = c("Cualitativa (color)" = "cuali", "Cuantitativa (diámetro)" = "cuanti"),
                      selected = "cuali"
                    ),
                    hr(),
                    
                    h4("Parámetros de diseño"),
                    sliderInput("conf_vii", "Nivel de confianza (%)",
                                min = 10, max = 99, value = 10, step = 1),
                    sliderInput("err_vii", "Margen de error (%)",
                                min = 1, max = 20, value = 5, step = 1),
                    
                    conditionalPanel(
                      condition = "input.tipo_var_vii == 'cuanti'",
                      sliderInput("var_pob_vii", "Varianza (población) usada en el cálculo",
                                  min = 0.10, max = 50, value = 1, step = 0.10)
                    ),
                    
                    uiOutput("vii_formula"),
                    uiOutput("vii_valores"),
                    
                    hr(),
                    actionButton("vii_nueva_muestra", "Nueva muestra", class = "btn-primary btn-block"),
                    actionButton("vii_descargar_csv", "Descargar muestra (CSV)", class = "btn-block")
                  ),
                  
                  mainPanel(
                    conditionalPanel(
                      condition = "input.tipo_var_vii == 'cuali'",
                      fluidRow(
                        column(6, plotOutput("poblacionPlot_cuali_vii", height = "520px")),
                        column(6, plotOutput("muestraPlot_cuali_vii", height = "520px"))
                      ),
                      hr(),
                      fluidRow(
                        column(6, plotOutput("donaPob_cuali_vii", height = "320px")),
                        column(6, plotOutput("donaMue_cuali_vii", height = "320px"))
                      ),
                      uiOutput("tablaProps_cuali_vii"),
                      hr(),
                      h4("Dataframe de la muestra (ID + Color)"),
                      div(class = "sample-scroll", tableOutput("tabla_muestra_vii_cuali"))
                    ),
                    
                    conditionalPanel(
                      condition = "input.tipo_var_vii == 'cuanti'",
                      fluidRow(
                        column(6, plotOutput("poblacionPlot_cuanti_vii", height = "520px")),
                        column(6, plotOutput("muestraPlot_cuanti_vii", height = "520px"))
                      ),
                      hr(),
                      fluidRow(
                        column(6, plotOutput("histPob_cuanti_vii", height = "320px")),
                        column(6, plotOutput("histMue_cuanti_vii", height = "320px"))
                      ),
                      uiOutput("tablaMediaVar_cuanti_vii"),
                      hr(),
                      h4("Dataframe de la muestra (ID + Diámetro)"),
                      div(class = "sample-scroll", tableOutput("tabla_muestra_vii_cuanti"))
                    )
                  )
                )
              )
            )
          ),
          
          # =========================
          # VIII. ANÁLISIS DESCRIPTIVO E INFERENCIA (solo muestra)
          # =========================
          tabPanel(
            title = "VIII. Análisis descriptivo e Inferencia",
            value = "inferencia_viii",
            div(
              class="box",
              div(class="box-header", "VIII. Análisis descriptivo e Inferencia"),
              div(
                class="box-body",
                sidebarLayout(
                  sidebarPanel(
                    h4("Tipo de variable"),
                    radioButtons(
                      "tipo_var_viii",
                      label = NULL,
                      choices = c("Cualitativa (color)" = "cuali", "Cuantitativa (diámetro)" = "cuanti"),
                      selected = "cuali"
                    ),
                    hr(),
                    
                    h4("Parámetros de diseño"),
                    sliderInput("conf_viii", "Nivel de confianza (%)",
                                min = 10, max = 99, value = 10, step = 1),
                    sliderInput("err_viii", "Margen de error (%)",
                                min = 1, max = 20, value = 5, step = 1),
                    
                    conditionalPanel(
                      condition = "input.tipo_var_viii == 'cuanti'",
                      sliderInput("var_pob_viii", "Varianza (población) usada en el cálculo",
                                  min = 0.10, max = 50, value = 1, step = 0.10)
                    ),
                    
                    uiOutput("viii_formula"),
                    uiOutput("viii_valores"),
                    
                    hr(),
                    actionButton("viii_nueva_muestra", "Nueva muestra", class = "btn-primary btn-block")
                  ),
                  
                  mainPanel(
                    conditionalPanel(
                      condition = "input.tipo_var_viii == 'cuali'",
                      
                      fluidRow(
                        column(6, plotOutput("muestraPlot_cuali_viii", height = "260px")),
                        column(6, plotOutput("donaMue_cuali_viii", height = "260px"))
                      ),
                      
                      hr(),
                      uiOutput("viii_formulas_stats"),
                      uiOutput("viii_resultados_cuali"),
                      uiOutput("viii_ic_cuali"),
                      hr(),
                      h4("Dataframe de la muestra (ID + Color)"),
                      div(class = "sample-scroll", tableOutput("tabla_muestra_viii_cuali"))
                    ),
                    
                    conditionalPanel(
                      condition = "input.tipo_var_viii == 'cuanti'",
                      
                      fluidRow(
                        column(6, plotOutput("muestraPlot_cuanti_viii", height = "260px")),
                        column(6, plotOutput("histMue_cuanti_viii", height = "260px"))
                      ),
                      
                      hr(),
                      uiOutput("viii_formulas_stats"),
                      uiOutput("viii_resultados_cuanti"),
                      uiOutput("viii_ic_cuanti"),
                      hr(),
                      h4("Dataframe de la muestra (ID + Diámetro)"),
                      div(class = "sample-scroll", tableOutput("tabla_muestra_viii_cuanti"))
                    )
                  )
                )
              )
            )
          ),
          # =========================
          # IX. INTERPRETACIÓN DE LA INFERENCIA
          # =========================
          tabPanel(
            title = "IX. Interpretación de la inferencia",
            value = "interpretacion_ix",
            div(
              class="box",
              div(class="box-header", "IX. Interpretación de la inferencia"),
              div(
                class="box-body",
                sidebarLayout(
                  sidebarPanel(
                    h4("Parámetro poblacional"),
                    radioButtons(
                      "ix_param",
                      label = NULL,
                      choices = c("Proporción (color)" = "prop", "Media (diámetro)" = "media"),
                      selected = "prop"
                    ),
                    
                    conditionalPanel(
                      condition = "input.ix_param == 'prop'",
                      selectInput(
                        "ix_color_obj",
                        "Color de interés (éxito):",
                        choices = c("Azul", "Naranja", "Verde"),
                        selected = "Azul"
                      )
                    ),
                    
                    hr(),
                    h4("Diseño del intervalo"),
                    sliderInput(
                      "ix_conf",
                      "Nivel de confianza (1 − α)",
                      min = 0.10, max = 0.99, value = 0.10, step = 0.01
                    ),
                    
                    conditionalPanel(
                      condition = "input.ix_param == 'prop'",
                      sliderInput(
                        "ix_E_prop",
                        "Margen de error (E)",
                        min = 0.01, max = 0.20, value = 0.05, step = 0.01
                      )
                    ),
                    conditionalPanel(
                      condition = "input.ix_param == 'media'",
                      sliderInput(
                        "ix_E_med",
                        "Margen de error (E)",
                        min = 0.10, max = 5.00, value = 1.00, step = 0.10
                      )
                    ),
                    
                    hr(),
                    h4("Tamaño de muestra (población finita)"),
                    uiOutput("ix_formula"),
                    uiOutput("ix_sustitucion"),
                    uiOutput("ix_n_txt"),
                    
                    hr(),
                    h4("Experimento de cobertura"),
                    sliderInput(
                      "ix_B",
                      "Número de intervalos (B)",
                      min = 50, max = 500, value = 100, step = 50
                    ),
                    
                    actionButton("ix_recalcular", "Repetir experimento", class = "btn-primary btn-block")
                  ),
                  
                  mainPanel(
                    plotOutput("ix_plot_ic", height = "420px"),
                    uiOutput("ix_resumen"),
                    
                    div(
                      class = "box",
                      div(class = "box-header", "Interpretación del experimento"),
                      div(
                        class = "box-body",
                        tags$ul(
                          tags$li(HTML("<b>Cada barra</b> representa un intervalo de confianza construido con una muestra distinta.")),
                          tags$li(HTML("<b>Verde</b>: el intervalo <b>sí</b> contiene al parámetro poblacional.")),
                          tags$li(HTML("<b>Rojo</b>: el intervalo <b>no</b> contiene al parámetro poblacional."))
                        ),
                        uiOutput("ix_pct_captura")
                      )
                    )
                  )
                )
              )
            )
          ),
          # =========================
          # X. GLOSARIO Y SIMBOLOGÍA
          # =========================
          tabPanel(
            title = "X. Glosario y simbología",
            value = "glosario_x",
            div(
              class="box",
              div(class="box-header", "X. Glosario y simbología"),
              div(
                class="box-body",
                uiOutput("glosario_ui")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ============================================================
  # POBLACIÓN ÚNICA (2000 bolas)
  # ============================================================
  N <- 2000
  n_col <- 40
  n_row <- ceiling(N / n_col)
  
  coords <- expand.grid(col = 1:n_col, row = 1:n_row)
  coords <- coords[1:N, ]
  
  cols <- c(
    "Azul"    = "#1f77b4",
    "Naranja" = "#ff7f0e",
    "Verde"   = "#2ca02c"
  )
  
  w <- runif(3, min = 0.2, max = 1.2)
  probs <- w / sum(w)
  
  colores <- sample(
    x = names(cols),
    size = N,
    replace = TRUE,
    prob = probs
  )
  
  diam_raw <- rnorm(N, mean = 1.0, sd = 0.25)
  diam_raw[diam_raw < 0.4] <- 0.4
  diam_raw[diam_raw > 1.8] <- 1.8
  
  poblacion <- data.frame(
    id    = 1:N,
    x     = coords$col,
    y     = -coords$row,
    color = factor(colores, levels = names(cols)),
    diam  = diam_raw
  )
  
  # Evita depender de scales::rescale
  rescale01 <- function(x) {
    r <- range(x, finite = TRUE)
    if (r[2] - r[1] == 0) return(rep(0.5, length(x)))
    (x - r[1]) / (r[2] - r[1])
  }
  poblacion$size_pt <- 1.6 + rescale01(poblacion$diam) * (4.2 - 1.6)
  
  set.seed(999)
  sigma_med <- 0.55
  diam_did <- poblacion$diam + rnorm(N, mean = 0, sd = sigma_med)
  diam_did <- pmin(pmax(diam_did, 0.2), 2.4)
  poblacion$diam_did <- diam_did
  
  xlim_all <- c(0.5, n_col + 0.5)
  ylim_all <- c(-n_row - 0.5, -0.5)
  
  ESCALA <- 10
  
  # =========================
  # HELPERS
  # =========================
  plot_poblacion_bolas <- function(titulo) {
    ggplot(poblacion, aes(x, y)) +
      geom_point(
        aes(fill = color, size = size_pt),
        shape = 21,
        color = "gray25",
        stroke = 0.15,
        alpha = 0.85
      ) +
      scale_fill_manual(values = cols) +
      scale_size_identity() +
      coord_equal(xlim = xlim_all, ylim = ylim_all, expand = FALSE) +
      labs(title = titulo) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  }
  
  donut_plot <- function(df_props, col_value_name, titulo = NULL) {
    df <- data.frame(
      categoria = df_props$categoria,
      valor = df_props[[col_value_name]],
      stringsAsFactors = FALSE
    )
    df$frac <- df$valor / sum(df$valor)
    df$ymax <- cumsum(df$frac)
    df$ymin <- c(0, head(df$ymax, -1))
    
    p <- ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = categoria)) +
      geom_rect(color = "white", linewidth = 0.6) +
      coord_polar(theta = "y") +
      xlim(0, 4.5) +
      scale_fill_manual(values = cols) +
      theme_void(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    if (!is.null(titulo)) p <- p + labs(title = titulo)
    p
  }
  
  hist_con_normal <- function(x, titulo) {
    mu <- mean(x)
    sdv <- sd(x)
    ggplot(data.frame(val = x), aes(val)) +
      geom_histogram(aes(y = ..density..),
                     bins = 18,
                     fill = "#1f77b4",
                     color = "white",
                     alpha = 0.75) +
      stat_function(fun = dnorm,
                    args = list(mean = mu, sd = sdv),
                    color = "red",
                    linewidth = 1.2) +
      labs(title = titulo, x = "Diámetro", y = "Frecuencia-Densidad") +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5))
  }
  
  fpc <- function(N, n) {
    if (n >= N) return(0)
    sqrt((N - n) / (N - 1))
  }
  
  skewness_simple <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3) return(NA_real_)
    m <- mean(x); s <- sd(x)
    if (s == 0) return(0)
    mean((x - m)^3) / (s^3)
  }
  
  kurtosis_excess_simple <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 4) return(NA_real_)
    m <- mean(x); s <- sd(x)
    if (s == 0) return(0)
    mean((x - m)^4) / (s^4) - 3
  }
  
  # =========================
  # II. POBLACIÓN
  # =========================
  output$poblacionPlot <- renderPlot({
    plot_poblacion_bolas(paste0("Población (N = ", N, ")"))
  })
  
  # =========================
  # III. VARIABLE CUALITATIVA
  # =========================
  output$poblacionPlot_cuali <- renderPlot({
    plot_poblacion_bolas(paste0("Población (N = ", N, ")"))
  })
  
  props_cuali_df <- reactive({
    pob_tab <- prop.table(table(poblacion$color))
    colores_all <- levels(poblacion$color)
    pob_tab <- pob_tab[colores_all]
    data.frame(
      categoria = colores_all,
      proporcion = as.numeric(pob_tab),
      stringsAsFactors = FALSE
    )
  })
  
  output$donaPob_cuali <- renderPlot({
    df <- props_cuali_df()
    donut_plot(df, "proporcion", "Proporción poblacional (por color)")
  })
  
  output$tablaProps_cuali <- renderUI({
    df <- props_cuali_df()
    tags$div(
      class = "result-box",
      tags$h4(
        HTML(
          "Variable <span style='font-family:serif; font-weight:700; font-style:italic;'>X</span> = Color | Parámetro investigado <span style='font-family:serif; font-weight:700;'>&theta;</span> = <span style='font-family:serif; font-weight:700;'>&pi;</span> (Proporción poblacional)"
        ),
        style = "margin-top:0;"
      ),
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(
          tags$tr(
            tags$th("Color", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
            tags$th(
              HTML("<span style='font-family:serif; font-weight:700;'>&pi;</span> (Proporción)"),
              style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"
            )
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(df$categoria[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
              tags$td(sprintf("%.3f", df$proporcion[i]),
                      style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
            )
          })
        )
      )
    )
  })
  
  # =========================
  # IV. VARIABLE CUANTITATIVA
  # =========================
  output$poblacionPlot_cuanti <- renderPlot({
    plot_poblacion_bolas(paste0("Población (N = ", N, ")"))
  })
  
  diam_calc_pob <- reactive({
    poblacion$diam_did * ESCALA
  })
  
  output$histPob_cuanti <- renderPlot({
    hist_con_normal(diam_calc_pob(), "Población: histograma del diámetro")
  })
  
  output$tablaMediaVar_cuanti <- renderUI({
    x_p <- diam_calc_pob()
    
    df <- data.frame(
      Estadístico = c(
        HTML("<span style='font-family:serif; font-weight:bold;'>&mu;</span> (Media)"),
        HTML("<span style='font-family:serif; font-weight:bold;'>&sigma;<sup>2</sup></span> (Varianza)")
      ),
      Valor = c(mean(x_p), var(x_p)),
      stringsAsFactors = FALSE
    )
    
    tags$div(
      class = "result-box",
      tags$h4(
        HTML("Variable <span style='font-family:serif; font-weight:bold; font-style:italic;'>X</span> = Diámetro |
        Parámetro investigado
        <span style='font-family:serif; font-weight:bold;'>&theta;</span> =
        <span style='font-family:serif; font-weight:bold;'>&mu;</span> (Media poblacional)"),
        style="margin-top:0;"
      ),
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(
          tags$tr(
            tags$th("Parámetros", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
            tags$th("Valor", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(HTML(df$Estadístico[i]),
                      style="padding:6px; border-bottom:1px solid #e6f0ff;"),
              tags$td(sprintf("%.3f", df$Valor[i]),
                      style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
            )
          })
        )
      )
    )
  })
  
  # =========================
  # V. MARCO MUESTRAL + DESCARGA CSV
  # =========================
  marco_df <- reactive({
    data.frame(
      ID = as.integer(poblacion$id),
      Color = as.character(poblacion$color),
      Diametro = round(poblacion$diam_did * ESCALA, 3),
      stringsAsFactors = FALSE
    )
  })
  
  output$tabla_marco <- renderTable({
    marco_df()
  }, striped = TRUE, bordered = TRUE, spacing = "xs", rownames = FALSE, align = "c")
  
  observeEvent(input$desc_poblacion_marco, {
    df <- marco_df()
    csv_txt <- paste(capture.output(write.csv(df, row.names = FALSE, fileEncoding = "UTF-8")), collapse = "\n")
    
    session$sendCustomMessage("download_text_file", list(
      filename = paste0("poblacion_", Sys.Date(), ".csv"),
      content  = csv_txt,
      mime     = "text/csv;charset=utf-8"
    ))
  })
  
  # ==========================================================
  # VI. NOCIÓN DE MUESTREO (tal cual)
  # ==========================================================
  sample_ids_cuali_vi <- reactiveVal(integer(0))
  tomar_muestra_cuali_vi <- function() {
    n <- min(as.integer(input$n_muestra_cuali_vi), N)
    sample_ids_cuali_vi(sample(poblacion$id, size = n, replace = FALSE))
  }
  observeEvent(TRUE, { tomar_muestra_cuali_vi() }, once = TRUE)
  observeEvent(input$n_muestra_cuali_vi, { tomar_muestra_cuali_vi() }, ignoreInit = TRUE)
  observeEvent(input$nueva_muestra_cuali_vi, { tomar_muestra_cuali_vi() }, ignoreInit = TRUE)
  
  muestra_cuali_vi <- reactive({
    ids <- sample_ids_cuali_vi()
    req(length(ids) > 0)
    poblacion[match(ids, poblacion$id), , drop = FALSE]
  })
  
  output$poblacionPlot_cuali_vi <- renderPlot({
    plot_poblacion_bolas(paste0("Población (N = ", N, ")"))
  })
  
  output$muestraPlot_cuali_vi <- renderPlot({
    m <- muestra_cuali_vi()
    req(nrow(m) > 0)
    
    ggplot() +
      geom_point(
        data = poblacion, aes(x, y),
        shape = 21, size = 2.2, fill = "white",
        color = "gray80", stroke = 0.12, alpha = 0.20
      ) +
      geom_point(
        data = m, aes(x, y, fill = color, size = size_pt),
        shape = 21, color = "gray15", stroke = 0.20, alpha = 0.98
      ) +
      scale_fill_manual(values = cols) +
      scale_size_identity() +
      coord_equal(xlim = xlim_all, ylim = ylim_all, expand = FALSE) +
      labs(title = paste0("Muestra (n = ", nrow(m), ")")) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  })
  
  props_cuali_vi_df <- reactive({
    m <- muestra_cuali_vi()
    req(nrow(m) > 0)
    
    pob_tab <- prop.table(table(poblacion$color))
    mue_tab <- prop.table(table(m$color))
    
    colores_all <- levels(poblacion$color)
    pob_tab <- pob_tab[colores_all]
    mue_tab <- mue_tab[colores_all]
    mue_tab[is.na(mue_tab)] <- 0
    
    data.frame(
      categoria = colores_all,
      poblacion = as.numeric(pob_tab),
      muestra = as.numeric(mue_tab),
      stringsAsFactors = FALSE
    )
  })
  
  output$donaPob_cuali_vi <- renderPlot({
    df <- props_cuali_vi_df()
    donut_plot(data.frame(categoria = df$categoria, poblacion = df$poblacion), "poblacion",
               "Proporción poblacional (por color)")
  })
  
  output$donaMue_cuali_vi <- renderPlot({
    df <- props_cuali_vi_df()
    donut_plot(data.frame(categoria = df$categoria, muestra = df$muestra), "muestra",
               "Proporción muestral (por color)")
  })
  
  output$tablaProps_cuali_vi <- renderUI({
    df <- props_cuali_vi_df()
    tags$div(
      class = "result-box",
      tags$h4("Comparación: proporción poblacional vs proporción muestral", style="margin-top:0;"),
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(
          tags$tr(
            tags$th("Categoría (color)", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
            tags$th("Población", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
            tags$th("Muestra", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(df$categoria[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
              tags$td(sprintf("%.3f", df$poblacion[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
              tags$td(sprintf("%.3f", df$muestra[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
            )
          })
        )
      )
    )
  })
  
  sample_ids_cuanti_vi <- reactiveVal(integer(0))
  tomar_muestra_cuanti_vi <- function() {
    n <- min(as.integer(input$n_muestra_cuanti_vi), N)
    sample_ids_cuanti_vi(sample(poblacion$id, size = n, replace = FALSE))
  }
  observeEvent(TRUE, { tomar_muestra_cuanti_vi() }, once = TRUE)
  observeEvent(input$n_muestra_cuanti_vi, { tomar_muestra_cuanti_vi() }, ignoreInit = TRUE)
  observeEvent(input$nueva_muestra_cuanti_vi, { tomar_muestra_cuanti_vi() }, ignoreInit = TRUE)
  
  muestra_cuanti_vi <- reactive({
    ids <- sample_ids_cuanti_vi()
    req(length(ids) > 0)
    poblacion[match(ids, poblacion$id), , drop = FALSE]
  })
  
  output$poblacionPlot_cuanti_vi <- renderPlot({
    plot_poblacion_bolas(paste0("Población (N = ", N, ")"))
  })
  
  output$muestraPlot_cuanti_vi <- renderPlot({
    m <- muestra_cuanti_vi()
    req(nrow(m) > 0)
    
    ggplot() +
      geom_point(
        data = poblacion, aes(x, y),
        shape = 21, size = 2.2, fill = "white",
        color = "gray80", stroke = 0.12, alpha = 0.20
      ) +
      geom_point(
        data = m, aes(x, y, fill = color, size = size_pt),
        shape = 21, color = "gold", stroke = 0.45, alpha = 0.98
      ) +
      scale_fill_manual(values = cols) +
      scale_size_identity() +
      coord_equal(xlim = xlim_all, ylim = ylim_all, expand = FALSE) +
      labs(title = paste0("Muestra (n = ", nrow(m), ")")) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  })
  
  diam_calc_pob_vi <- reactive({ poblacion$diam_did * ESCALA })
  diam_calc_mue_vi <- reactive({
    m <- muestra_cuanti_vi()
    req(nrow(m) > 0)
    m$diam_did * ESCALA
  })
  
  output$histPob_cuanti_vi <- renderPlot({
    hist_con_normal(diam_calc_pob_vi(), "Población: histograma del diámetro")
  })
  
  output$histMue_cuanti_vi <- renderPlot({
    hist_con_normal(diam_calc_mue_vi(), "Muestra: histograma del diámetro")
  })
  
  output$tablaMediaVar_cuanti_vi <- renderUI({
    x_p <- diam_calc_pob_vi()
    x_m <- diam_calc_mue_vi()
    
    df <- data.frame(
      Estadístico = c("Media", "Varianza"),
      Población = c(mean(x_p), var(x_p)),
      Muestra = c(mean(x_m), var(x_m)),
      stringsAsFactors = FALSE
    )
    
    tags$div(
      class = "result-box",
      tags$h4("Comparación: media y varianza del diámetro", style="margin-top:0;"),
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(
          tags$tr(
            tags$th("Medidas centrales y de dispersión", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
            tags$th("Población", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
            tags$th("Muestra", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(df$Estadístico[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
              tags$td(sprintf("%.3f", df$Población[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
              tags$td(sprintf("%.3f", df$Muestra[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
            )
          })
        )
      )
    )
  })
  
  # ==========================================================
  # VII. MUESTREO ALEATORIO (tal cual ya lo tenías)
  # ==========================================================
  observe({
    req(!is.null(input$tipo_var_vii))
    if (input$tipo_var_vii == "cuanti") {
      v0 <- var(diam_calc_pob())
      updateSliderInput(
        session, "var_pob_vii",
        min = max(0.10, v0 * 0.20),
        max = max(0.50, v0 * 3.00),
        value = v0,
        step = 0.10
      )
    }
  })
  
  z_vii <- reactive({
    conf <- input$conf_vii / 100
    qnorm(1 - (1 - conf) / 2)
  })
  
  n_vii <- reactive({
    Z <- z_vii()
    Nn <- N
    
    if (input$tipo_var_vii == "cuali") {
      E <- input$err_vii / 100
      p <- 0.5; q <- 0.5
      num <- Nn * (Z^2) * p * q
      den <- (Nn - 1) * (E^2) + (Z^2) * p * q
      n <- ceiling(num / den)
    } else {
      S2 <- input$var_pob_vii
      mu_p <- mean(diam_calc_pob())
      E_abs <- mu_p * (input$err_vii / 100)
      num <- Nn * (Z^2) * S2
      den <- (Nn - 1) * (E_abs^2) + (Z^2) * S2
      n <- ceiling(num / den)
    }
    
    max(10, min(n, Nn))
  })
  
  output$vii_formula <- renderUI({
    if (input$tipo_var_vii == "cuali") {
      withMathJax(
        tags$div(class = "formula-box",
                 tags$h4("Fórmula (población finita)"),
                 HTML("$$n = \\frac{N\\,Z^2\\,p\\,q}{(N-1)\\,E^2 + Z^2\\,p\\,q}$$"))
      )
    } else {
      withMathJax(
        tags$div(class = "formula-box",
                 tags$h4("Fórmula (población finita)"),
                 HTML("$$n = \\frac{N\\,Z^2\\,S^2}{(N-1)\\,E^2 + Z^2\\,S^2}$$"))
      )
    }
  })
  
  output$vii_valores <- renderUI({
    Z <- z_vii()
    Nn <- N
    
    if (input$tipo_var_vii == "cuali") {
      p <- 0.5; q <- 0.5
      E <- input$err_vii / 100
      tags$div(class = "formula-box",
               tags$h4("Valores sustituidos"),
               tags$ul(
                 tags$li(HTML(paste0("<b>N</b> = ", Nn))),
                 tags$li(HTML(paste0("<b>Z</b> = ", sprintf("%.3f", Z), " (", input$conf_vii, "%)"))),
                 tags$li(HTML(paste0("<b>p</b> = ", sprintf("%.2f", p), " ; <b>q</b> = ", sprintf("%.2f", q)))),
                 tags$li(HTML(paste0("<b>E</b> = ", sprintf("%.3f", E))))
               ),
               tags$hr(style="margin:8px 0;"),
               tags$h4(HTML(paste0("Tamaño de muestra calculado: <b>n = ", n_vii(), "</b>")))
      )
    } else {
      S2 <- input$var_pob_vii
      mu_p <- mean(diam_calc_pob())
      E_abs <- mu_p * (input$err_vii / 100)
      tags$div(class = "formula-box",
               tags$h4("Valores sustituidos"),
               tags$ul(
                 tags$li(HTML(paste0("<b>N</b> = ", Nn))),
                 tags$li(HTML(paste0("<b>Z</b> = ", sprintf("%.3f", Z), " (", input$conf_vii, "%)"))),
                 tags$li(HTML(paste0("<b>S²</b> = ", sprintf("%.3f", S2)))),
                 tags$li(HTML(paste0("<b>E</b> = ", sprintf("%.3f", E_abs))))
               ),
               tags$hr(style="margin:8px 0;"),
               tags$h4(HTML(paste0("Tamaño de muestra calculado: <b>n = ", n_vii(), "</b>")))
      )
    }
  })
  
  sample_ids_vii <- reactiveVal(integer(0))
  tomar_muestra_vii <- function() {
    n <- min(n_vii(), N)
    sample_ids_vii(sample(poblacion$id, size = n, replace = FALSE))
  }
  observeEvent(TRUE, { tomar_muestra_vii() }, once = TRUE)
  
  observeEvent(
    c(input$tipo_var_vii, input$conf_vii, input$err_vii, input$var_pob_vii),
    { tomar_muestra_vii() },
    ignoreInit = TRUE
  )
  observeEvent(input$vii_nueva_muestra, { tomar_muestra_vii() }, ignoreInit = TRUE)
  
  muestra_vii <- reactive({
    ids <- sample_ids_vii()
    req(length(ids) > 0)
    poblacion[match(ids, poblacion$id), , drop = FALSE]
  })
  
  output$poblacionPlot_cuali_vii <- renderPlot({ plot_poblacion_bolas(paste0("Población (N = ", N, ")")) })
  output$muestraPlot_cuali_vii <- renderPlot({
    m <- muestra_vii(); req(nrow(m) > 0)
    ggplot() +
      geom_point(data = poblacion, aes(x, y), shape = 21, size = 2.2, fill = "white",
                 color = "gray80", stroke = 0.12, alpha = 0.20) +
      geom_point(data = m, aes(x, y, fill = color, size = size_pt), shape = 21,
                 color = "gray15", stroke = 0.20, alpha = 0.98) +
      scale_fill_manual(values = cols) + scale_size_identity() +
      coord_equal(xlim = xlim_all, ylim = ylim_all, expand = FALSE) +
      labs(title = paste0("Muestra (n = ", nrow(m), ")")) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none")
  })
  
  props_cuali_vii_df <- reactive({
    m <- muestra_vii(); req(nrow(m) > 0)
    pob_tab <- prop.table(table(poblacion$color))
    mue_tab <- prop.table(table(m$color))
    colores_all <- levels(poblacion$color)
    pob_tab <- pob_tab[colores_all]
    mue_tab <- mue_tab[colores_all]
    mue_tab[is.na(mue_tab)] <- 0
    data.frame(categoria = colores_all, poblacion = as.numeric(pob_tab), muestra = as.numeric(mue_tab),
               stringsAsFactors = FALSE)
  })
  
  output$donaPob_cuali_vii <- renderPlot({
    df <- props_cuali_vii_df()
    donut_plot(data.frame(categoria=df$categoria, poblacion=df$poblacion), "poblacion",
               "Proporción poblacional (por color)")
  })
  output$donaMue_cuali_vii <- renderPlot({
    df <- props_cuali_vii_df()
    donut_plot(data.frame(categoria=df$categoria, muestra=df$muestra), "muestra",
               "Proporción muestral (por color)")
  })
  output$tablaProps_cuali_vii <- renderUI({
    df <- props_cuali_vii_df()
    tags$div(
      class = "result-box",
      tags$h4("Comparación: proporción poblacional vs proporción muestral", style="margin-top:0;"),
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("Categoría (color)", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("Población", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("Muestra", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
        )),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$categoria[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$poblacion[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$muestra[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
          )
        }))
      )
    )
  })
  output$tabla_muestra_vii_cuali <- renderTable({
    m <- muestra_vii(); req(nrow(m) > 0)
    data.frame(ID = as.integer(m$id), Color = as.character(m$color), stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, spacing = "xs", rownames = FALSE, align = "c")
  
  output$poblacionPlot_cuanti_vii <- renderPlot({ plot_poblacion_bolas(paste0("Población (N = ", N, ")")) })
  output$muestraPlot_cuanti_vii <- renderPlot({
    m <- muestra_vii(); req(nrow(m) > 0)
    ggplot() +
      geom_point(data = poblacion, aes(x, y), shape = 21, size = 2.2, fill = "white",
                 color = "gray80", stroke = 0.12, alpha = 0.20) +
      geom_point(data = m, aes(x, y, fill = color, size = size_pt), shape = 21,
                 color = "gold", stroke = 0.45, alpha = 0.98) +
      scale_fill_manual(values = cols) + scale_size_identity() +
      coord_equal(xlim = xlim_all, ylim = ylim_all, expand = FALSE) +
      labs(title = paste0("Muestra (n = ", nrow(m), ")")) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none")
  })
  
  diam_calc_pob_vii <- reactive({ poblacion$diam_did * ESCALA })
  diam_calc_mue_vii <- reactive({
    m <- muestra_vii(); req(nrow(m) > 0)
    m$diam_did * ESCALA
  })
  output$histPob_cuanti_vii <- renderPlot({ hist_con_normal(diam_calc_pob_vii(), "Población: histograma del diámetro") })
  output$histMue_cuanti_vii <- renderPlot({ hist_con_normal(diam_calc_mue_vii(), "Muestra: histograma del diámetro") })
  
  output$tablaMediaVar_cuanti_vii <- renderUI({
    x_p <- diam_calc_pob_vii()
    x_m <- diam_calc_mue_vii()
    df <- data.frame(
      Estadístico = c("Media", "Varianza"),
      Población = c(mean(x_p), var(x_p)),
      Muestra = c(mean(x_m), var(x_m)),
      stringsAsFactors = FALSE
    )
    tags$div(
      class = "result-box",
      tags$h4("Comparación: media y varianza del diámetro", style="margin-top:0;"),
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("Estadístico", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("Población", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("Muestra", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
        )),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Estadístico[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$Población[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$Muestra[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
          )
        }))
      )
    )
  })
  
  output$tabla_muestra_vii_cuanti <- renderTable({
    m <- muestra_vii(); req(nrow(m) > 0)
    data.frame(ID = as.integer(m$id), Diametro = round(m$diam_did * ESCALA, 3), stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, spacing = "xs", rownames = FALSE, align = "c")
  
  muestra_export_vii <- reactive({
    m <- muestra_vii(); req(nrow(m) > 0)
    if (input$tipo_var_vii == "cuali") {
      data.frame(ID = as.integer(m$id), Color = as.character(m$color), stringsAsFactors = FALSE)
    } else {
      data.frame(ID = as.integer(m$id), Diametro = round(m$diam_did * ESCALA, 3), stringsAsFactors = FALSE)
    }
  })
  observeEvent(input$vii_descargar_csv, {
    df <- muestra_export_vii()
    req(nrow(df) > 0)
    
    csv_txt <- paste(capture.output(write.csv(df, row.names = FALSE, fileEncoding = "UTF-8")), collapse = "\n")
    
    session$sendCustomMessage("download_text_file", list(
      filename = paste0("muestra_vii_", Sys.Date(), ".csv"),
      content  = csv_txt,
      mime     = "text/csv;charset=utf-8"
    ))
  })
  
  # ==========================================================
  # VIII. ANÁLISIS DESCRIPTIVO E INFERENCIA (solo muestra)  ✅
  # ==========================================================
  observe({
    req(!is.null(input$tipo_var_viii))
    if (input$tipo_var_viii == "cuanti") {
      v0 <- var(diam_calc_pob())
      updateSliderInput(
        session, "var_pob_viii",
        min = max(0.10, v0 * 0.20),
        max = max(0.50, v0 * 3.00),
        value = v0,
        step = 0.10
      )
    }
  })
  
  z_viii <- reactive({
    conf <- input$conf_viii / 100
    qnorm(1 - (1 - conf) / 2)
  })
  
  n_viii <- reactive({
    Z <- z_viii()
    Nn <- N
    
    if (input$tipo_var_viii == "cuali") {
      E <- input$err_viii / 100
      p <- 0.5; q <- 0.5
      num <- Nn * (Z^2) * p * q
      den <- (Nn - 1) * (E^2) + (Z^2) * p * q
      n <- ceiling(num / den)
    } else {
      S2 <- input$var_pob_viii
      mu_p <- mean(diam_calc_pob())
      E_abs <- mu_p * (input$err_viii / 100)
      num <- Nn * (Z^2) * S2
      den <- (Nn - 1) * (E_abs^2) + (Z^2) * S2
      n <- ceiling(num / den)
    }
    
    max(10, min(n, Nn))
  })
  
  output$viii_formula <- renderUI({
    if (input$tipo_var_viii == "cuali") {
      withMathJax(
        tags$div(class = "formula-box",
                 tags$h4("Fórmula (población finita)"),
                 HTML("$$n = \\frac{N\\,Z^2\\,p\\,q}{(N-1)\\,E^2 + Z^2\\,p\\,q}$$"))
      )
    } else {
      withMathJax(
        tags$div(class = "formula-box",
                 tags$h4("Fórmula (población finita)"),
                 HTML("$$n = \\frac{N\\,Z^2\\,S^2}{(N-1)\\,E^2 + Z^2\\,S^2}$$"))
      )
    }
  })
  
  output$viii_valores <- renderUI({
    Z <- z_viii()
    Nn <- N
    
    if (input$tipo_var_viii == "cuali") {
      p <- 0.5; q <- 0.5
      E <- input$err_viii / 100
      tags$div(class = "formula-box",
               tags$h4("Valores sustituidos"),
               tags$ul(
                 tags$li(HTML(paste0("<b>N</b> = ", Nn))),
                 tags$li(HTML(paste0("<b>Z</b> = ", sprintf("%.3f", Z), " (", input$conf_viii, "%)"))),
                 tags$li(HTML(paste0("<b>p</b> = ", sprintf("%.2f", p), " ; <b>q</b> = ", sprintf("%.2f", q)))),
                 tags$li(HTML(paste0("<b>E</b> = ", sprintf("%.3f", E))))
               ),
               tags$hr(style="margin:8px 0;"),
               tags$h4(HTML(paste0("Tamaño de muestra calculado: <b>n = ", n_viii(), "</b>")))
      )
    } else {
      S2 <- input$var_pob_viii
      mu_p <- mean(diam_calc_pob())
      E_abs <- mu_p * (input$err_viii / 100)
      tags$div(class = "formula-box",
               tags$h4("Valores sustituidos"),
               tags$ul(
                 tags$li(HTML(paste0("<b>N</b> = ", Nn))),
                 tags$li(HTML(paste0("<b>Z</b> = ", sprintf("%.3f", Z), " (", input$conf_viii, "%)"))),
                 tags$li(HTML(paste0("<b>S²</b> = ", sprintf("%.3f", S2)))),
                 tags$li(HTML(paste0("<b>E</b> = ", sprintf("%.3f", E_abs))))
               ),
               tags$hr(style="margin:8px 0;"),
               tags$h4(HTML(paste0("Tamaño de muestra calculado: <b>n = ", n_viii(), "</b>")))
      )
    }
  })
  
  output$viii_formulas_stats <- renderUI({
    withMathJax(
      tags$div(class="formula-box",
               tags$h4("Fórmulas (notación matemática)"),
               HTML("
               $$\\hat{p}=\\frac{x}{n}$$
               $$\\bar{x}=\\frac{1}{n}\\sum_{i=1}^{n}x_i$$
               $$s^2=\\frac{1}{n-1}\\sum_{i=1}^{n}(x_i-\\bar{x})^2$$
               $$s=\\sqrt{s^2}$$
               "))
    )
  })
  
  sample_ids_viii <- reactiveVal(integer(0))
  tomar_muestra_viii <- function() {
    n <- min(n_viii(), N)
    sample_ids_viii(sample(poblacion$id, size = n, replace = FALSE))
  }
  observeEvent(TRUE, { tomar_muestra_viii() }, once = TRUE)
  observeEvent(
    c(input$tipo_var_viii, input$conf_viii, input$err_viii, input$var_pob_viii),
    { tomar_muestra_viii() },
    ignoreInit = TRUE
  )
  observeEvent(input$viii_nueva_muestra, { tomar_muestra_viii() }, ignoreInit = TRUE)
  
  muestra_viii <- reactive({
    ids <- sample_ids_viii()
    req(length(ids) > 0)
    poblacion[match(ids, poblacion$id), , drop = FALSE]
  })
  
  output$muestraPlot_cuali_viii <- renderPlot({
    m <- muestra_viii(); req(nrow(m) > 0)
    ggplot() +
      geom_point(data = poblacion, aes(x, y), shape = 21, size = 2.2, fill = "white",
                 color = "gray80", stroke = 0.12, alpha = 0.20) +
      geom_point(data = m, aes(x, y, fill = color, size = size_pt), shape = 21,
                 color = "gray15", stroke = 0.20, alpha = 0.98) +
      scale_fill_manual(values = cols) +
      scale_size_identity() +
      coord_equal(xlim = xlim_all, ylim = ylim_all, expand = FALSE) +
      labs(title = paste0("Muestra (n = ", nrow(m), ")")) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  })
  
  output$muestraPlot_cuanti_viii <- renderPlot({
    m <- muestra_viii(); req(nrow(m) > 0)
    ggplot() +
      geom_point(data = poblacion, aes(x, y), shape = 21, size = 2.2, fill = "white",
                 color = "gray80", stroke = 0.12, alpha = 0.20) +
      geom_point(data = m, aes(x, y, fill = color, size = size_pt), shape = 21,
                 color = "gold", stroke = 0.45, alpha = 0.98) +
      scale_fill_manual(values = cols) +
      scale_size_identity() +
      coord_equal(xlim = xlim_all, ylim = ylim_all, expand = FALSE) +
      labs(title = paste0("Muestra (n = ", nrow(m), ")")) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  })
  
  resultados_cuali_viii <- reactive({
    m <- muestra_viii(); req(nrow(m) > 0)
    tab <- table(m$color)
    df <- data.frame(
      Color = names(tab),
      Frecuencia = as.integer(tab),
      Proporción = as.numeric(tab) / sum(tab),
      stringsAsFactors = FALSE
    )
    df[order(df$Color), , drop = FALSE]
  })
  
  output$viii_resultados_cuali <- renderUI({
    df <- resultados_cuali_viii()
    tags$div(
      class = "result-box",
      tags$h4("Resultados de la muestra: frecuencias y proporciones", style="margin-top:0;"),
      tags$table(
        style="width:100%; border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("Color", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("Frecuencia", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("Proporción", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
        )),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Color[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
            tags$td(df$Frecuencia[i], style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$Proporción[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
          )
        }))
      )
    )
  })
  
  output$viii_ic_cuali <- renderUI({
    m <- muestra_viii(); req(nrow(m) > 0)
    n <- nrow(m)
    Z <- z_viii()
    f <- fpc(N, n)
    
    df <- resultados_cuali_viii()
    df$SE <- sqrt(df$Proporción * (1 - df$Proporción) / n) * f
    df$LI <- pmax(0, df$Proporción - Z * df$SE)
    df$LS <- pmin(1, df$Proporción + Z * df$SE)
    
    tags$div(
      class="result-box",
      tags$h4("Intervalos de confianza para las proporciones (muestra)", style="margin-top:0;"),
      withMathJax(HTML("$$\\hat{p} \\pm Z\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}\\cdot \\sqrt{\\frac{N-n}{N-1}}$$")),
      tags$table(
        style="width:100%; border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("Color", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("p̂", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("LI", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("LS", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
        )),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Color[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$Proporción[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$LI[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.3f", df$LS[i]), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
          )
        }))
      )
    )
  })
  
  output$tabla_muestra_viii_cuali <- renderTable({
    m <- muestra_viii(); req(nrow(m) > 0)
    data.frame(ID = as.integer(m$id), Color = as.character(m$color), stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, spacing = "xs", rownames = FALSE, align = "c")
  
  x_mue_viii <- reactive({
    m <- muestra_viii(); req(nrow(m) > 0)
    m$diam_did * ESCALA
  })
  
  output$viii_resultados_cuanti <- renderUI({
    x <- x_mue_viii()
    q <- quantile(x, probs = c(0.25, 0.50, 0.75), names = FALSE)
    df <- data.frame(
      Estadístico = c("n", "Media", "Varianza", "Desv. estándar", "Q1", "Mediana (Q2)", "Q3", "Sesgo", "Curtosis (exceso)"),
      Valor = c(length(x), mean(x), var(x), sd(x), q[1], q[2], q[3], skewness_simple(x), kurtosis_excess_simple(x)),
      stringsAsFactors = FALSE
    )
    tags$div(
      class = "result-box",
      tags$h4("Resultados de la muestra: estadísticos descriptivos", style="margin-top:0;"),
      tags$table(
        style="width:100%; border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("Estadístico", style="text-align:left; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("Valor", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
        )),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Estadístico[i], style="padding:6px; border-bottom:1px solid #e6f0ff;"),
            tags$td(sprintf("%.4f", as.numeric(df$Valor[i])),
                    style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
          )
        }))
      )
    )
  })
  
  output$viii_ic_cuanti <- renderUI({
    x <- x_mue_viii()
    n <- length(x)
    conf <- input$conf_viii / 100
    alpha <- 1 - conf
    tcrit <- qt(1 - alpha/2, df = n - 1)
    f <- fpc(N, n)
    se <- sd(x) / sqrt(n) * f
    li <- mean(x) - tcrit * se
    ls <- mean(x) + tcrit * se
    
    tags$div(
      class="result-box",
      tags$h4("Intervalo de confianza para la media (muestra)", style="margin-top:0;"),
      withMathJax(HTML("$$\\bar{x} \\pm t_{\\alpha/2,\\,n-1}\\,\\frac{s}{\\sqrt{n}}\\cdot \\sqrt{\\frac{N-n}{N-1}}$$")),
      tags$table(
        style="width:100%; border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("x̄", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("LI", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;"),
          tags$th("LS", style="text-align:right; border-bottom:2px solid #cfe3ff; padding:6px;")
        )),
        tags$tbody(tags$tr(
          tags$td(sprintf("%.4f", mean(x)), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
          tags$td(sprintf("%.4f", li), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;"),
          tags$td(sprintf("%.4f", ls), style="padding:6px; text-align:right; border-bottom:1px solid #e6f0ff;")
        ))
      )
    )
  })
  
  output$tabla_muestra_viii_cuanti <- renderTable({
    m <- muestra_viii(); req(nrow(m) > 0)
    data.frame(ID = as.integer(m$id), Diametro = round(m$diam_did * ESCALA, 3), stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, spacing = "xs", rownames = FALSE, align = "c")
  
  output$donaMue_cuali_viii <- renderPlot({
    m <- muestra_viii(); req(nrow(m) > 0)
    mue_tab <- prop.table(table(m$color))
    colores_all <- levels(poblacion$color)
    mue_tab <- mue_tab[colores_all]
    mue_tab[is.na(mue_tab)] <- 0
    
    df <- data.frame(
      categoria = colores_all,
      muestra = as.numeric(mue_tab),
      stringsAsFactors = FALSE
    )
    
    donut_plot(df, "muestra", "Proporción muestral (por color)")
  }) 
  
  output$histMue_cuanti_viii <- renderPlot({
    x <- x_mue_viii(); req(length(x) > 0)
    hist_con_normal(x, "Muestra: histograma del diámetro")
  })
  
  # ==========================================================
  # IX. INTERPRETACIÓN DE LA INFERENCIA (Simulador de cobertura)
  # ==========================================================
  ix_z <- reactive({
    qnorm(1 - (1 - input$ix_conf) / 2)
  })
  
  ix_theta <- reactive({
    if (input$ix_param == "prop") {
      mean(poblacion$color == input$ix_color_obj)
    } else {
      mean(poblacion$diam_did * ESCALA)
    }
  })
  
  ix_sigma <- reactive({
    if (input$ix_param == "prop") {
      p <- ix_theta()
      sqrt(p * (1 - p))
    } else {
      sd(poblacion$diam_did * ESCALA)
    }
  })
  
  ix_n <- reactive({
    Z <- ix_z()
    sig <- ix_sigma()
    E <- if (input$ix_param == "prop") input$ix_E_prop else input$ix_E_med
    
    num <- N * (Z^2) * (sig^2)
    den <- (N - 1) * (E^2) + (Z^2) * (sig^2)
    n <- ceiling(num / den)
    
    max(10, min(n, N))
  })
  
  output$ix_formula <- renderUI({
    if (input$ix_param == "prop") {
      withMathJax(
        tags$div(class="formula-box",
                 tags$h4("Fórmula usada"),
                 HTML("$$n = \\frac{N\\,Z^2\\,\\sigma^2}{(N-1)\\,E^2 + Z^2\\,\\sigma^2},\\quad \\sigma=\\sqrt{\\pi(1-\\pi)}$$"))
      )
    } else {
      withMathJax(
        tags$div(class="formula-box",
                 tags$h4("Fórmula usada"),
                 HTML("$$n = \\frac{N\\,Z^2\\,\\sigma^2}{(N-1)\\,E^2 + Z^2\\,\\sigma^2}$$"))
      )
    }
  })
  
  output$ix_sustitucion <- renderUI({
    Z <- ix_z()
    sig <- ix_sigma()
    E <- if (input$ix_param == "prop") input$ix_E_prop else input$ix_E_med
    theta <- ix_theta()
    
    tags$div(
      class="formula-box",
      tags$h4("Valores sustituidos"),
      tags$ul(
        tags$li(HTML(paste0("<b>N</b> = ", N))),
        tags$li(HTML(paste0("<b>Z</b> = ", sprintf("%.3f", Z), " (", round(input$ix_conf*100), "%)"))),
        tags$li(HTML(paste0("<b>σ</b> = ", sprintf("%.4f", sig)))),
        tags$li(HTML(paste0("<b>E</b> = ", sprintf("%.4f", E)))),
        tags$li(HTML(paste0("<b>Parámetro poblacional</b> = ", sprintf("%.4f", theta))))
      )
    )
  })
  
  output$ix_n_txt <- renderUI({
    tags$div(class="result-box",
             tags$h4(HTML(paste0("Tamaño de muestra calculado: <b>n = ", ix_n(), "</b>")),
                     style="margin-top:0;"))
  })
  
  ix_trigger <- reactiveVal(0)
  observeEvent(input$ix_recalcular, {
    ix_trigger(ix_trigger() + 1)
  })
  
  ix_intervalos <- reactive({
    ix_trigger()
    
    B <- as.integer(input$ix_B)
    n <- as.integer(ix_n())
    theta <- ix_theta()
    
    lower <- upper <- numeric(B)
    
    if (input$ix_param == "prop") {
      Z <- ix_z()
      y <- as.integer(poblacion$color == input$ix_color_obj)
      f <- fpc(N, n)
      
      for (i in seq_len(B)) {
        idx <- sample.int(N, n, replace = FALSE)
        phat <- mean(y[idx])
        e <- Z * sqrt(phat * (1 - phat) / n) * f
        lower[i] <- phat - e
        upper[i] <- phat + e
      }
      lower <- pmax(0, lower); upper <- pmin(1, upper)
      
    } else {
      conf <- input$ix_conf
      alpha <- 1 - conf
      tcrit <- qt(1 - alpha/2, df = n - 1)
      x <- poblacion$diam_did * ESCALA
      f <- fpc(N, n)
      
      for (i in seq_len(B)) {
        idx <- sample.int(N, n, replace = FALSE)
        xb <- mean(x[idx])
        s <- sd(x[idx])
        e <- tcrit * (s / sqrt(n)) * f
        lower[i] <- xb - e
        upper[i] <- xb + e
      }
    }
    
    cubren <- (lower <= theta) & (upper >= theta)
    list(lower = lower, upper = upper, cubren = cubren, theta = theta, n = n, B = B)
  })
  
  output$ix_plot_ic <- renderPlot({
    obj <- ix_intervalos()
    B <- obj$B
    ylim <- range(c(obj$lower, obj$upper, obj$theta), finite = TRUE)
    
    titulo <- if (input$ix_param == "prop") {
      paste0("Intervalos de confianza (proporción)  |  n = ", obj$n, "  |  B = ", B)
    } else {
      paste0("Intervalos de confianza (media)  |  n = ", obj$n, "  |  B = ", B)
    }
    
    plot(NULL, xlim = c(0, B + 1), ylim = ylim,
         xlab = "Muestras", ylab = "Intervalo", main = titulo)
    abline(h = obj$theta, col = "blue", lwd = 3, lty = 2)
    segments(1:B, obj$lower, 1:B, obj$upper,
             col = ifelse(obj$cubren, "darkgreen", "red"),
             lwd = if (B <= 100) 2 else 0.8)
  })
  
  output$ix_resumen <- renderUI({
    obj <- ix_intervalos()
    pct <- mean(obj$cubren) * 100
    
    etiqueta_theta <- if (input$ix_param == "prop") "π" else "μ"
    extra <- if (input$ix_param == "prop") {
      paste0(" (color = ", input$ix_color_obj, ")")
    } else ""
    
    tags$div(
      class="result-box",
      tags$h4("Resumen del experimento", style="margin-top:0;"),
      tags$ul(
        tags$li(HTML(paste0("<b>Parámetro poblacional ", etiqueta_theta, "</b> = ", sprintf("%.4f", obj$theta), extra))),
        tags$li(HTML(paste0("<b>n</b> = ", obj$n, " ; <b>B</b> = ", obj$B))),
        tags$li(HTML(paste0("<b>Porcentaje de intervalos que capturan al parámetro</b> = <b>", sprintf("%.1f", pct), "%</b>")))
      )
    )
  })
  
  output$ix_pct_captura <- renderUI({
    obj <- ix_intervalos()
    pct <- mean(obj$cubren) * 100
    
    tags$div(
      class="result-box",
      tags$h4("Cobertura empírica", style="margin-top:0;"),
      p(HTML(paste0("Porcentaje de intervalos que capturan al parámetro poblacional: <b>", sprintf("%.1f", pct), "%</b>."))),
      p("En un experimento ideal, este porcentaje suele estar cerca del nivel de confianza seleccionado, aunque puede variar por azar muestral.")
    )
  })
  
  # ==========================================================
  # X. GLOSARIO Y SIMBOLOGÍA (integrado)
  # ==========================================================
  output$glosario_ui <- renderUI({
    withMathJax(
      tags$div(
        class = "glossary-wrap",
        
        div(
          class = "result-box",
          tags$h4("Glosario: conceptos, símbolos y notación", style="margin-top:0;"),
          
          tags$table(
            class = "glossary-table",
            tags$thead(
              tags$tr(
                tags$th("Concepto"),
                tags$th("Símbolo"),
                tags$th("Descripción")
              )
            ),
            tags$tbody(
              
              tags$tr(
                tags$td(HTML("<b>Población o universo</b>")),
                tags$td(withMathJax(HTML("\\(\\Omega\\),\\ \\(N\\)"))),
                tags$td(HTML("Conjunto total de individuos con características de interés. \\(N\\) denota el tamaño de la población."))
              ),
              
              tags$tr(
                tags$td(HTML("<b>Variable</b>")),
                tags$td(HTML("<span class='math-serif-bold'>X</span>")),
                tags$td(HTML("Representa el atributo de interés. <b>Clasificación:</b> <b>cualitativa</b> (categorías) o <b>cuantitativa</b> (valores numéricos)."))
              ),
              
              tags$tr(
                tags$td(HTML("<b>Parámetro poblacional (genérico, desconocido)</b>")),
                tags$td(HTML("<span class='math-serif-bold'>&theta;</span>")),
                tags$td("Valor único que resume la característica de interés en la población; generalmente es desconocido y se estima con una muestra.")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Proporción poblacional</b> (si el interés es cualitativo)")),
                tags$td(HTML("<span class='math-serif-bold'>&pi;</span>")),
                tags$td(withMathJax(HTML("Parámetro cuando la variable es cualitativa (éxito/fracaso). \\(\\pi\\) es la proporción de “éxitos” en la población.")))
              ),
              
              tags$tr(
                tags$td(HTML("<b>Media poblacional</b> (si el interés es cuantitativo)")),
                tags$td(HTML("<span class='math-serif-bold'>&mu;</span>")),
                tags$td(withMathJax(HTML("Parámetro cuando la variable es cuantitativa (promedio de la población).")))
              ),
              
              tags$tr(
                tags$td(HTML("<b>Muestreo</b>")),
                tags$td(withMathJax(HTML("\\(n\\)"))),
                tags$td("Proceso de selección aleatoria de unidades de la población. \\(n\\) es el tamaño de la muestra.")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Nivel de confianza</b>")),
                tags$td(withMathJax(HTML("\\(1-\\alpha\\)"))),
                tags$td("Probabilidad de éxito del procedimiento de estimación (cobertura a largo plazo).")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Margen de error</b>")),
                tags$td(withMathJax(HTML("\\(E\\)"))),
                tags$td("Medida máxima de desviación respecto al valor del parámetro investigado.")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Varianza poblacional</b> (para estimar media)")),
                tags$td(HTML("<span class='math-serif-bold'>&sigma;<sup>2</sup></span>")),
                tags$td(withMathJax(HTML("Medida de dispersión en la población. Se usa en el cálculo de \\(n\\) cuando se estima \\(\\mu\\).")))
              ),
              
              # --- Tamaño de muestra (FINITA) ---
              tags$tr(
                tags$td(HTML("<b>Tamaño de muestra (proporción, población finita)</b>")),
                tags$td(withMathJax(HTML("\\(n\\)"))),
                tags$td(withMathJax(HTML(
                  "Fórmula: \\[\\; n = \\frac{N\\,Z_{\\alpha/2}^{2}\\,\\pi(1-\\pi)}{(N-1)\\,E^{2} + Z_{\\alpha/2}^{2}\\,\\pi(1-\\pi)} \\;\\]"
                )))
              ),
              tags$tr(
                tags$td(HTML("<b>Tamaño de muestra (media, población finita)</b>")),
                tags$td(withMathJax(HTML("\\(n\\)"))),
                tags$td(withMathJax(HTML(
                  "Fórmula: \\[\\; n = \\frac{N\\,Z_{\\alpha/2}^{2}\\,\\sigma^{2}}{(N-1)\\,E^{2} + Z_{\\alpha/2}^{2}\\,\\sigma^{2}} \\;\\]"
                )))
              ),
              
              tags$tr(
                tags$td(HTML("<b>Unidades muestrales</b>")),
                tags$td(withMathJax(HTML("\\(X_1,\\dots,X_n\\)"))),
                tags$td("Individuos seleccionados aleatoriamente que conforman la muestra.")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Estadístico (estadígrafo)</b>")),
                tags$td(withMathJax(HTML("\\(t\\)"))),
                tags$td("Medida-resumen calculada con la muestra (toma un solo valor). Depende del parámetro investigado.")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Proporción muestral</b> (si se investiga &pi;)")),
                tags$td(withMathJax(HTML("\\(p\\)"))),
                tags$td(withMathJax(HTML("\\(p=\\frac{x}{n}\\), donde \\(x\\) es el número de éxitos observados en la muestra.")))
              ),
              
              tags$tr(
                tags$td(HTML("<b>Media muestral</b> (si se investiga &mu;)")),
                tags$td(withMathJax(HTML("\\(\\bar{x}\\)"))),
                tags$td(withMathJax(HTML("\\(\\bar{x}=\\frac{1}{n}\\sum_{i=1}^{n}x_i\\).")))
              ),
              
              # --- Mediana ---
              tags$tr(
                tags$td(HTML("<b>Mediana</b>")),
                tags$td(withMathJax(HTML("\\(\\tilde{x}\\)"))),
                tags$td(withMathJax(HTML(
                  "Si \\(x_{(1)}\\le\\cdots\\le x_{(n)}\\): \\[\\;\\tilde{x}=\\begin{cases}x_{\\left(\\frac{n+1}{2}\\right)}, & n\\ \\text{impar}\\\\[4pt]
                \\frac{x_{\\left(\\frac{n}{2}\\right)}+x_{\\left(\\frac{n}{2}+1\\right)}}{2}, & n\\ \\text{par}
                \\end{cases}\\;\\]"
                )))
              ),
              
              # --- Moda ---
              tags$tr(
                tags$td(HTML("<b>Moda</b>")),
                tags$td(withMathJax(HTML("\\(\\mathrm{Mo}\\)"))),
                tags$td(withMathJax(HTML(
                  "Valor con mayor frecuencia observada: \\(\\mathrm{Mo}=\\arg\\max_{x}\\,f(x)\\), donde \\(f(x)\\) es la frecuencia."
                )))
              ),
              
              # --- Cuartil i-ésimo ---
              tags$tr(
                tags$td(HTML("<b>Cuartil i-ésimo</b>")),
                tags$td(withMathJax(HTML("\\(Q_i\\)"))),
                tags$td(withMathJax(HTML(
                  "Para \\(i\\in\\{1,2,3\\}\\) y datos ordenados: \\[\\; Q_i = x_{\\left(\\lceil \\frac{i(n+1)}{4} \\rceil\\right)} \\;\\] (regla de posición; existen variantes equivalentes en la práctica)."
                )))
              ),
              
              # --- Varianza muestral ---
              tags$tr(
                tags$td(HTML("<b>Varianza muestral</b>")),
                tags$td(withMathJax(HTML("\\(s^2\\)"))),
                tags$td(withMathJax(HTML(
                  "\\[\\; s^2=\\frac{1}{n-1}\\sum_{i=1}^{n}(x_i-\\bar{x})^2 \\;\\]"
                )))
              ),
              
              # --- Sesgo ---
              tags$tr(
                tags$td(HTML("<b>Sesgo</b>")),
                tags$td(withMathJax(HTML("\\(g_1\\)"))),
                tags$td(withMathJax(HTML(
                  "\\[\\; g_1=\\frac{1}{n}\\sum_{i=1}^{n}\\left(\\frac{x_i-\\bar{x}}{s}\\right)^3 \\;\\]"
                )))
              ),
              
              # --- Curtosis (exceso) ---
              tags$tr(
                tags$td(HTML("<b>Curtosis</b> (exceso)")),
                tags$td(withMathJax(HTML("\\(g_2\\)"))),
                tags$td(withMathJax(HTML(
                  "\\[\\; g_2=\\frac{1}{n}\\sum_{i=1}^{n}\\left(\\frac{x_i-\\bar{x}}{s}\\right)^4-3 \\;\\]"
                )))
              ),
              
              tags$tr(
                tags$td(HTML("<b>Inferencia estadística</b>")),
                tags$td("—"),
                tags$td("Proceso de aproximar parámetros poblacionales a partir de estadísticos muestrales, cuantificando la incertidumbre.")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Estimador genérico</b>")),
                tags$td(withMathJax(HTML("\\(\\hat{\\theta}\\)"))),
                tags$td("Aproximación de \\(\\theta\\) basada en la muestra.")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Relación estimador–parámetro (proporción)</b>")),
                tags$td(withMathJax(HTML("\\(\\hat{\\theta}=p\\), \\(\\hat{\\pi}=p\\)"))),
                tags$td("Si el parámetro investigado es \\(\\pi\\), el estimador es la proporción muestral \\(p\\).")
              ),
              
              tags$tr(
                tags$td(HTML("<b>Relación estimador–parámetro (media)</b>")),
                tags$td(withMathJax(HTML("\\(\\hat{\\theta}=\\bar{x}\\), \\(\\hat{\\mu}=\\bar{x}\\)"))),
                tags$td("Si el parámetro investigado es \\(\\mu\\), el estimador es la media muestral \\(\\bar{x}\\).")
              )
            )
          )
        ),
        
        tags$div(
          class = "pedagogic-note",
          tags$div(class = "title", "Ideas clave"),
          tags$div(class = "mathline",
                   withMathJax(HTML("\\(\\textbf{Población}\\;\\rightarrow\\;\\boldsymbol{\\theta}\\;(\\text{desconocido})\\)"))),
          tags$div(class = "mathline",
                   withMathJax(HTML("\\(\\textbf{Muestra}\\;\\rightarrow\\;t\\;\\rightarrow\\;\\hat{\\boldsymbol{\\theta}}\\)"))),
          tags$div(
            style="margin-top:8px;",
            tags$ul(
              tags$li(withMathJax(HTML("Si se investiga proporción: \\(\\hat{\\pi}=p\\)"))),
              tags$li(withMathJax(HTML("Si se investiga media: \\(\\hat{\\mu}=\\bar{x}\\)")))
            )
          )
        )
      )
    )
  })
  
}

shinyApp(ui, server)