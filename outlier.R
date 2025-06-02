library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(DBI)
library(dbplyr)
library(lubridate)
library(DT)
library(robustbase)
library(anomalize)
library(furrr)
library(forecast)
library(dbscan)
library(nortest)
library(tseries)
library(moments)

if(requireNamespace("bestNormalize", quietly = TRUE)) {
  library(bestNormalize)
} else {
  warning("Package 'bestNormalize' is not installed. Lütfen kurun.")
}

if (!requireNamespace("MASS", quietly = TRUE)) {
  stop("Package 'MASS' is required but not installed. Please install it.")
}

plan(multisession, workers = parallel::detectCores() - 1)

options(temizhavaR.base_dir = "/home/byte/Desktop/Work/TemizHava_base_dir")
con <- tryCatch(
  temizhavaR:::create_postgres_conn(),
  error = function(e) { stop("Veritabanı bağlantısı başarısız: ", e$message) }
)

technique_descriptions <- list(
  hampel = list(
    name = "Hampel Filtresi",
    description = "Medyan mutlak sapma (MAD) temelli robust bir yöntemdir. Penceredeki medyan ve MAD hesaplanarak eşik aşımı belirlenir.",
    pros = "• Hızlı, hesaplama açısından verimli\n• Küçük veri setlerinde iyi performans\n• Aykırı değerlere karşı dayanıklı",
    cons = "• Periyodik desenlerde zorlanabilir\n• Pencere boyutu seçimi kritik"
  ),
  gesd = list(
    name = "Genelleştirilmiş ESD",
    description = "İstatistiksel olarak anlamlı aykırı değerleri tespit eder. Çoklu aykırı değeri tek tek test eder.",
    pros = "• İstatistiksel olarak sağlam\n• Çoklu aykırı değeri tespit edebilir\n• Normal dağılım varsayımı altında etkilidir",
    cons = "• Hesaplama yoğun\n• Maksimum aykırı değer sayısının belirlenmesi gerekir"
  ),
  stl_esd = list(
    name = "STL + ESD",
    description = "Veriyi mevsimsel, trend ve artık bileşenlerine ayırır; ardından artık bileşende ESD testi uygular.",
    pros = "• Mevsimsel ve trend etkilerini giderir\n• Karmaşık zaman serilerinde uygundur\n• Yüksek doğruluk",
    cons = "• Hesaplama yoğun\n• Uzun zaman serileri gerekir"
  ),
  dbscan = list(
    name = "DBSCAN",
    description = "Yoğunluk temelli kümeleme algoritmasıdır. Yoğun bölgeleri küme, seyrek bölgeleri gürültü olarak değerlendirir.",
    pros = "• Küme bağımsız\n• Gürültüye karşı dayanıklı\n• Parametreler sezgisel",
    cons = "• Yoğunluk farklılıklarında zorlanabilir\n• Yüksek boyutlu verilerde performans düşebilir"
  )
)

# 1) Add new method description
technique_descriptions$cdf <- list(
  name        = "Teorik CDF Temelli",
  description = "En iyi uydurulan dağılımın %95 kesme noktasına göre aykırı değerleri tespit eder.",
  pros        = "• Olasılıksal temelli\n• Parametrik dağılım varsayımı\n• Yüksek özgüllük",
  cons        = "• Dağılım varsayımı tutmuyorsa yanıltıcı olabilir"
)

# --- Modal progress functions ---
show_modal_progress <- function(text) {
  showModal(modalDialog(title = "İşlem Devam Ediyor", paste(text, "..."), footer = NULL))
}
remove_modal_progress <- function() {
  removeModal()
}
inc_modal_progress <- function(progress, detail = "") {
  message(sprintf("İlerleme: %d%% %s", round(progress * 100), detail))
}

# --- UI ---
ui <- navbarPage(
  title = div(
    icon("cloud"), "Hava Kirliliği Analiz Platformu"
  ),
  windowTitle = "Hava Kirliliği Analiz Platformu",
  inverse = TRUE,
  tabPanel(
    "Dağılım Analizi",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .shiny-output-error { color: #ff0000; }
          .sidebar-panel { background-color: #f8f9fa; padding: 18px 18px 10px 18px; border-radius: 8px; box-shadow: 0 2px 8px #e0e0e0; }
          .main-panel { padding-left: 30px; }
          .info-text { font-size: 1em; color: #6c757d; margin-bottom: 10px; }
          .dist-summary { background-color: #e9ecef; padding: 12px; border-radius: 6px; margin-bottom: 18px; font-family: monospace;}
          .section-title { color: #007bff; margin-top: 18px; margin-bottom: 8px; }
          .panel-heading { font-size: 1.2em; font-weight: bold; color: #007bff; }
          .shiny-input-container { margin-bottom: 12px; }
        "))
      ),
      fluidRow(
        column(12, h2(icon("chart-bar"), "Dağılım Analizi", class = "panel-heading")),
        column(12, div(class = "info-text", "Belirli bir istasyon ve kirletici için dağılım analizi yapar. En iyi uyan dağılımı bulur, AIC değerlerini ve yoğunluk grafiğini gösterir."))
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",
          pickerInput("station_select_dist",
                      "İstasyon Seçin:",
                      choices = NULL,
                      multiple = FALSE,
                      options = list(`live-search` = TRUE)),
          selectInput("pollutant_select_dist",
                      "Kirletici:",
                      choices = c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3"),
                      selected = "PM10"),
          hr(),
          # 1. Add date range input for filtering
          dateRangeInput("date_range_dist", "Tarih Aralığı:",
                         start = Sys.Date() - 365, end = Sys.Date(),
                         format = "yyyy-mm-dd"),
          actionButton("analyze_dist_btn", label = tagList(icon("search"), "Dağılımı Analiz Et"), class = "btn btn-primary btn-block")
        ),
        mainPanel(
          width = 9,
          class = "main-panel",
          h4(class = "section-title", icon("award"), "En İyi Uyan Dağılım"),
          uiOutput("dist_fit_summary"),
          hr(),
          h4(class = "section-title", icon("chart-area"), "Yoğunluk Grafiği ve Uydurulan Dağılım"),
          div(class = "info-text", "Grafik, verinin yoğunluğunu (mavi) ve en iyi uyan teorik dağılımı (yeşil) gösterir."),
          plotlyOutput("density_plot_dist", height = "350px"),
          hr(),
          h4(class = "section-title", icon("table"), "Tüm Denenen Dağılımlar (AIC Değerleri)"),
          DTOutput("aic_table"),
          hr(),
          h4(class = "section-title", icon("list-ol"), "Veri Önizleme (İlk 100 Satır)"),
          DTOutput("data_preview_table")
        )
      )
    )
  ),
  tabPanel(
    "Aykırı Değer Analizi",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .shiny-output-error { color: #ff0000; }
          .loading-message { font-weight: bold; font-size: 18px; }
          .summary-panel { font-family: monospace; }
          .plot-container { height: 500px; }
          .sidebar-panel { background-color: #f8f9fa; padding: 18px 18px 10px 18px; border-radius: 8px; box-shadow: 0 2px 8px #e0e0e0; }
          .main-panel { padding-left: 30px; }
          .nav-tabs { margin-bottom: 15px; }
          .tab-content { padding-top: 15px; }
          .bttn-gradient { width: 100%; margin-bottom: 10px; }
          .info-text { font-size: 1em; color: #6c757d; margin-bottom: 10px; }
          .method-description { background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #007bff; }
          .method-title { font-weight: bold; color: #007bff; margin-top: 0; }
          .pros-cons { margin-top: 10px; }
          .pros { color: #28a745; }
          .cons { color: #dc3545; }
        "))
      ),
      fluidRow(
        column(12, h2(icon("exclamation-triangle"), "Aykırı Değer Analizi", class = "panel-heading")),
        column(12, div(class = "info-text", "Seçilen istasyon ve kirletici için aykırı değer tespiti ve zaman serisi analizleri."))
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",
          pickerInput("station_select_outlier", 
                      "İstasyon Seçin:",
                      choices = NULL, 
                      multiple = FALSE, 
                      options = list(`live-search` = TRUE)),
          selectInput("pollutant_select_outlier",
                      "Kirletici:",
                      choices = c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3"),
                      selected = "PM10"),
          hr(),
          # 1. Add date range input for filtering
          dateRangeInput("date_range_outlier", "Tarih Aralığı:",
                         start = Sys.Date() - 365, end = Sys.Date(),
                         format = "yyyy-mm-dd"),
          radioGroupButtons("method_select",
                            "Aykırı Değer Tespit Yöntemi:",
                            choices = c("Hampel Filtresi" = "hampel",
                                        "Genelleştirilmiş ESD" = "gesd",
                                        "STL + ESD" = "stl_esd",
                                        "DBSCAN" = "dbscan",
                                        "Teorik CDF" = "cdf"),
                            selected = "stl_esd",
                            status = "primary",
                            direction = "vertical"),
          uiOutput("method_description"),
          sliderInput("sensitivity",
                      "Hassasiyet (Eşik):",
                      min = 1, max = 5, value = 3, step = 0.1),
          materialSwitch("seasonal_adjust",
                         "Mevsimsel Düzeltme Uygula",
                         status = "primary", value = TRUE),
          materialSwitch("trend_adjust",
                         "Trend Düzeltme Uygula",
                         status = "primary", value = TRUE),
          materialSwitch("normalize_data",
                         "Veriyi Normalleştir (Box-Cox)",
                         status = "primary", value = FALSE),
          hr(),
          actionBttn("analyze_btn", label = tagList(icon("search"), "Seçili İstasyonu Analiz Et"), style = "gradient", color = "primary", class = "bttn-gradient"),
          actionBttn("batch_btn", label = tagList(icon("cogs"), "Tüm İstasyonları Toplu İşle"), style = "gradient", color = "danger", class = "bttn-gradient"),
          downloadBttn("download_btn", label = tagList(icon("download"), "Sonuçları İndir"), style = "gradient", color = "success")
        ),
        mainPanel(
          width = 9,
          class = "main-panel",
          tabsetPanel(
            type = "tabs",
            tabPanel("Zaman Serisi Analizi",
                     div(class = "info-text",
                         "Bu grafik, seçilen istasyon için kirletici konsantrasyonlarının zaman serisini göstermektedir.
                          Kırmızı noktalar tespit edilen aykırı değerleri göstermektedir."),
                     fluidRow(column(12, div(class = "plot-container", plotlyOutput("ts_plot")))),
                     fluidRow(
                       column(6, div(class = "plot-container", plotlyOutput("decomposition_plot"))),
                       column(6, div(class = "plot-container", plotlyOutput("residual_plot")))
                     ),
                     plotlyOutput("ak_hourly_comparison_plot"),
                     textOutput("ak_nullify_msg")
            ),
            tabPanel("İstatistiksel Analiz",
                     div(class = "info-text",
                         "Bu bölüm, kirletici verilerinin görselleştirmelerini ve istatistiksel özetlerini sunar.
                          Kutu ve yoğunluk grafikleri, temel istatistikler ve aykırı değer oranını içerir."),
                     fluidRow(
                       column(6, div(class = "plot-container", plotlyOutput("boxplot"))),
                       column(6, div(class = "plot-container", plotlyOutput("density_plot")))
                     ),
                     fluidRow(column(12, verbatimTextOutput("stats_summary")))
            ),
            tabPanel("Aykırı Değer Tablosu",
                     div(class = "info-text",
                         "Bu tablo, tespit edilen aykırı değerleri tarih, değer ve (varsa) istasyon bilgisiyle listeler."),
                     DTOutput("outlier_table"),
                     plotlyOutput("lognorm_cutoff_plot", height = "300px")
            ),
            tabPanel("Tanısal Analizler",
                     div(class = "info-text",
                         "Bu sekme, ACF grafikleri ve normallik, durağanlık testleri gibi tanısal analizleri içerir."),
                     fluidRow(column(12, verbatimTextOutput("model_diagnostics"))),
                     fluidRow(
                       column(6, div(class = "plot-container", plotlyOutput("acf_plot"))),
                       column(6, div(class = "plot-container", plotlyOutput("transformed_plot")))
                     )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "En Yüksek 5 İstasyon",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .top5-title { font-size: 1.5em; color: #007bff; font-weight: bold; margin-bottom: 10px; }
          .top5-section { margin-bottom: 18px; }
        "))
      ),
      div(class = "top5-title", icon("star"), "En Yüksek 5 İstasyon (public.views.hourly_75_pm10_pm25_so2_no2)"),
      DTOutput("top5_station_table"),
      hr(),
      h4(class = "section-title", icon("balance-scale"), "Karşılaştırmalı Analiz Sonuçları"),
      DTOutput("top5_comparison_table"),
      hr(),
      fluidRow(
        column(12, checkboxGroupInput(
          "top5_selected_pollutants",
          "Grafikte Gösterilecek Kirleticiler:",
          choices = c("PM10", "PM25", "SO2", "NO2"),
          selected = c("PM10", "PM25", "SO2", "NO2"),
          inline = TRUE
        ))
      ),
      h4("Z-Score Aykırı Değerler (±3σ)"),
      plotlyOutput("top5_zscore_outlier_plot", height = "350px"),
      h4("CDF Temelli Aykırı Değerler (Sağ Kuyruk %5)"),
      plotlyOutput("top5_cdf95_outlier_plot", height = "350px"),
      h4("CDF Temelli Aykırı Değerler (Sağ Kuyruk %1)"),
      plotlyOutput("top5_cdf99_outlier_plot", height = "350px"),
      h4("IQR Temelli Aykırı Değerler (Q3 + 1.5*IQR)"),
      plotlyOutput("top5_iqr_outlier_plot", height = "350px"),
      hr(),
      # ➊ Download button for Top5 outliers
      downloadBttn(
        "download_top5_outliers",
        label = tagList(icon("download"), "Top5 Aykırı Değerleri İndir"),
        style = "gradient", color = "success"
      ),
      uiOutput("top5_station_details")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # --- Distribution Analysis State ---
  rv_dist <- reactiveValues(
    data = NULL,
    fit_results = NULL
  )
  # --- Outlier Analysis State ---
  rv <- reactiveValues(
    station_data = NULL,
    outliers = NULL,
    all_stations = NULL,
    decomposition = NULL,
    transformation = list(transformed = FALSE)
  )

  # --- Shared: Station List Loading ---
  observe({
    tryCatch({
      stations <- dbGetQuery(con,
        "SELECT DISTINCT \"Istasyon_modified\" as station_id FROM public.hourly_detail_zcleaned ORDER BY station_id")
      updatePickerInput(session, "station_select_dist", choices = stations$station_id)
      updatePickerInput(session, "station_select_outlier", choices = stations$station_id)
      rv$all_stations <- stations$station_id
    }, error = function(e) {
      showNotification(paste("Veritabanı hatası:", e$message), type = "error")
    })
  })

  # --- Distribution Analysis Logic (for Dağılım Analizi tab) ---
  validate_and_clean_data_dist <- function(data, pollutant) {
    data %>%
      mutate(
        date  = as_date(date),
        value = suppressWarnings(as.numeric(as.character(.data[[pollutant]])))
      ) %>%
      # remove bad dates/values, keep negative‐check
      filter(!is.na(date), is.finite(value), value >= 0) %>%
      arrange(date) %>%
      # nullify Z-score outliers
      { tmp <- .
        z   <- (tmp$value - mean(tmp$value, na.rm=TRUE)) / sd(tmp$value, na.rm=TRUE)
        tmp %>% mutate(value = ifelse(abs(z) > 3, NA, value))
      } %>%
      select(date, value)
  }

  fit_best_distribution <- function(x_values) {
    x_vec <- as.numeric(x_values[is.finite(x_values)])
    if (length(x_vec) < 10) return(NULL)
    dists <- c("normal", "lognormal", "gamma", "weibull", "exponential", "logistic", "cauchy")
    fits <- list()
    aics <- c()
    for (d in dists) {
      fit <- tryCatch({
        suppressWarnings({
          if (d == "normal") {
            MASS::fitdistr(x = x_vec, densfun = "normal")
          } else if (d == "lognormal") {
            if (all(x_vec > 0)) MASS::fitdistr(x = x_vec, densfun = "lognormal") else NULL
          } else if (d == "gamma") {
            if (all(x_vec > 0)) MASS::fitdistr(x = x_vec, densfun = "gamma") else NULL
          } else if (d == "weibull") {
            if (all(x_vec > 0)) MASS::fitdistr(x = x_vec, densfun = "weibull") else NULL
          } else if (d == "exponential") {
            if (all(x_vec > 0)) MASS::fitdistr(x = x_vec, densfun = "exponential") else NULL
          } else if (d == "logistic") {
            MASS::fitdistr(x = x_vec, densfun = "logistic")
          } else if (d == "cauchy") {
            MASS::fitdistr(x = x_vec, densfun = "cauchy")
          }
        })
      }, error = function(e) NULL)
      if (!is.null(fit)) {
        k <- length(fit$estimate)
        aic <- 2 * k - 2 * fit$loglik
        fits[[d]] <- fit
        aics[d] <- aic
      }
    }
    if (length(aics) == 0) return(NULL)
    best <- names(which.min(aics))
    list(
      best_dist = best,
      fit = fits[[best]],
      all_fits = fits,
      all_aic = aics
    )
  }

  observeEvent(input$analyze_dist_btn, {
    req(input$station_select_dist, input$pollutant_select_dist)
    shiny::showNotification("Veri yükleniyor ve dağılım analizi yapılıyor...", type = "message", duration = NULL, id = "fitting_msg")
    tryCatch({
      # use raw hourly_detail here
      qry <- sprintf("SELECT \"Tarih\" as date, \"%s\" FROM public.hourly_detail
                      WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL
                        AND \"Tarih\" BETWEEN '%s' AND '%s'
                      ORDER BY \"Tarih\"",
                     input$pollutant_select_dist, input$station_select_dist, input$pollutant_select_dist,
                     format(input$date_range_dist[1]), format(input$date_range_dist[2]))
      data_raw <- dbGetQuery(con, qry)
      data_clean <- validate_and_clean_data_dist(data_raw, input$pollutant_select_dist)
      if(nrow(data_clean) < 30) {
        shiny::removeNotification("fitting_msg")
        showNotification("Yeterli sayıda geçerli veri yok (en az 30 gereklidir)", type = "error")
        return()
      }
      rv_dist$data <- data_clean
      value_vec <- as.numeric(data_clean$value)
      temp_results <- fit_best_distribution(x_values = value_vec)
      rv_dist$fit_results <- temp_results
      shiny::removeNotification("fitting_msg")
      if(is.null(rv_dist$fit_results)) {
        showNotification("Dağılım uydurulamadı veya ana fonksiyonda hata oluştu.", type = "warning")
      } else {
        showNotification("Dağılım analizi tamamlandı.", type = "message")
      }
    }, error = function(e) {
      shiny::removeNotification("fitting_msg")
      showNotification(paste("Hata:", conditionMessage(e)), type = "error", duration = NULL)
      rv_dist$data <- NULL
      rv_dist$fit_results <- NULL
    })
  })

  output$dist_fit_summary <- renderUI({
    req(rv_dist$fit_results)
    fit_info <- rv_dist$fit_results
    if (is.null(fit_info) || is.null(fit_info$fit)) return(p("Dağılım bilgisi bulunamadı."))
    fit <- fit_info$fit
    dname <- fit_info$best_dist
    dist_names <- c(
      "normal" = "Normal",
      "lognormal" = "Lognormal",
      "gamma" = "Gamma",
      "weibull" = "Weibull",
      "exponential" = "Üstel",
      "logistic" = "Lojistik",
      "cauchy" = "Cauchy"
    )
    display_name <- dist_names[dname]
    if (is.na(display_name)) display_name <- dname
    params <- as.list(fit$estimate)
    param_str <- paste(names(params), signif(unlist(params), 4), sep = " = ", collapse = ", ")
    k <- length(params)
    aic <- 2 * k - 2 * fit$loglik
    div(class = "dist-summary",
        p(strong("En İyi Dağılım:"), code(display_name)),
        p(strong("Parametreler:"), code(param_str)),
        p(strong("AIC:"), code(round(aic, 2))),
        p(strong("Log-Likelihood:"), code(round(fit$loglik, 2))),
        p(strong("Gözlem sayısı:"), code(length(rv_dist$data$value)))
    )
  })

  output$density_plot_dist <- renderPlotly({
    req(rv_dist$data, rv_dist$fit_results)
    fit_info <- rv_dist$fit_results
    if(is.null(fit_info) || is.null(fit_info$fit)) {
      return(plotly_empty() %>% layout(title = "Dağılım uyumu başarısız"))
    }
    plot_data <- rv_dist$data %>% filter(is.finite(value))
    # Aggregate to daily mean if more than 1000 points
    if(nrow(plot_data) > 1000) {
      plot_data <- plot_data %>%
        group_by(date = as.Date(date)) %>%
        summarise(value = mean(value, na.rm = TRUE))
    }
    if(nrow(plot_data) < 2) {
      return(plotly_empty() %>% layout(title = "Grafik için yeterli veri yok"))
    }
    dens <- density(plot_data$value)
    p <- plot_ly(x = ~dens$x, y = ~dens$y, type = 'scatter', mode = 'lines',
                 name = 'Gözlenen Yoğunluk', line = list(color = 'steelblue'))
    fit <- fit_info$fit
    dname <- fit_info$best_dist
    params <- as.list(fit$estimate)
    xfit <- seq(min(dens$x), max(dens$x), length.out = 200)
    dist_function_map <- list(
      "normal" = "dnorm",
      "lognormal" = "dlnorm",
      "gamma" = "dgamma",
      "weibull" = "dweibull",
      "exponential" = "dexp",
      "logistic" = "dlogis",
      "cauchy" = "dcauchy"
    )
    r_dist_func <- dist_function_map[[dname]]
    if (!is.null(r_dist_func)) {
      yfit <- tryCatch({
        if (dname == "normal") {
          stats::dnorm(xfit, mean = params$mean, sd = params$sd)
        } else if (dname == "lognormal") {
          stats::dlnorm(xfit, meanlog = params$meanlog, sdlog = params$sdlog)
        } else if (dname == "gamma") {
          stats::dgamma(xfit, shape = params$shape, rate = params$rate)
        } else if (dname == "weibull") {
          stats::dweibull(xfit, shape = params$shape, scale = params$scale)
        } else if (dname == "exponential") {
          stats::dexp(xfit, rate = params$rate)
        } else if (dname == "logistic") {
          stats::dlogis(xfit, location = params$location, scale = params$scale)
        } else if (dname == "cauchy") {
          stats::dcauchy(xfit, location = params$location, scale = params$scale)
        } else {
          NULL
        }
      }, error = function(e) NULL)
      if (!is.null(yfit)) {
        scale_factor <- max(dens$y) / max(yfit, na.rm = TRUE)
        yfit <- yfit * scale_factor
        dist_names <- c(
          "normal" = "Normal",
          "lognormal" = "Lognormal",
          "gamma" = "Gamma",
          "weibull" = "Weibull",
          "exponential" = "Üstel",
          "logistic" = "Lojistik",
          "cauchy" = "Cauchy"
        )
        display_name <- dist_names[dname]
        if (is.na(display_name)) display_name <- dname
        p <- p %>% add_trace(x = ~xfit, y = ~yfit, 
                            name = paste("Uydurulan:", display_name),
                            line = list(color = 'green', dash = 'dot'))
      }
    }
    p %>% layout(title = paste("Veri Yoğunluğu ve En İyi Uyan Dağılım -", input$pollutant_select_dist),
                 xaxis = list(title = "Konsantrasyon"),
                 yaxis = list(title = "Yoğunluk"),
                 legend = list(orientation = 'h', y = -0.1))
  })

  output$aic_table <- renderDT({
      req(rv_dist$fit_results)
      aics <- rv_dist$fit_results$all_aic
      if (is.null(aics) || length(aics) == 0) return(datatable(data.frame(Uyarı="AIC değerleri hesaplanamadı.")))
      dist_names <- c(
        "normal" = "Normal",
        "lognormal" = "Lognormal",
        "gamma" = "Gamma",
        "weibull" = "Weibull",
        "exponential" = "Üstel",
        "logistic" = "Lojistik",
        "cauchy" = "Cauchy"
      )
      aic_df <- tibble(
          Distribution = names(aics),
          AIC = round(aics, 2)
      ) %>% 
      mutate(
        Distribution = case_when(
          Distribution %in% names(dist_names) ~ dist_names[Distribution],
          TRUE ~ Distribution
        )
      ) %>%
      arrange(AIC)
      datatable(aic_df, rownames = FALSE,
                options = list(pageLength = 10, dom = 't'),
                caption = "Tüm Başarılı Uydurulan Dağılımlar için AIC Değerleri (Düşük daha iyi)")
  })

  output$data_preview_table <- renderDT({
      req(rv_dist$data)
      preview_data <- head(rv_dist$data, 100) %>%
          mutate(date = format(date, "%Y-%m-%d"), value = round(value, 3))
      datatable(preview_data, rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE, dom = 'tp', serverSide = TRUE),
                caption = "Yüklenen ve Temizlenen Verinin İlk 100 Satırı")
  })

  # --- Outlier Analysis Logic (for Aykırı Değer Analizi tab) ---

  # Helper: validate and clean data for outlier tab
  validate_and_clean_data_outlier <- function(data, pollutant) {
    data %>%
      mutate(
        date  = as_date(date),
        value = suppressWarnings(as.numeric(as.character(.data[[pollutant]])))
      ) %>%
      filter(!is.na(date), is.finite(value)) %>%
      arrange(date) %>%
      # nullify Z-score outliers
      { tmp <- .
        z   <- (tmp$value - mean(tmp$value, na.rm=TRUE)) / sd(tmp$value, na.rm=TRUE)
        tmp %>% mutate(value = ifelse(abs(z) > 3, NA, value))
      } %>%
      select(date, value)
  }

  # Helper: Box-Cox normalization
  normalize_data_outlier <- function(data) {
    if (input$normalize_data && all(data$value > 0)) {
      bc <- bestNormalize::boxcox(data$value)
      data <- data %>% mutate(value_raw = value, value = predict(bc))
      rv$transformation <- list(method = "Box-Cox", lambda = bc$lambda, transformed = TRUE)
    } else {
      rv$transformation <- list(transformed = FALSE)
    }
    data
  }

  # Helper: Outlier detection
  detect_outliers_outlier <- function(data, method, threshold, seasonal, trend) {
    if (nrow(data) == 0) return(data)
    if (seasonal || trend) {
      ts_data <- ts(data$value, frequency = 365)
      decomp <- stl(ts_data, s.window = "periodic", robust = TRUE)
      data <- data %>%
        mutate(trend = as.numeric(decomp$time.series[, "trend"]),
               seasonal = as.numeric(decomp$time.series[, "seasonal"]),
               remainder = as.numeric(decomp$time.series[, "remainder"]))
      rv$decomposition <- decomp
      if (seasonal && trend) {
        data$residual <- data$remainder
      } else if (seasonal) {
        data$residual <- data$value - data$seasonal
      } else if (trend) {
        data$residual <- data$value - data$trend
      }
    } else {
      data$residual <- data$value
    }
    switch(method,
           hampel = { data <- hampel_filter_outlier(data, threshold) },
           gesd   = { data <- gesd_test_outlier(data, threshold) },
           stl_esd = { data <- stl_esd_test_outlier(data, threshold) },
           dbscan = { data <- dbscan_detection_outlier(data, threshold) },
           cdf = { data <- cdf_outlier(data) },
           { data$is_outlier <- FALSE }
    )
    data
  }

  hampel_filter_outlier <- function(data, threshold) {
    data %>%
      mutate(median_val = runmed(residual, k = min(15, floor(n()/2))),
             mad = 1.4826 * median(abs(residual - median_val), na.rm = TRUE),
             is_outlier = abs(residual - median_val) > threshold * mad)
  }
  gesd_test_outlier <- function(data, threshold) {
    tryCatch({
      alpha <- 1 - (threshold / 10)
      out <- gesd(data$residual, alpha = alpha, max_anoms = 0.2)
      data$is_outlier <- FALSE
      if (!is.null(out$anoms)) {
        data$is_outlier[out$anoms$index] <- TRUE
      }
      data
    }, error = function(e) {
      data$is_outlier <- FALSE
      data
    })
  }
  stl_esd_test_outlier <- function(data, threshold) {
    tryCatch({
      ts_data <- ts(data$residual, frequency = 365)
      anomaly_res <- ts_data %>%
        time_decompose(frequency = "auto", trend = "auto") %>%
        anomalize(remainder, method = "gesd", alpha = 1 - (threshold/10), max_anoms = 0.2) %>%
        time_recompose()
      data$is_outlier <- anomaly_res$anomaly == "Yes"
      data
    }, error = function(e) {
      data$is_outlier <- FALSE
      data
    })
  }
  dbscan_detection_outlier <- function(data, threshold) {
    tryCatch({
      scaled_data <- scale(data.frame(time = as.numeric(data$date), value = data$residual))
      eps_val <- threshold * (0.1 * max(dist(scaled_data)))
      db_res <- dbscan(scaled_data, eps = eps_val, minPts = 5)
      data$is_outlier <- db_res$cluster == 0
      data
    }, error = function(e) {
      data$is_outlier <- FALSE
      data
    })
  }

  # 3) Define the CDF–based outlier helper (after your other helpers)
  cdf_outlier <- function(data) {
    x <- data$residual
    fit_info <- fit_best_distribution(x)    # assumes you have fit_best_distribution() in scope
    if (is.null(fit_info)) {
      data$is_outlier <- FALSE
      return(data)
    }
    dname  <- fit_info$best_dist
    params <- fit_info$fit$estimate
    qf <- switch(dname,
      normal      = function(p) qnorm(p,      mean = params["mean"],      sd    = params["sd"]),
      lognormal   = function(p) qlnorm(p,     meanlog = params["meanlog"], sdlog = params["sdlog"]),
      gamma       = function(p) qgamma(p,     shape   = params["shape"],   rate  = params["rate"]),
      weibull     = function(p) qweibull(p,   shape   = params["shape"],   scale = params["scale"]),
      exponential = function(p) qexp(p,       rate    = params["rate"]),
      logistic    = function(p) qlogis(p,     location= params["location"], scale = params["scale"]),
      cauchy      = function(p) qcauchy(p,   location= params["location"], scale = params["scale"]),
      NULL
    )
    if (is.null(qf)) {
      data$is_outlier <- FALSE
      return(data)
    }
    cutoff <- qf(0.95)                      # %95 kesme noktası
    data$is_outlier <- data$residual > cutoff
    data
  }

  # --- Main Outlier Analysis Button ---
  observeEvent(input$analyze_btn, {
    req(input$station_select_outlier, input$pollutant_select_outlier)
    show_modal_progress("Veri yükleniyor ve analiz ediliyor")
    tryCatch({
      # 2. Use date range filter in SQL query
      qry <- sprintf("SELECT \"Tarih\" as date, \"%s\" FROM public.hourly_detail_zcleaned 
                      WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL 
                        AND \"Tarih\" BETWEEN '%s' AND '%s'
                      ORDER BY \"Tarih\"",
                     input$pollutant_select_outlier, input$station_select_outlier, input$pollutant_select_outlier,
                     format(input$date_range_outlier[1]), format(input$date_range_outlier[2]))
      data_raw <- dbGetQuery(con, qry) %>% validate_and_clean_data_outlier(input$pollutant_select_outlier)
      if(nrow(data_raw) < 30) stop("Yeterli sayıda veri yok (en az 30 gereklidir)")
      data_clean <- data_raw %>% normalize_data_outlier()
      rv$station_data <- data_clean
      rv$outliers <- detect_outliers_outlier(
        data = data_clean,
        method = input$method_select,
        threshold = input$sensitivity,
        seasonal = input$seasonal_adjust,
        trend = input$trend_adjust
      )
      remove_modal_progress()

      # --- if selected station is Ankara-KeçiörenSanatoryum show comparison and message ---
      if (input$station_select_outlier == "Ankara-KeçiörenSanatoryum") {
        # count nullified entries
        n_null <- sum(is.na(rv$station_data$value))
        output$ak_nullify_msg <- renderText({
          sprintf("Ankara-KeçiörenSanatoryum: %d hourly values nullified as outliers", n_null)
        })
        output$ak_hourly_comparison_plot <- renderPlotly({
          df <- rv$station_data %>% mutate(date = as.POSIXct(date))
          plot_ly(df, x=~date) %>%
            add_lines(y=~value, name="Cleaned (nullified)", line=list(color="blue")) %>%
            add_markers(data=filter(df, is.na(value)), x=~date, y=0,
                        name="Nullified", marker=list(color="red", size=6)) %>%
            layout(title="Ankara-KeçiörenSanatoryum hourly – nullified outliers",
                   xaxis=list(title="Time"), yaxis=list(title="Value"))
        })
      }
    }, error = function(e) {
      remove_modal_progress()
      showNotification(paste("Hata:", e$message), type = "error")
    })
  })

  # 5. Warn user if too much data is selected
  observe({
    req(input$station_select_outlier, input$pollutant_select_outlier, input$date_range_outlier)
    qry <- sprintf("SELECT COUNT(*) as n FROM public.hourly_detail_zcleaned 
                    WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL 
                      AND \"Tarih\" BETWEEN '%s' AND '%s'",
                   input$station_select_outlier, input$pollutant_select_outlier,
                   format(input$date_range_outlier[1]), format(input$date_range_outlier[2]))
    n <- tryCatch(dbGetQuery(con, qry)$n, error = function(e) 0)
    if (n > 5000) {
      showNotification("Seçilen aralıkta çok fazla veri var, lütfen daha kısa bir tarih aralığı seçin.", type = "warning")
    }
  })

  # --- Batch Outlier Analysis Button ---
  observeEvent(input$batch_btn, {
    req(rv$all_stations)
    show_modal_progress("Tüm istasyonlar toplu işleniyor")
    tryCatch({
      results <- future_map_dfr(rv$all_stations, function(station) {
        inc_modal_progress(which(rv$all_stations == station) / length(rv$all_stations),
                           paste("İşleniyor:", station))
        qry <- sprintf("SELECT \"Tarih\" as date, \"%s\" FROM public.hourly_detail_zcleaned 
                        WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL 
                        AND \"Tarih\" BETWEEN '%s' AND '%s'
                        ORDER BY \"Tarih\"",
                       input$pollutant_select_outlier, station, input$pollutant_select_outlier,
                       format(input$date_range_outlier[1]), format(input$date_range_outlier[2]))
        dt <- dbGetQuery(con, qry) %>% validate_and_clean_data_outlier(input$pollutant_select_outlier)
        if(nrow(dt) < 30) return(tibble())
        dt <- dt %>% normalize_data_outlier()
        ot <- detect_outliers_outlier(dt, input$method_select, input$sensitivity,
                                      input$seasonal_adjust, input$trend_adjust)
        ot %>% mutate(station_id = station) %>% select(station_id, everything())
      }, .options = furrr_options(seed = TRUE))
      rv$outliers <- results
      remove_modal_progress()
      showNotification("Toplu işlem tamamlandı!", type = "message")
    }, error = function(e) {
      remove_modal_progress()
      showNotification(paste("Toplu işlem hatası:", e$message), type = "error")
    })
  })


  output$method_description <- renderUI({
    req(input$method_select)
    desc <- technique_descriptions[[input$method_select]]
    div(class = "method-description",
        h4(class = "method-title", desc$name),
        p(desc$description),
        div(class = "pros-cons",
            div(class = "pros", strong("Avantajlar:"), HTML(gsub("\n", "<br>", desc$pros))),
            div(class = "cons", strong("Dezavantajlar:"), HTML(gsub("\n", "<br>", desc$cons)))
        )
    )
  })

  output$ts_plot <- renderPlotly({
    req(rv$outliers)
    plot_data <- rv$outliers %>% mutate(date = as.Date(date))
    tooltip <- if ("station_id" %in% names(plot_data)) {
      paste0("İstasyon: ", plot_data$station_id, "<br>")
    } else { "" }
    if ("value_raw" %in% names(plot_data)) {
      plot_data <- plot_data %>% mutate(
        tooltip_text = paste("Tarih:", format(date, "%Y-%m-%d"),
                             "<br>Değer:", round(value_raw, 2),
                             "<br>", tooltip,
                             "Yöntem:", input$method_select,
                             if (rv$transformation$transformed)
                               paste("<br>Dönüşüm:", rv$transformation$method, "(λ =", round(rv$transformation$lambda, 2), ")")
                             else ""),
        y_value = value_raw
      )
    } else {
      plot_data <- plot_data %>% mutate(
        tooltip_text = paste("Tarih:", format(date, "%Y-%m-%d"),
                             "<br>Değer:", round(value, 2),
                             "<br>", tooltip,
                             "Yöntem:", input$method_select,
                             if (rv$transformation$transformed)
                               paste("<br>Dönüşüm:", rv$transformation$method, "(λ =", round(rv$transformation$lambda, 2), ")")
                             else ""),
        y_value = value
      )
    }
    non_outlier_data <- plot_data %>% mutate(is_outlier = ifelse(is.na(is_outlier), FALSE, is_outlier))
    non_outlier_data$segment <- cumsum(c(TRUE, diff(non_outlier_data$is_outlier) != 0))
    p <- plot_ly()
    segs <- split(non_outlier_data, non_outlier_data$segment)
    for (seg in segs) {
      if (nrow(seg) > 1 && !any(seg$is_outlier)) {
        p <- p %>% add_trace(
          data = seg,
          x = ~date, y = ~y_value, type = 'scatter', mode = 'lines',
          line = list(color = 'steelblue'),
          name = 'Zaman Serisi (Aykırı Olmayan)',
          text = ~tooltip_text, hoverinfo = 'text',
          showlegend = FALSE
        )
      }
    }
    # Add faint line for all data (including outliers)
    if (nrow(plot_data) > 1) {
      p <- p %>% add_trace(
        data = plot_data,
        x = ~date, y = ~y_value, type = 'scatter', mode = 'lines',
        line = list(color = 'gray', width = 1, dash = 'dot'),
        name = 'Tüm Veri (Referans)',
        text = ~tooltip_text, hoverinfo = 'none', showlegend = TRUE
      )
    }
    # Add legend only once
    first_non_outlier <- non_outlier_data %>% filter(!is_outlier)
    if (nrow(first_non_outlier) > 0) {
      p <- p %>% add_trace(
        data = first_non_outlier[1,],
        x = ~date, y = ~y_value, type = 'scatter', mode = 'lines',
        line = list(color = 'steelblue'),
        name = 'Zaman Serisi (Aykırı Olmayan)',
        text = ~tooltip_text, hoverinfo = 'text',
        showlegend = TRUE
      )
    }
    # Outlier points: markers only, no lines
    if (any(plot_data$is_outlier, na.rm = TRUE)) {
      outlier_data <- filter(plot_data, is_outlier)
      outlier_y <- if ("value_raw" %in% names(outlier_data)) outlier_data$value_raw else outlier_data$value
      p <- p %>% add_trace(
        data = outlier_data,
        x = ~date,
        y = outlier_y,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = 'red', size = 8, symbol = 'circle'),
        name = 'Aykırı Değer',
        text = ~tooltip_text,
        showlegend = TRUE
      )
    }
    p %>% layout(title = paste("Aykırı Değerlerle Zaman Serisi -", input$station_select_outlier),
                 xaxis = list(title = "Tarih"),
                 yaxis = list(title = paste(input$pollutant_select_outlier, "Konsantrasyon")),
                 hovermode = 'x unified',
                 legend = list(orientation = 'h', y = -0.2),
                 margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
                 annotations = list(
                   list(
                     xref = 'paper', yref = 'paper', x = 0, y = 1.08, showarrow = FALSE,
                     text = 'Mavi çizgi: Aykırı olmayan veri | Kırmızı noktalar: Aykırı değerler | Gri kesik çizgi: Tüm veri',
                     font = list(size = 13, color = 'gray')
                   )
                 ))
  })

  output$decomposition_plot <- renderPlotly({
    req(rv$decomposition, rv$station_data)
    decomp_df <- tibble(
      Date = rv$station_data$date,
      seasonal = as.numeric(rv$decomposition$time.series[,"seasonal"]),
      trend = as.numeric(rv$decomposition$time.series[,"trend"]),
      remainder = as.numeric(rv$decomposition$time.series[,"remainder"])
    )
    plot_ly(decomp_df, x = ~Date) %>%
      add_trace(y = ~seasonal, name = 'Mevsimsel', type = 'scatter', mode = 'lines', line = list(color = '#1f77b4')) %>%
      add_trace(y = ~trend, name = 'Trend', type = 'scatter', mode = 'lines', line = list(color = '#ff7f0e')) %>%
      add_trace(y = ~remainder, name = 'Artık', type = 'scatter', mode = 'lines', line = list(color = '#2ca02c')) %>%
      layout(title = "STL Ayrıştırma",
             yaxis = list(title = "Bileşen Değeri"),
             legend = list(orientation = 'h', y = -0.2),
             hovermode = 'x unified')
  })

  output$residual_plot <- renderPlotly({
    req(rv$outliers)
    plot_data <- rv$outliers %>% filter(!is.na(residual), is.finite(residual))
    p <- plot_ly(plot_data, x = ~date, y = ~residual, type = 'scatter', mode = 'lines',
                 line = list(color = 'gray'), name = 'Artıklar')
    if (any(plot_data$is_outlier, na.rm = TRUE)) {
      outlier_data <- filter(plot_data, is_outlier)
      p <- p %>% add_trace(data = outlier_data, x = ~date, y = ~residual, type = 'scatter',
                           mode = 'markers', marker = list(color = 'red', size = 8),
                           name = 'Aykırı Değer')
    }
    p %>% layout(title = "Aykırı Değerlerle Artıklar",
                 xaxis = list(title = "Tarih"),
                 yaxis = list(title = "Artık Değer"),
                 hovermode = 'x unified',
                 shapes = list(
                   list(type = 'line', x0 = min(plot_data$date), x1 = max(plot_data$date),
                        y0 = 0, y1 = 0, line = list(dash = 'dash'))
                 ))
  })

  output$boxplot <- renderPlotly({
    req(rv$station_data)
    pd <- rv$station_data %>% filter(!is.na(value), is.finite(value))
    y_vals <- if ("value_raw" %in% names(pd)) pd$value_raw else pd$value
    plot_ly(data = pd, y = y_vals, type = 'box', boxpoints = 'outliers',
            marker = list(color = 'red'), line = list(color = 'steelblue'),
            name = input$pollutant_select_outlier) %>%
      layout(title = paste("Dağılım -", input$pollutant_select_outlier),
             yaxis = list(title = "Konsantrasyon"))
  })

  output$density_plot <- renderPlotly({
    req(rv$station_data)
    pd <- rv$station_data %>% filter(!is.na(value), is.finite(value))
    if(nrow(pd) < 2) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "Yeterli veri bulunamadı"))
    dens1 <- density(if("value_raw" %in% names(pd)) pd$value_raw else pd$value)
    p <- plot_ly(x = ~dens1$x, y = ~dens1$y, type = 'scatter', mode = 'lines',
                 name = 'Orijinal', line = list(color = 'steelblue'))
    if("value_raw" %in% names(pd)) {
      dens2 <- density(pd$value)
      p <- p %>% add_trace(x = ~dens2$x, y = ~dens2$y, type = 'scatter', mode = 'lines',
                           name = 'Dönüştürülmüş', line = list(color = 'red'))
    }
    p %>% layout(title = paste("Yoğunluk -", input$pollutant_select_outlier),
                 xaxis = list(title = "Konsantrasyon"),
                 yaxis = list(title = "Yoğunluk"),
                 legend = list(orientation = 'h'))
  })

  output$acf_plot <- renderPlotly({
    req(rv$station_data)
    pd <- rv$station_data %>% filter(!is.na(value), is.finite(value))
    if(nrow(pd) < 2) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "ACF için yeterli veri yok"))
    }
    acf_obj <- acf(pd$value, plot = FALSE)
    acf_df <- with(acf_obj, data.frame(lag, acf))
    ci <- qnorm(0.975) / sqrt(nrow(pd))
    plot_ly(acf_df, x = ~lag, y = ~acf, type = 'bar', mode = 'markers') %>% 
      layout(title = "Otokorelasyon Fonksiyonu (ACF)",
             xaxis = list(title = "Gecikme"),
             yaxis = list(title = "ACF"),
             shapes = list(
               list(type = 'line', x0 = min(acf_df$lag), x1 = max(acf_df$lag),
                    y0 = ci, y1 = ci, line = list(dash = 'dash', color = 'blue')),
               list(type = 'line', x0 = min(acf_df$lag), x1 = max(acf_df$lag),
                    y0 = -ci, y1 = -ci, line = list(dash = 'dash', color = 'blue'))
             ))
  })

  output$transformed_plot <- renderPlotly({
    req(rv$station_data)
    pd <- rv$station_data %>% filter(!is.na(value), is.finite(value))
    if(nrow(pd) < 2 || !all(pd$value > 0) || !rv$transformation$transformed) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "Dönüşüm uygulanmadı veya veri uygun değil"))
    }
    p1 <- plot_ly(pd, x = ~date, y = ~value_raw, type = 'scatter', mode = 'lines',
                  name = 'Orijinal') %>% layout(yaxis = list(title = "Orijinal Değer"))
    p2 <- plot_ly(pd, x = ~date, y = ~value, type = 'scatter', mode = 'lines',
                  name = 'Dönüştürülmüş') %>% layout(yaxis = list(title = paste("Box-Cox (λ =", round(rv$transformation$lambda, 2), ")")))
    subplot(p1, p2, nrows = 2, shareX = TRUE) %>% layout(title = "Normalleştirme Dönüşümü Karşılaştırması")
  })

  output$model_diagnostics <- renderPrint({
    req(rv$station_data)
    pd <- rv$station_data %>% filter(!is.na(value), is.finite(value))
    if(nrow(pd) < 2) {
      cat("Tanısal analizler için yeterli veri yok.\n")
      return()
    }
    cat("=== Zaman Serisi Tanısal Analizleri ===\n\n")
    cat("İstasyon:", input$station_select_outlier, "\n")
    cat("Kirletici:", input$pollutant_select_outlier, "\n")
    cat("Zaman Aralığı:", format(min(pd$date), "%Y-%m-%d"), "ile", format(max(pd$date), "%Y-%m-%d"), "\n")
    cat("Geçerli Gözlem Sayısı:", nrow(pd), "\n\n")
    cat("Temel İstatistikler:\n")
    print(summary(if("value_raw" %in% names(pd)) pd$value_raw else pd$value))
    cat("\nNormallik Testleri:\n")
    if(nrow(pd) <= 5000) {
      sw_test <- shapiro.test(pd$value)
      cat("Shapiro-Wilk p-değeri:", format.pval(sw_test$p.value), ifelse(sw_test$p.value < 0.05, "→ Normal değil", "→ Normal"), "\n")
    } else {
      cat("Shapiro-Wilk testi yapılmadı (örnek boyutu > 5000)\n")
    }
    ad_test <- ad.test(pd$value)
    cat("Anderson-Darling p-değeri:", format.pval(ad_test$p.value), ifelse(ad_test$p.value < 0.05, "→ Normal değil", "→ Normal"), "\n")
    ks_test <- ks.test(jitter(scale(pd$value)), "pnorm")
    cat("Kolmogorov-Smirnov p-değeri:", format.pval(ks_test$p.value), ifelse(ks_test$p.value < 0.05, "→ Normal değil", "→ Normal"), "\n")
    cat("\nDağılım Şekli Analizi:\n")
    skew_val <- moments::skewness(pd$value, na.rm = TRUE)
    kurt_val <- moments::kurtosis(pd$value, na.rm = TRUE)
    cat("Çarpıklık:", round(skew_val, 3), "\n")
    cat("Basıklık:", round(kurt_val, 3), "\n")
    if(rv$transformation$transformed) {
      cat("\nNormalleştirme Sonrası Testler:\n")
      ad_test_trans <- ad.test(pd$value)
      cat("Anderson-Darling p-değeri (dönüştürülmüş):", format.pval(ad_test_trans$p.value),
          ifelse(ad_test_trans$p.value < 0.05, "→ Hala normal değil", "→ Normalleşti"), "\n")
      cat("Çarpıklık (dönüştürülmüş):", round(moments::skewness(pd$value, na.rm = TRUE), 3), "\n")
      cat("Basıklık (dönüştürülmüş):", round(moments::kurtosis(pd$value, na.rm = TRUE), 3), "\n")
    }
    cat("\nDurağanlık Testleri:\n")
    adf_test <- suppressWarnings(adf.test(na.omit(pd$value)))
    cat("ADF p-değeri:", format.pval(adf_test$p.value), ifelse(adf_test$p.value < 0.05, "→ Durağan", "→ Durağan değil"), "\n")
    kpss_test <- suppressWarnings(kpss.test(na.omit(pd$value)))
    cat("KPSS p-değeri:", format.pval(kpss_test$p.value), ifelse(kpss_test$p.value < 0.05, "→ Durağan değil", "→ Durağan"), "\n")
    cat("\nOtokorelasyon Analizi:\n")
    lb_test <- Box.test(pd$value, lag = 20, type = "Ljung-Box")
    cat("Ljung-Box p-değeri:", format.pval(lb_test$p.value), ifelse(lb_test$p.value < 0.05, "→ Otokorelasyon var", "→ Otokorelasyon yok"), "\n")
  })

  # 6. For all other data tables (DTOutput), add serverSide = TRUE and limit rows
  # Example for outlier_table:
  output$outlier_table <- renderDT({
    req(rv$outliers)
    ot_data <- rv$outliers %>% filter(is_outlier) %>%
      mutate(date = format(date, "%Y-%m-%d"),
             value = round(if("value_raw" %in% names(.)) value_raw else value, 2))
    if ("station_id" %in% names(ot_data)) {
      ot_data <- ot_data %>% select(date, value, station_id)
      col_names <- c("Tarih", "Değer", "İstasyon ID")
    } else {
      ot_data <- ot_data %>% select(date, value)
      col_names <- c("Tarih", "Değer")
    }
    datatable(ot_data, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'),
                             serverSide = TRUE),
              extensions = 'Buttons',
              colnames = col_names,
              caption = "Tespit Edilen Aykırı Değerler")
  })

  output$lognorm_cutoff_plot <- renderPlotly({
    # dynamic CDF & cutoff using best-fit distribution
    pd <- rv$station_data %>% filter(!is.na(value))
    if(nrow(pd) < 5) return(plotly_empty() %>% layout(title = "Yeterli veri yok"))
    fb <- fit_best_distribution(pd$value)
    if(is.null(fb) || is.null(fb$fit)) {
      return(plotly_empty() %>% layout(title = "Dağılım uydurulamadı"))
    }
    params <- fb$fit$estimate
    dist_name <- fb$best_dist
    # prepare CDF data
    xfit <- seq(min(pd$value, na.rm=TRUE), max(pd$value, na.rm=TRUE), length.out = 200)
    emp_cdf  <- ecdf(pd$value)(xfit)
    theo_cdf <- switch(dist_name,
      normal      = pnorm(xfit,      mean = params["mean"],       sd = params["sd"]),
      lognormal   = plnorm(xfit,     meanlog = params["meanlog"], sdlog = params["sdlog"]),
      gamma       = pgamma(xfit,     shape = params["shape"],     rate = params["rate"]),
      weibull     = pweibull(xfit,   shape = params["shape"],     scale = params["scale"]),
      exponential = pexp(xfit,       rate = params["rate"]),
      logistic    = plogis(xfit,     location = params["location"], scale = params["scale"]),
      cauchy      = pcauchy(xfit,    location = params["location"], scale = params["scale"]),
      emp_cdf)
    cutoff95 <- switch(dist_name,
      normal      = qnorm(0.95,      mean = params["mean"],       sd = params["sd"]),
      lognormal   = qlnorm(0.95,     meanlog = params["meanlog"], sdlog = params["sdlog"]),
      gamma       = qgamma(0.95,     shape = params["shape"],     rate = params["rate"]),
      weibull     = qweibull(0.95,   shape = params["shape"],     scale = params["scale"]),
      exponential = qexp(0.95,       rate = params["rate"]),
      logistic    = qlogis(0.95,     location = params["location"], scale = params["scale"]),
      cauchy      = qcauchy(0.95,    location = params["location"], scale = params["scale"]),
      NA)
    dist_label <- tools::toTitleCase(dist_name)
    plot_ly() %>%
      add_trace(x = ~xfit, y = ~emp_cdf,
                type = 'scatter', mode = 'lines',
                name = 'Empirik CDF', line = list(color = 'steelblue')) %>%
      add_trace(x = ~xfit, y = ~theo_cdf,
                type = 'scatter', mode = 'lines',
                name = paste0(dist_label, ' CDF'),
                line = list(color = 'firebrick', dash = 'dash')) %>%
      layout(
        title = paste('Empirik vs.', dist_label, 'CDF (95% Kesme)'),
        xaxis = list(title = 'Değer'),
        yaxis = list(title = 'CDF'),
        shapes = list(
          list(type = 'line', x0 = cutoff95, x1 = cutoff95,
               y0 = 0, y1 = 1, line = list(dash = 'dot', color = 'black'))
        )
      )
  })

  output$stats_summary <- renderPrint({
    req(rv$outliers)
    pd <- rv$outliers %>% filter(!is.na(value), is.finite(value))
    cat("=== İstatistiksel Özet ===\n\n")
    cat("İstasyon:", if("station_id" %in% names(pd)) unique(pd$station_id) else input$station_select_outlier, "\n")
    cat("Kirletici:", input$pollutant_select_outlier, "\n")
    cat("Zaman Aralığı:", format(min(pd$date), "%Y-%m-%d"), "ile", format(max(pd$date), "%Y-%m-%d"), "\n")
    cat("Geçerli Gözlem Sayısı:", nrow(pd), "\n\n")
    if(rv$transformation$transformed) {
      cat("Uygulanan Dönüşüm: Box-Cox (lambda =", round(rv$transformation$lambda, 4), ")\n\n")
    }
    cat("Toplam Aykırı Değer:", sum(pd$is_outlier, na.rm = TRUE), "\n")
    cat("Aykırı Değer Yüzdesi:", round(mean(pd$is_outlier, na.rm = TRUE) * 100, 2), "%\n\n")
    if(sum(pd$is_outlier, na.rm = TRUE) > 0) {
      cat("Aykırı Değer İstatistikleri:\n")
      outlier_stats <- pd %>% filter(is_outlier) %>%
        summarise(Min = format(min(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  Q1 = format(quantile(if("value_raw" %in% names(.)) value_raw else value, 0.25, na.rm = TRUE), digits = 6),
                  Medyan = format(median(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  Ortalama = format(mean(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  Q3 = format(quantile(if("value_raw" %in% names(.)) value_raw else value, 0.75, na.rm = TRUE), digits = 6),
                  Maks = format(max(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  SS = format(sd(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6))
      print(outlier_stats)
      cat("\nAykırı Olmayan Değer İstatistikleri:\n")
      non_outlier_stats <- pd %>% filter(!is_outlier) %>%
        summarise(Min = format(min(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  Q1 = format(quantile(if("value_raw" %in% names(.)) value_raw else value, 0.25, na.rm = TRUE), digits = 6),
                  Medyan = format(median(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  Ortalama = format(mean(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  Q3 = format(quantile(if("value_raw" %in% names(.)) value_raw else value, 0.75, na.rm = TRUE), digits = 6),
                  Maks = format(max(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6),
                  SS = format(sd(if("value_raw" %in% names(.)) value_raw else value, na.rm = TRUE), digits = 6))
      print(non_outlier_stats)
    }
  })

  # main download button
  output$download_btn <- downloadHandler(
    filename = function() {
      station <- if("station_id" %in% names(rv$outliers)) "tum_istasyonlar" else input$station_select_outlier
      paste0("hava_kirliligi_aykira_degerler_", station, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Dosya indiriliyor...", value = 0, {
        write_csv(rv$outliers, file)
        incProgress(1)
      })
    }
  )

  # Top5 Excel download
  output$download_top5_outliers <- downloadHandler(
    filename = function() paste0("top5_outliers_", Sys.Date(), ".xlsx"),
    content = function(file) {
      withProgress(message = "Top5 dosyası indiriliyor...", value = 0, {
        stations   <- top5_stations()
        pollutants <- c("PM10","PM25","SO2","NO2")
        sheets     <- list()

        stats_list <- list()
        for(st in stations) {
          st_mod <- st %>% gsub("/", "_", .) %>% gsub(" ", "", .) %>% gsub("\\.", "", .)
          for(p in pollutants) {
            # 1) Pull raw values for overall stats
            qry_raw <- sprintf(
              "SELECT \"%s\" AS value
               FROM public.hourly_detail
               WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL",
              p, st_mod, p
            )
            df_raw <- tryCatch(dbGetQuery(con, qry_raw), error = function(e) NULL)

            # initialize stats
            current_mean_all   <- NA_real_; current_median_all <- NA_real_; current_sd_all     <- NA_real_
            current_mean_no_z  <- NA_real_; current_median_no_z<- NA_real_; current_sd_no_z    <- NA_real_

            if (!is.null(df_raw) && "value" %in% names(df_raw)) {
              vals <- df_raw$value %>% as.numeric() %>% na.omit()
              if (length(vals) > 0) {
                current_mean_all   <- mean(vals, na.rm = TRUE)
                current_median_all <- median(vals, na.rm = TRUE)
                if (length(vals) >= 2) current_sd_all <- sd(vals, na.rm = TRUE)

                # 2) remove Z-score outliers from raw vals
                if (length(vals) >= 2 && !is.na(current_sd_all) && current_sd_all > 0) {
                  zflags <- abs((vals - current_mean_all) / current_sd_all) > 3
                  vals_noz <- vals[!zflags]
                } else {
                  vals_noz <- vals
                }
                if (length(vals_noz) > 0) {
                  current_mean_no_z   <- mean(vals_noz, na.rm = TRUE)
                  current_median_no_z <- median(vals_noz, na.rm = TRUE)
                  if (length(vals_noz) >= 2) current_sd_no_z <- sd(vals_noz, na.rm = TRUE)
                }
              }
            }

            stats_list[[paste(st,p)]] <- tibble(
              station      = st,
              pollutant    = p,
              mean_all     = current_mean_all,
              median_all   = current_median_all,
              sd_all       = current_sd_all,
              mean_no_z    = current_mean_no_z,
              median_no_z  = current_median_no_z,
              sd_no_z      = current_sd_no_z
            )
          }
        }
        sheets[["Summary"]] <- bind_rows(stats_list)

        for (station in stations) {
          station_mod <- station %>%
            gsub("/", "_", .) %>%
            gsub(" ", "", .) %>%
            gsub("\\.", "", .)

          out_list <- list()
          for (p in pollutants) {
            qry <- sprintf(
              "SELECT to_char(timezone('Europe/Istanbul', \"Tarih\"), 'MM/DD/YYYY HH24:MI:SS') as date, \"%s\" as value
               FROM public.hourly_detail_zcleaned
               WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL
               ORDER BY \"Tarih\"",
              p, station_mod, p
            )

            data_raw <- tryCatch(dbGetQuery(con, qry), error = function(e) NULL)
            if (is.null(data_raw) || !"value" %in% names(data_raw)) next

            data_clean <- data_raw %>%
              rename(timestamp = date) %>%   # preserve original time string
              mutate(value = as.numeric(value)) %>%
              filter(!is.na(value))

            # identify Z-score outliers
            if (nrow(data_clean) > 0) {
              out_df <- data_clean %>%
                mutate(zscore = as.numeric(scale(value))) %>%
                filter(abs(zscore) > 3) %>%
                select(timestamp, value) %>%
                mutate(pollutant = p) %>%
                select(pollutant, timestamp, value)
              if (nrow(out_df) > 0) out_list[[p]] <- out_df
            }
          }

          if (length(out_list) > 0) {
            sheets[[station]] <- bind_rows(out_list) %>%
              pivot_wider(
                id_cols      = timestamp,
                names_from   = pollutant,
                values_from  = value,
                values_fill  = NA
              ) %>%
              arrange(timestamp)
          } else {
            sheets[[station]] <- tibble(
              Note = sprintf("No Z-score (±3σ) outliers for %s", station)
            )
          }
        }

        writexl::write_xlsx(sheets, path = file)
        incProgress(1)
      })
    }
  )

  # --- Top 5 Station Table Logic ---
  top5_stations <- reactiveVal(NULL)

  output$top5_station_table <- renderDT({
    qry <- "
      SELECT *
      FROM public.daily_75_pm10_pm25_so2_no2
      ORDER BY overall DESC
      LIMIT 5
    "
    df <- tryCatch({
      dbGetQuery(con, qry)
    }, error = function(e) {
      data.frame(Hata = paste("Veri alınamadı:", e$message))
    })

    station_col <- grep("station|istasyon", names(df), ignore.case = TRUE, value = TRUE)
    show_cols <- c(station_col, "overall")
    show_cols <- show_cols %in% names(df)
    top5_stations(as.character(df[[station_col]]))
    datatable(df[, show_cols, drop = FALSE], rownames = FALSE,
              selection = "single",
              options = list(pageLength = 5),
              caption = "En yüksek 5 istasyon (overall sütununa göre)")
  })

  # --- Batch Analysis for Top 5 Stations ---
  top5_analysis <- reactiveVal(NULL)

  observeEvent(top5_stations(), {
    stations <- top5_stations()
    req(stations)
    pollutants <- c("PM10", "PM25", "SO2", "NO2")
    results <- list() 

    for (station in stations) {
      station_mod <- station
      station_mod <- gsub("/", "_", station_mod)
      station_mod <- gsub(" ", "", station_mod)
      station_mod <- gsub("\\.", "", station_mod)
      row <- list(station = station) 

      for (pollutant in pollutants) {
        qry <- sprintf(
          "SELECT \"Tarih\" as date, \"%s\" as value FROM public.hourly_detail_zcleaned 
           WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL 
           ORDER BY \"Tarih\"",
          pollutant, station_mod, pollutant
        )
        message("Query for station_mod: ", station_mod, " (original: ", station, ") pollutant: ", pollutant)
        data_raw <- tryCatch(dbGetQuery(con, qry), error = function(e) {
          message("DB error: ", e$message)
          NULL
        })

        # Initialize pollutant-specific fields in row to NA
        row[[paste0(pollutant, "_n")]] <- NA_integer_
        row[[paste0(pollutant, "_mean")]] <- NA_real_
        row[[paste0(pollutant, "_sd")]] <- NA_real_
        row[[paste0(pollutant, "_min")]] <- NA_real_
        row[[paste0(pollutant, "_max")]] <- NA_real_
        row[[paste0(pollutant, "_outlier_count")]] <- NA_integer_
        row[[paste0(pollutant, "_outlier_pct")]] <- NA_real_
        row[[paste0(pollutant, "_fit_dist")]] <- NA_character_
        row[[paste0(pollutant, "_dist_outlier_pct")]] <- NA_real_
        row[[paste0(pollutant, "_dist_outlier99_pct")]] <- NA_real_
       

        if (is.null(data_raw) || !"value" %in% names(data_raw) || all(is.na(data_raw$value))) {
          next
        }

        data_clean <- data_raw %>%
          rename(timestamp = date) %>%
          mutate(value = as.numeric(value)) %>%
          filter(!is.na(value))

        if (nrow(data_clean) < 30) {
          next
        }

        row[[paste0(pollutant, "_n")]] <- nrow(data_clean)
        row[[paste0(pollutant, "_mean")]] <- round(mean(data_clean$value, na.rm = TRUE), 2)
        current_sd <- sd(data_clean$value, na.rm = TRUE)
        row[[paste0(pollutant, "_sd")]] <- if (!is.na(current_sd)) round(current_sd, 2) else NA_real_
        row[[paste0(pollutant, "_min")]] <- round(min(data_clean$value, na.rm = TRUE), 2)
        row[[paste0(pollutant, "_max")]] <- round(max(data_clean$value, na.rm = TRUE), 2)

        # Z-score outliers
        if (nrow(data_clean) >= 2 && !is.na(current_sd) && current_sd > 0) {
            outlier_count <- sum(abs(scale(data_clean$value)) > 3, na.rm = TRUE)
            outlier_pct <- round(100 * outlier_count / nrow(data_clean), 2)
            row[[paste0(pollutant, "_outlier_count")]] <- outlier_count
            row[[paste0(pollutant, "_outlier_pct")]] <- outlier_pct
        } else {
            row[[paste0(pollutant, "_outlier_count")]] <- 0 # Or NA_integer_ if preferred when sd is 0/NA
            row[[paste0(pollutant, "_outlier_pct")]] <- 0   # Or NA_real_
        }
        
        # Add distribution-based outlier metrics
        fb <- fit_best_distribution(data_clean$value)
        if (!is.null(fb) && !is.null(fb$fit) && !is.null(fb$fit$estimate)) {
          row[[paste0(pollutant, "_fit_dist")]] <- fb$best_dist
          params <- fb$fit$estimate
          qf <- switch(fb$best_dist,
            normal      = function(p_val) qnorm(p_val, mean = params["mean"], sd = params["sd"]),
            lognormal   = function(p_val) qlnorm(p_val, meanlog = params["meanlog"], sdlog = params["sdlog"]),
            gamma       = function(p_val) qgamma(p_val, shape = params["shape"], rate = params["rate"]),
            weibull     = function(p_val) qweibull(p_val, shape = params["shape"], scale = params["scale"]),
            exponential = function(p_val) qexp(p_val, rate = params["rate"]),
            logistic    = function(p_val) qlogis(p_val, location = params["location"], scale = params["scale"]),
            cauchy      = function(p_val) qcauchy(p_val, location = params["location"], scale = params["scale"]),
            NULL
          )
          if (!is.null(qf)) {
            cutoff95 <- tryCatch(qf(0.95), error = function(e) NA_real_)
            cutoff99 <- tryCatch(qf(0.99), error = function(e) NA_real_)
            
            if(!is.na(cutoff95)) {
              row[[paste0(pollutant, "_dist_outlier_pct")]] <-
                round(100 * sum(data_clean$value > cutoff95, na.rm = TRUE) / nrow(data_clean), 2)
            }
            if(!is.na(cutoff99)) {
              row[[paste0(pollutant, "_dist_outlier99_pct")]] <-
                round(100 * sum(data_clean$value > cutoff99, na.rm = TRUE) / nrow(data_clean), 2)
            }
          }
        } else {
          # Distribution fitting failed, ensure relevant fields are NA
          row[[paste0(pollutant, "_fit_dist")]] <- NA_character_
          row[[paste0(pollutant, "_dist_outlier_pct")]] <- NA_real_
          row[[paste0(pollutant, "_dist_outlier99_pct")]] <- NA_real_
        }
      } # End for (pollutant in pollutants) loop
      
      results[[length(results) + 1]] <- row # Add the completed row for the station to the results list
    } # End for (station in stations) loop

    if (length(results) == 0) {
      top5_analysis(NULL)
    } else {
      top5_analysis(bind_rows(results))
    }
  }) # End observeEvent

  output$top5_comparison_table <- renderDT({
    df <- top5_analysis()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(Uyarı="Analiz sonuçları yok")))
    # Show only summary columns for each pollutant
    cols <- c("station")
    for (p in c("PM10", "PM25", "SO2", "NO2")) {
      cols <- c(
        cols,
        paste0(p, "_mean"),
        paste0(p, "_outlier_pct"),
        paste0(p, "_fit_dist"),
        paste0(p, "_dist_outlier_pct"),
        paste0(p, "_dist_outlier99_pct"),
        paste0(p, "_iqr_outlier_pct")
      )
    }
    cols <- cols[cols %in% names(df)]
    colnames_map <- c(
      station = "İstasyon",
      PM10_mean = "PM10 Ortalama",
      PM10_outlier_pct = "PM10 Z-Score Aykırı (%)",
      PM10_fit_dist = "PM10 En İyi Dağılım",
      PM10_dist_outlier_pct = "PM10 CDF Aykırı 95% (%)",
      PM10_dist_outlier99_pct = "PM10 CDF Aykırı 99% (%)",
      PM10_iqr_outlier_pct = "PM10 IQR Aykırı (%)",
      PM25_mean = "PM25 Ortalama",
      PM25_outlier_pct = "PM25 Z-Score Aykırı (%)",
      PM25_fit_dist = "PM25 En İyi Dağılım",
      PM25_dist_outlier_pct = "PM25 CDF Aykırı 95% (%)",
      PM25_dist_outlier99_pct = "PM25 CDF Aykırı 99% (%)",
      PM25_iqr_outlier_pct = "PM25 IQR Aykırı (%)",
      SO2_mean = "SO2 Ortalama",
      SO2_outlier_pct = "SO2 Z-Score Aykırı (%)",
      SO2_fit_dist = "SO2 En İyi Dağılım",
      SO2_dist_outlier_pct = "SO2 CDF Aykırı 95% (%)",
      SO2_dist_outlier99_pct = "SO2 CDF Aykırı 99% (%)",
      SO2_iqr_outlier_pct = "SO2 IQR Aykırı (%)",
      NO2_mean = "NO2 Ortalama",
      NO2_outlier_pct = "NO2 Z-Score Aykırı (%)",
      NO2_fit_dist = "NO2 En İyi Dağılım",
      NO2_dist_outlier_pct = "NO2 CDF Aykırı 95% (%)",
      NO2_dist_outlier99_pct = "NO2 CDF Aykırı 99% (%)",
      NO2_iqr_outlier_pct = "NO2 IQR Aykırı (%)"
    )
    datatable(
      df[, cols, drop = FALSE],
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 5,
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      colnames = unname(colnames_map[cols])
    ) %>%
      formatStyle(
        columns = names(df[, cols, drop = FALSE]),
        fontSize = '15px'
      ) %>%
      htmlwidgets::prependContent(
        htmltools::tags$div(
          style = "background:#f8f9fa; border-radius:6px; padding:10px 18px; margin-bottom:10px; font-size:1em; color:#333;",
          htmltools::tags$b(icon("info-circle"), " Açıklama:"),
          htmltools::tags$ul(
            htmltools::tags$li(htmltools::tags$b("Z-Score Aykırı (%)"), ": ±3σ dışında kalan verinin yüzdesi"),
            htmltools::tags$li(htmltools::tags$b("CDF Aykırı 95% (%)"), ": En iyi uyan dağılım modelinin %95 kesme noktasını aşan verinin yüzdesi"),
            htmltools::tags$li(htmltools::tags$b("CDF Aykırı 99% (%)"), ": En iyi uyan dağılım modelinin %99 kesme noktasını aşan verinin yüzdesi"),
            htmltools::tags$li(htmltools::tags$b("IQR Aykırı (%)"), ": Q3 + 1.5*IQR üstünde kalan verinin yüzdesi")
          )
        )
      )
  })

  # Add plotly outputs for top 5 station graphs
  output$top5_zscore_outlier_plot <- renderPlotly({
    df <- top5_analysis()
    s  <- input$top5_comparison_table_rows_selected
    if (is.null(df) || length(s)==0) return(plotly_empty() %>% layout(title = "İstasyon seçilmedi veya veri yok"))
    station     <- df$station[s]
    station_mod <- gsub("/", "_", gsub(" ", "", gsub("\\.", "", station)))
    all_pollutants_for_palette <- c("PM10", "PM25", "SO2", "NO2")
    sel_pollutants <- input$top5_selected_pollutants
    if (is.null(sel_pollutants) || length(sel_pollutants) == 0) {
      return(plotly_empty() %>% layout(title = "Grafik için kirletici seçin"))
    }
    pal_colors <- RColorBrewer::brewer.pal(length(all_pollutants_for_palette), "Set1")
    names(pal_colors) <- all_pollutants_for_palette
    p <- plot_ly()
    for(pollutant_selected in sel_pollutants) {
      qry <- sprintf(
        "SELECT \"Tarih\" as date, \"%s\" as value FROM public.hourly_detail WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL ORDER BY \"Tarih\"",
        pollutant_selected, station_mod, pollutant_selected
      )
      data_raw <- tryCatch(dbGetQuery(con, qry), error = function(e) NULL)
      if (is.null(data_raw) || !"value" %in% names(data_raw) || all(is.na(data_raw$value))) next
      data_clean <- data_raw %>%
        rename(timestamp = date) %>%   # keep original format
        mutate(value = as.numeric(value)) %>%
        filter(!is.na(value))
      if (nrow(data_clean) < 30) next
      data_clean$outlier_z <- abs(scale(data_clean$value)) > 3
      # Baseline line for all data of the current pollutant
      p <- p %>% add_trace(
        data = data_clean, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
        line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
        name = paste0(pollutant_selected, " (Tüm Veri)"), showlegend = FALSE, legendgroup = pollutant_selected
      )
      # Z-score outlier points for the current pollutant
      z_out_data <- data_clean %>% filter(outlier_z)
      if(nrow(z_out_data) > 0){
        p <- p %>% add_trace(
          data = z_out_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'markers',
          marker = list(color = pal_colors[pollutant_selected], size = 8, symbol = 'circle'),
          name = paste0(pollutant_selected, " Z-Score Aykırı"), showlegend = TRUE, legendgroup = pollutant_selected
        )
      }
      if (nrow(data_clean) > 0 && !any(data_clean$outlier_z)) {
         p <- p %>% add_trace(
            data = data_clean[1,],
            x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
            line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
            name = paste0(pollutant_selected, " (Veri)"), showlegend = TRUE, legendgroup = pollutant_selected,
            visible = "legendonly" 
         )
      }
    }
    p %>% layout(
      title = paste0(station, " - Z-Score Aykırı Değerler (±3σ)"),
      xaxis = list(title = "Tarih"),
      yaxis = list(title = "Konsantrasyon"),
      legend = list(orientation = 'h', y = -0.2, tracegroupgap = 10)
    )
  })

  output$top5_cdf95_outlier_plot <- renderPlotly({
    df <- top5_analysis()
    s  <- input$top5_comparison_table_rows_selected
    if (is.null(df) || length(s)==0) return(plotly_empty() %>% layout(title = "İstasyon seçilmedi veya veri yok"))
    station     <- df$station[s]
    station_mod <- gsub("/", "_", gsub(" ", "", gsub("\\.", "", station)))
    all_pollutants_for_palette <- c("PM10", "PM25", "SO2", "NO2")
    sel_pollutants <- input$top5_selected_pollutants
    if (is.null(sel_pollutants) || length(sel_pollutants) == 0) {
      return(plotly_empty() %>% layout(title = "Grafik için kirletici seçin"))
    }
    pal_colors <- RColorBrewer::brewer.pal(length(all_pollutants_for_palette), "Set1")
    names(pal_colors) <- all_pollutants_for_palette
    p <- plot_ly()
    for(pollutant_selected in sel_pollutants) {
      qry <- sprintf(
        "SELECT \"Tarih\" as date, \"%s\" as value FROM public.hourly_detail WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL ORDER BY \"Tarih\"",
        pollutant_selected, station_mod, pollutant_selected
      )
      data_raw <- tryCatch(dbGetQuery(con, qry), error = function(e) NULL)
      if (is.null(data_raw) || !"value" %in% names(data_raw) || all(is.na(data_raw$value))) next
      data_clean <- data_raw %>%
        rename(timestamp = date) %>%
        mutate(value = as.numeric(value)) %>%
        filter(!is.na(value))
      if (nrow(data_clean) < 30) next
      fb <- fit_best_distribution(data_clean$value)
      data_clean$outlier_cdf95 <- FALSE
      if (!is.null(fb) && !is.null(fb$fit) && !is.null(fb$fit$estimate)) {
        p95 <- switch(fb$best_dist,
          normal      = qnorm(0.95, mean=fb$fit$estimate["mean"], sd=fb$fit$estimate["sd"]),
          lognormal   = qlnorm(0.95, meanlog=fb$fit$estimate["meanlog"], sdlog=fb$fit$estimate["sdlog"]),
          gamma       = qgamma(0.95, shape=fb$fit$estimate["shape"], rate=fb$fit$estimate["rate"]),
          weibull     = qweibull(0.95, shape=fb$fit$estimate["shape"], scale=fb$fit$estimate["scale"]),
          exponential = qexp(0.95, rate=fb$fit$estimate["rate"]),
          logistic    = qlogis(0.95, location=fb$fit$estimate["location"], scale=fb$fit$estimate["scale"]),
          cauchy      = qcauchy(0.95, location=fb$fit$estimate["location"], scale=fb$fit$estimate["scale"]),
          NULL
        )
        if (!is.na(p95)) data_clean$outlier_cdf95 <- data_clean$value > p95
      }
      # Baseline line for all data of the current pollutant
      p <- p %>% add_trace(
        data = data_clean, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
        line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
        name = paste0(pollutant_selected, " (Tüm Veri)"), showlegend = FALSE, legendgroup = pollutant_selected
      )
      # CDF 95% outlier points
      cdf95_out_data <- data_clean %>% filter(outlier_cdf95)
      if(nrow(cdf95_out_data) > 0){
        p <- p %>% add_trace(
          data = cdf95_out_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'markers',
          marker = list(color = pal_colors[pollutant_selected], size = 8, symbol = 'x'),
          name = paste0(pollutant_selected, " CDF 95% Aykırı"), showlegend = TRUE, legendgroup = pollutant_selected
        )
      }
      if (nrow(data_clean) > 0 && !any(data_clean$outlier_cdf95)) {
         p <- p %>% add_trace(
            data = data_clean[1,], x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
            line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
            name = paste0(pollutant_selected, " (Veri)"), showlegend = TRUE, legendgroup = pollutant_selected,
            visible = "legendonly"
         )
      }
    }
    p %>% layout(
      title = paste0(station, " - CDF Temelli Aykırı Değerler (Sağ Kuyruk %5)"),
      xaxis = list(title = "Tarih"),
      yaxis = list(title = "Konsantrasyon"),
      legend = list(orientation = 'h', y = -0.2, tracegroupgap = 10)
    )
  })

  output$top5_cdf99_outlier_plot <- renderPlotly({
    df <- top5_analysis()
    s  <- input$top5_comparison_table_rows_selected
    if (is.null(df) || length(s)==0) return(plotly_empty() %>% layout(title = "İstasyon seçilmedi veya veri yok"))
    station     <- df$station[s]
    station_mod <- gsub("/", "_", gsub(" ", "", gsub("\\.", "", station)))
    all_pollutants_for_palette <- c("PM10", "PM25", "SO2", "NO2")
    sel_pollutants <- input$top5_selected_pollutants
    if (is.null(sel_pollutants) || length(sel_pollutants) == 0) {
      return(plotly_empty() %>% layout(title = "Grafik için kirletici seçin"))
    }
    pal_colors <- RColorBrewer::brewer.pal(length(all_pollutants_for_palette), "Set1")
    names(pal_colors) <- all_pollutants_for_palette
    p <- plot_ly()
    for(pollutant_selected in sel_pollutants) {
      qry <- sprintf(
        "SELECT \"Tarih\" as date, \"%s\" as value FROM public.hourly_detail WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL ORDER BY \"Tarih\"",
        pollutant_selected, station_mod, pollutant_selected
      )
      data_raw <- tryCatch(dbGetQuery(con, qry), error = function(e) NULL)
      if (is.null(data_raw) || !"value" %in% names(data_raw) || all(is.na(data_raw$value))) next
      data_clean <- data_raw %>%
        rename(timestamp = date) %>%
        mutate(value = as.numeric(value)) %>%
        filter(!is.na(value))
      if (nrow(data_clean) < 30) next
      fb <- fit_best_distribution(data_clean$value)
      data_clean$outlier_cdf99 <- FALSE
      if (!is.null(fb) && !is.null(fb$fit) && !is.null(fb$fit$estimate)) {
        p99 <- switch(fb$best_dist,
          normal      = qnorm(0.99, mean=fb$fit$estimate["mean"], sd=fb$fit$estimate["sd"]),
          lognormal   = qlnorm(0.99, meanlog=fb$fit$estimate["meanlog"], sdlog=fb$fit$estimate["sdlog"]),
          gamma       = qgamma(0.99, shape=fb$fit$estimate["shape"], rate=fb$fit$estimate["rate"]),
          weibull     = qweibull(0.99, shape=fb$fit$estimate["shape"], scale=fb$fit$estimate["scale"]),
          exponential = qexp(0.99, rate=fb$fit$estimate["rate"]),
          logistic    = qlogis(0.99, location=fb$fit$estimate["location"], scale=fb$fit$estimate["scale"]),
          cauchy      = qcauchy(0.99, location=fb$fit$estimate["location"], scale=fb$fit$estimate["scale"]),
          NULL
        )
        if (!is.na(p99)) data_clean$outlier_cdf99 <- data_clean$value > p99
      }
      # Baseline line for all data of the current pollutant
      p <- p %>% add_trace(
        data = data_clean, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
        line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
        name = paste0(pollutant_selected, " (Tüm Veri)"), showlegend = FALSE, legendgroup = pollutant_selected
      )
      # CDF 99% outlier points
      cdf99_out_data <- data_clean %>% filter(outlier_cdf99)
      if(nrow(cdf99_out_data) > 0){
        p <- p %>% add_trace(
          data = cdf99_out_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'markers',
          marker = list(color = pal_colors[pollutant_selected], size = 10, symbol = 'diamond'),
          name = paste0(pollutant_selected, " CDF 99% Aykırı"), showlegend = TRUE, legendgroup = pollutant_selected
        )
      }
      if (nrow(data_clean) > 0 && !any(data_clean$outlier_cdf99)) {
         p <- p %>% add_trace(
            data = data_clean[1,], x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
            line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
            name = paste0(pollutant_selected, " (Veri)"), showlegend = TRUE, legendgroup = pollutant_selected,
            visible = "legendonly"
         )
      }
    }
    p %>% layout(
      title = paste0(station, " - CDF Temelli Aykırı Değerler (Sağ Kuyruk %1)"),
      xaxis = list(title = "Tarih"),
      yaxis = list(title = "Konsantrasyon"),
      legend = list(orientation = 'h', y = -0.2, tracegroupgap = 10)
    )
  })

  output$top5_iqr_outlier_plot <- renderPlotly({
    df <- top5_analysis()
    s  <- input$top5_comparison_table_rows_selected
    if (is.null(df) || length(s)==0) return(plotly_empty() %>% layout(title = "İstasyon seçilmedi veya veri yok"))
    station     <- df$station[s]
    station_mod <- gsub("/", "_", gsub(" ", "", gsub("\\.", "", station)))
    all_pollutants_for_palette <- c("PM10", "PM25", "SO2", "NO2")
    sel_pollutants <- input$top5_selected_pollutants
    if (is.null(sel_pollutants) || length(sel_pollutants) == 0) {
      return(plotly_empty() %>% layout(title = "Grafik için kirletici seçin"))
    }
    pal_colors <- RColorBrewer::brewer.pal(length(all_pollutants_for_palette), "Set1")
    names(pal_colors) <- all_pollutants_for_palette
    p <- plot_ly()
    for(pollutant_selected in sel_pollutants) {
      qry <- sprintf(
        "SELECT \"Tarih\" as date, \"%s\" as value FROM public.hourly_detail WHERE \"Istasyon_modified\" = '%s' AND \"%s\" IS NOT NULL ORDER BY \"Tarih\"",
        pollutant_selected, station_mod, pollutant_selected
      )
      data_raw <- tryCatch(dbGetQuery(con, qry), error = function(e) NULL)
      if (is.null(data_raw) || !"value" %in% names(data_raw) || all(is.na(data_raw$value))) next
      data_clean <- data_raw %>%
        rename(timestamp = date) %>%
        mutate(value = as.numeric(value)) %>%
        filter(!is.na(value))
      if (nrow(data_clean) < 30) next
      q3 <- quantile(data_clean$value, 0.75, na.rm = TRUE)
      q1 <- quantile(data_clean$value, 0.25, na.rm = TRUE)
      iqr_val <- q3 - q1
      iqr_cutoff <- q3 + 1.5 * iqr_val
      data_clean$outlier_iqr <- data_clean$value > iqr_cutoff
      # Baseline line for all data of the current pollutant
      p <- p %>% add_trace(
        data = data_clean, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
        line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
        name = paste0(pollutant_selected, " (Tüm Veri)"), showlegend = FALSE, legendgroup = pollutant_selected
      )
      # IQR outlier points
      iqr_out_data <- data_clean %>% filter(outlier_iqr)
      if(nrow(iqr_out_data) > 0){
        p <- p %>% add_trace(
          data = iqr_out_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'markers',
          marker = list(color = pal_colors[pollutant_selected], size = 10, symbol = 'star'),
          name = paste0(pollutant_selected, " IQR Aykırı"), showlegend = TRUE, legendgroup = pollutant_selected
        )
      }
      if (nrow(data_clean) > 0 && !any(data_clean$outlier_iqr)) {
         p <- p %>% add_trace(
            data = data_clean[1,], x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
            line = list(color = pal_colors[pollutant_selected], width = 1, dash = 'dot', opacity = 0.5),
            name = paste0(pollutant_selected, " (Veri)"), showlegend = TRUE, legendgroup = pollutant_selected,
            visible = "legendonly"
         )
      }
    }
    p %>% layout(
      title = paste0(station, " - IQR Temelli Aykırı Değerler (Q3 + 1.5*IQR)"),
      xaxis = list(title = "Tarih"),
      yaxis = list(title = "Konsantrasyon"),
      legend = list(orientation = 'h', y = -0.2, tracegroupgap = 10)
    )
  })
}

shinyApp(ui = ui, server = server)
