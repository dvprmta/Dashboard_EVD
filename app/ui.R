# ui.R
library(shinydashboard)
library(shiny)
library(plotly)
library(rsconnect)
library(DT)
library(shinyjs)

# Tambahkan ini di awal UI
addResourcePath("images", "www")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML('
      .landing-page {
  text-align: center;
  min-height: 100vh;
  background: linear-gradient(rgba(0, 51, 102, 0.8), rgba(0, 71, 140, 0.8)),
              url("images/background.jpg");
  background-size: cover;
  background-position: center;
  background-attachment: fixed;
  color: white;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 40px 20px;
}

.welcome-title {
  font-size: 4em;
  font-weight: bold;
  margin-bottom: 20px;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
  animation: fadeIn 1s ease-in;
  color: white;
}

.welcome-subtitle {
  font-size: 2em;
  color: #FFA500;
  margin-bottom: 40px;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
  animation: fadeIn 1.5s ease-in;
}

.stats-container {
  display: flex;
  justify-content: center;
  flex-wrap: wrap;
  gap: 20px;
  margin: 30px 0;
  animation: slideUp 1s ease-out;
}

.stat-box {
  background: rgba(255, 255, 255, 0.9);
  backdrop-filter: blur(10px);
  padding: 20px;
  border-radius: 12px;
  width: 180px;
  border: 2px solid rgba(0, 51, 102, 0.3);
  transition: transform 0.3s, background 0.3s;
}

/* Style khusus untuk box total */
.stat-box.total-box {
  background: #D35400;
  border: 2px solid #002B5C;
}

.stat-box.total-box .stat-value {
  color: white;
}

.stat-box.total-box .stat-label {
  color: white;
}

.stat-value {
  font-size: 2em;
  font-weight: bold;
  color: #002B5C;
  margin-bottom: 8px;
}

.stat-label {
  font-size: 1em;
  color: #003366;
}

.description-box {
  background: rgba(255, 255, 255, 0.9);
  padding: 30px;
  border-radius: 15px;
  max-width: 800px;
  margin: 0 auto 40px auto;
  animation: fadeIn 2s ease-in;
  border: 2px solid rgba(0, 51, 102, 0.3);
}

.description-text {
  font-size: 1.2em;
  line-height: 1.6;
  margin-bottom: 20px;
  color: #003366;
}

.enter-button {
  padding: 15px 50px;
  font-size: 1.3em;
  background: #FFA500;
  color: white;
  border: none;
  border-radius: 30px;
  cursor: pointer;
  transition: all 0.3s;
  font-weight: bold;
  text-transform: uppercase;
  letter-spacing: 2px;
  animation: pulse 2s infinite;
}

.enter-button:hover {
  background: #002B5C;
  transform: scale(1.05);
}

@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}

@keyframes slideUp {
  from { transform: translateY(50px); opacity: 0; }
  to { transform: translateY(0); opacity: 1; }
}

@keyframes pulse {
  0% { transform: scale(1); }
  50% { transform: scale(1.05); }
  100% { transform: scale(1); }
}
    '))
  ),
  
  # Halaman Utama
  div(id = "landing-page", class = "landing-page",
      h1(class = "welcome-title", "SIAP TANGGAP"),
      h3(class = "welcome-subtitle", "Sistem Informasi Analisis & Penanggulangan Bencana"),
      
      div(class = "description-box",
          div(class = "description-text",
              HTML("Selamat datang di <b>SIAP TANGGAP</b> - Sistem Informasi Analisis & Penanggulangan Bencana. Platform ini menyediakan gambaran visual dan analisis lengkap tentang kejadian bencana alam di tahun 2024 dari berbagai negara di dunia."),
              br(), br(),
              HTML("Temukan informasi penting tentang <b>jumlah korban</b>, <b>kerugian ekonomi</b>, dan <b>penyebaran bencana</b> melalui tampilan data yang mudah dipahami. Pahami lebih dalam bagaimana bencana alam terjadi dan dampaknya bagi masyarakat melalui dashboard interaktif kami.")
          )
      ),
      
      div(class = "stats-container",
          div(class = "stat-box",
              div(class = "stat-value", "1449"),
              div(class = "stat-label", "Kebakaran")
          ),
          div(class = "stat-box",
              div(class = "stat-value", "1440"),
              div(class = "stat-label", "Topan")
          ),
          div(class = "stat-box",
              div(class = "stat-value", "1482"),
              div(class = "stat-label", "Tornado")
          ),
          div(class = "stat-box",
              div(class = "stat-value", "1463"),
              div(class = "stat-label", "Banjir")
          ),
          div(class = "stat-box",
              div(class = "stat-value", "147"),
              div(class = "stat-label", "Gempa")
          ),
          div(class = "stat-box total-box",
              div(class = "stat-value", "7320"),
              div(class = "stat-label", "Total Bencana")
          )
      ),
      
      actionButton("enter_dashboard", "AKSES DASHBOARD", class = "enter-button")
  ),
  
  # Dashboard Utama
  div(id = "dashboard-container", class = "dashboard-container",
      dashboardPage(
        dashboardHeader(
          title = "Dashboard Bencana Alam",
          dropdownMenu(
            type = "notifications",
            notificationItem(
              text = "Selamat Datang di Analisis Bencana Alam",
              icon = icon("info"),
              status = "success"
            )
          )
        ),
        
        dashboardSidebar(
          sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Statistik Deskriptif", tabName = "descriptive", icon = icon("table")),
            menuItem("Tren", tabName = "trends", icon = icon("chart-line")),
            menuItem("Korelasi", tabName = "correlations", icon = icon("connectdevelop")),
            menuItem("Regresi", tabName = "regression", icon = icon("calculator")),
            menuItem("Peramalan", tabName = "forecasting", icon = icon("chart-line")),
            menuItem("Data", tabName = "data", icon = icon("database")),
            menuItem("Tentang", tabName = "about", icon = icon("info-circle"))
          )
        ),
        
        dashboardBody(
          tags$head(
            tags$style(HTML('
        .small-box {height: 100px}
        .skin-blue .main-header .logo {background-color: #3c8dbc}
        .skin-blue .main-header .navbar {background-color: #3c8dbc}
      '))
          ),
          
          tabItems(
            # Tab Dashboard
            tabItem(
              tabName = "dashboard",
              fluidRow(
                infoBox(
                  "Total Bencana", 
                  textOutput("total_disasters"), 
                  icon = icon("exclamation-triangle"),
                  color = "red",
                  width = 3
                ),
                infoBox(
                  "Total Korban Jiwa", 
                  textOutput("total_fatalities"), 
                  icon = icon("skull"),
                  color = "purple",
                  width = 3
                ),
                infoBox(
                  "Rata-rata Kerugian", 
                  textOutput("avg_loss"), 
                  icon = icon("dollar-sign"),
                  color = "green",
                  width = 3
                ),
                infoBox(
                  "Magnitudo Maksimum", 
                  textOutput("max_magnitude"), 
                  icon = icon("chart-line"),
                  color = "yellow",
                  width = 3
                )
              ),
              
              fluidRow(
                box(
                  title = "Grafik Analisis Bencana",
                  status = "primary",
                  solidHeader = TRUE,
                  height = 520,
                  div(
                    class = "chart-container chart-delay-1",
                    style = "position: relative; height: 450px;",
                    plotlyOutput("plot1", height = "100%")
                  ),
                  width = 8
                ),
                
                box(
                  title = "Pengaturan",
                  status = "warning",
                  solidHeader = TRUE,
                  height = 520,
                  width = 4,
                  
                  div(
                    style = "padding: 5px 15px;",
                    
                    div(
                      style = "margin-bottom: 20px;",
                      selectInput("disaster_type",
                                  "Jenis Bencana:",
                                  choices = c("Semua" = "All", 
                                              "Kebakaran" = "Kebakaran", 
                                              "Angin Topan" = "Angin Topan", 
                                              "Tornado" = "Tornado", 
                                              "Banjir" = "Banjir", 
                                              "Gempa Bumi" = "Gempa Bumi"),
                                  selected = "All")
                    ),
                    
                    div(
                      style = "margin-bottom: 20px;",
                      selectInput("plot_type",
                                  "Jenis Grafik:",
                                  choices = c("Korban Jiwa per Jenis" = "fatalities",
                                              "Kerugian Ekonomi per Jenis" = "loss",
                                              "Distribusi Magnitudo" = "magnitude"),
                                  selected = "fatalities")
                    ),
                    
                    div(
                      style = "margin-bottom: 20px;",
                      selectInput("plot_color",
                                  "Warna Grafik:",
                                  choices = c("Biru" = "#3c8dbc",
                                              "Merah" = "#ff0000",
                                              "Hijau" = "#50c878",
                                              "Ungu" = "#800080",
                                              "Oranye" = "#ffa500"),
                                  selected = "#3c8dbc")
                    ),
                    
                    hr(style = "border-top: 1px solid #ddd; margin: 20px 0;"),
                    
                    div(
                      style = "margin-bottom: 20px;",
                      checkboxInput("show_values", 
                                    "Tampilkan Nilai pada Grafik", 
                                    value = TRUE)
                    ),
                    
                    div(
                      style = "margin-bottom: 20px;",
                      checkboxInput("show_percentage", 
                                    "Tampilkan Persentase", 
                                    value = TRUE)
                    ),
                    
                    div(
                      style = "margin-bottom: 20px;",
                      sliderInput("hole_size",
                                  "Ukuran Lubang Donat:",
                                  min = 0,
                                  max = 0.8,
                                  value = 0.4,
                                  step = 0.1)
                    ),
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Distribusi Lokasi",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("location_plot", height = 270),
                  width = 12
                )
              )
            ),
            
            # Tab Statistik Deskriptif
            tabItem(
              tabName = "descriptive",
              fluidRow(
                box(
                  title = "Pemilihan Variabel",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  column(6,
                         selectInput("desc_var",
                                     "Pilih Variabel:",
                                     choices = c(
                                       "Korban Jiwa" = "Fatalities",
                                       "Kerugian Ekonomi (Rp)" = "Economic_Loss($)",
                                       "Magnitudo" = "Magnitude"
                                     ),
                                     selected = "Fatalities"
                         )
                  ),
                  column(6,
                         selectInput("desc_group",
                                     "Kelompok Data Berdasarkan:",
                                     choices = c(
                                       "Keseluruhan" = "none",
                                       "Jenis Bencana" = "Disaster_Type",
                                       "Lokasi" = "Location"
                                     ),
                                     selected = "none"
                         )
                  )
                )
              ),
              fluidRow(
                # Kotak Statistik Ringkasan
                box(
                  title = "Statistik Ringkasan",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  DTOutput("summary_stats_table")
                ),
                # Kotak Statistik Distribusi
                box(
                  title = "Statistik Distribusi",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  DTOutput("distribution_stats_table")
                )
              ),
              fluidRow(
                # Histogram
                box(
                  title = "Grafik Distribusi",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("dist_plot", height = 350)
                )
              )
            ),
            
            # Tab Tren
            tabItem(
              tabName = "trends",
              fluidRow(
                box(
                  title = "Analisis Tren Dampak Bencana",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("trend_analysis", height = 400),
                  width = 8
                ),
                box(
                  title = "Pengaturan",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("trend_metric",
                              "Pilih Metrik:",
                              choices = c(
                                "Jumlah Bencana" = "count",
                                "Kerugian Ekonomi" = "loss",
                                "Rata-rata Magnitudo" = "magnitude"
                              ),
                              selected = "count"
                  ),
                  selectInput("trend_disaster_type",
                              "Jenis Bencana:",
                              choices = c("Semua" = "All", 
                                          "Kebakaran" = "Kebakaran", 
                                          "Angin Topan" = "Angin Topan", 
                                          "Tornado" = "Tornado", 
                                          "Banjir" = "Banjir", 
                                          "Gempa Bumi" = "Gempa Bumi"),
                              selected = "All"
                  ),
                  radioButtons("trend_view",
                               "Jenis Tampilan:",
                               choices = c(
                                 "Grafik Garis" = "line",
                                 "Grafik Batang" = "bar",
                                 "Grafik Area" = "area"
                               ),
                               selected = "line"
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Distribusi Bulanan",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("monthly_heatmap", height = 300),
                  width = 12
                )
              )
            ),
            
            # Tab Korelasi
            tabItem(
              tabName = "correlations",
              fluidRow(
                box(
                  title = "Matriks Korelasi",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("correlation_plot", height = 400),
                  width = 6
                ),
                box(
                  title = "Distribusi Tingkat Keparahan Bencana",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("severity_analysis", height = 400),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Analisis Distribusi (Box Plot)",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("boxplot_analysis", height = 400),
                  width = 12,
                  
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("boxplot_var",
                                  "Pilih Variabel:",
                                  choices = c(
                                    "Korban Jiwa" = "Fatalities",
                                    "Kerugian Ekonomi (Rp)" = "Economic_Loss($)",
                                    "Magnitudo" = "Magnitude"
                                  ),
                                  selected = "Fatalities"
                      )
                    ),
                    column(
                      width = 6,
                      checkboxInput("show_outliers", 
                                    "Tampilkan Pencilan", 
                                    value = TRUE
                      )
                    )
                  )
                )
              )
            ),
            
            # Tab Regresi
            tabItem(
              tabName = "regression",
              fluidRow(
                box(
                  title = "Analisis Regresi",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("regression_plot", height = 500),
                  width = 8
                ),
                box(
                  title = "Pengaturan",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("reg_x_var",
                              "Variabel Bebas (X):",
                              choices = c(
                                "Magnitude" = "Magnitude",
                                "Kerugian Ekonomi (USD)" = "Economic_Loss($)"
                              ),
                              selected = "Magnitude"
                  ),
                  selectInput("reg_y_var",
                              "Variabel Terikat (Y):",
                              choices = c(
                                "Korban Jiwa" = "Fatalities",
                                "Kerugian Ekonomi (USD)" = "Economic_Loss($)"
                              ),
                              selected = "Fatalities"
                  ),
                  selectInput("reg_type",
                              "Jenis Bencana:",
                              choices = c(
                                "Semua" = "All",
                                "Kebakaran" = "Kebakaran",
                                "Angin Topan" = "Angin Topan",
                                "Tornado" = "Tornado",
                                "Banjir" = "Banjir",
                                "Gempa Bumi" = "Gempa Bumi"
                              ),
                              selected = "All"
                  ),
                  checkboxInput("show_reg_eq", 
                                "Tampilkan Garis Regresi", 
                                value = TRUE)
                )
                
              ),
              
              fluidRow(
                box(
                  title = "Grafik Gelembung Dampak Bencana",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("bubble_chart", height = 400)
                )
              )
            ),
            tabItem(
              tabName = "forecasting",
              fluidRow(
                box(
                  title = "Peramalan Deret Waktu",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("forecast_plot", height = 500)
                ),
                box(
                  title = "Pengaturan Peramalan",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("forecast_var",
                              "Pilih Variabel:",
                              choices = c(
                                "Jumlah Bencana" = "count",
                                "Korban Jiwa" = "fatalities",
                                "Kerugian Ekonomi" = "economic_loss"
                              ),
                              selected = "count"
                  ),
                  selectInput("forecast_type",
                              "Jenis Bencana:",
                              choices = c("Semua" = "All", 
                                          "Kebakaran" = "Kebakaran", 
                                          "Angin Topan" = "Angin Topan", 
                                          "Tornado" = "Tornado", 
                                          "Banjir" = "Banjir", 
                                          "Gempa Bumi" = "Gempa Bumi"),
                              selected = "All"
                  ),
                  numericInput("forecast_periods",
                               "Periode Peramalan (Bulan):",
                               value = 3,
                               min = 1,
                               max = 12
                  ),
                  selectInput("forecast_model",
                              "Model Peramalan:",
                              choices = c(
                                "ARIMA" = "arima",
                                "Penghalusan Eksponensial" = "ets",
                                "Rata-rata Bergerak Sederhana" = "sma"
                              ),
                              selected = "arima"
                  ),
                  checkboxInput("show_ci", 
                                "Tampilkan Interval Kepercayaan", 
                                value = TRUE)
                )
              ),
              fluidRow(
                box(
                  title = "Hasil Peramalan",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("forecast_table")
                )
              )
            ),
            
            # Tab Data
            tabItem(
              tabName = "data",
              fluidRow(
                box(
                  title = "Pemilihan Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(6,
                           selectInput("data_disaster_filter", "Pilih Jenis Bencana:",
                                       choices = c("Semua" = "All", 
                                                   "Kebakaran" = "Kebakaran", 
                                                   "Angin Topan" = "Angin Topan", 
                                                   "Tornado" = "Tornado", 
                                                   "Banjir" = "Banjir", 
                                                   "Gempa Bumi" = "Gempa Bumi"),
                                       selected = "All")
                    ),
                    column(6,
                           selectInput("data_location_filter", "Pilih Lokasi:",
                                       choices = c("Semua" = "All"),  # Akan diperbarui di server
                                       selected = "All")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Ringkasan Data Bencana",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("data_table")
                )
              )
            ),
            
            tabItem(
              tabName = "about",
              fluidRow(
                box(
                  title = "Tentang Dashboard Ini",
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  # Main content wrapper
                  tags$div(
                    class = "about-content",
                    style = "padding: 20px;",
                    
                    # Ikhtisar Section
                    tags$h3(
                      class = "section-header",
                      style = "color: #3c8dbc; margin-bottom: 20px; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px;", 
                      "Ikhtisar"
                    ),
                    tags$p(
                      "Dashboard ini menyajikan analisis komprehensif data bencana alam tahun 2024, dengan fokus pada visualisasi interaktif dari berbagai jenis bencana seperti banjir, gempa bumi, tornado, badai, dan kebakaran hutan. Data dikategorikan berdasarkan wilayah, periode waktu, dan dampak, mencakup informasi tentang korban jiwa dan kerugian ekonomi. Dilengkapi dengan fitur penyaringan canggih, grafik dinamis, dan peta interaktif, dashboard ini memungkinkan pengguna untuk mengeksplorasi pola, tren, dan tingkat keparahan bencana secara mendalam."
                    ),
                    
                    # Metodologi Section
                    tags$h3(
                      class = "section-header",
                      style = "color: #3c8dbc; margin: 30px 0 20px; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px;", 
                      "Metodologi"
                    ),
                    
                    # Statistical Analysis
                    tags$div(
                      class = "methodology-section",
                      tags$h4(
                        style = "color: #2979b5; margin: 20px 0 15px;",
                        "1. Analisis Statistik Deskriptif"
                      ),
                      tags$ul(
                        style = "list-style-type: disc; margin-left: 20px;",
                        tags$li(
                          tags$b("Ukuran Pemusatan: "),
                          "Mean, median, dan modus untuk mengidentifikasi nilai tipikal"
                        ),
                        tags$li(
                          tags$b("Ukuran Penyebaran: "),
                          "Standar deviasi, range, dan IQR untuk mengukur variabilitas"
                        ),
                        tags$li(
                          tags$b("Distribusi: "),
                          "Analisis skewness dan kurtosis untuk memahami bentuk distribusi data"
                        ),
                        tags$li(
                          tags$b("Visualisasi: "),
                          "Histogram, box plot, dan violin plot untuk representasi visual distribusi"
                        )
                      )
                    ),
                    
                    # Correlation Analysis
                    tags$div(
                      class = "methodology-section",
                      tags$h4(
                        style = "color: #2979b5; margin: 20px 0 15px;",
                        "2. Analisis Korelasi"
                      ),
                      tags$ul(
                        style = "list-style-type: disc; margin-left: 20px;",
                        tags$li(
                          tags$b("Korelasi Pearson: "),
                          "Mengukur kekuatan hubungan linear antar variabel numerik"
                        ),
                        tags$li(
                          tags$b("Heatmap Korelasi: "),
                          "Visualisasi matriks korelasi untuk memudahkan interpretasi"
                        ),
                        tags$li(
                          tags$b("Analisis Multivariat: "),
                          "Mengeksplorasi hubungan kompleks antar multiple variabel"
                        )
                      )
                    ),
                    
                    # Regression Analysis
                    tags$div(
                      class = "methodology-section",
                      tags$h4(
                        style = "color: #2979b5; margin: 20px 0 15px;",
                        "3. Analisis Regresi"
                      ),
                      tags$ul(
                        style = "list-style-type: disc; margin-left: 20px;",
                        tags$li(
                          tags$b("Regresi Linear: "),
                          "Memodelkan hubungan linear antara variabel dependen dan independen"
                        ),
                        tags$li(
                          tags$b("Evaluasi Model: "),
                          "Menggunakan R², RMSE, dan p-value untuk menilai kualitas model"
                        ),
                        tags$li(
                          tags$b("Diagnostik Model: "),
                          "Analisis residual dan uji asumsi untuk validasi model"
                        )
                      )
                    ),
                    
                    # Time Series Analysis
                    tags$div(
                      class = "methodology-section",
                      tags$h4(
                        style = "color: #2979b5; margin: 20px 0 15px;",
                        "4. Analisis Time Series & Peramalan"
                      ),
                      tags$ul(
                        style = "list-style-type: disc; margin-left: 20px;",
                        tags$li(
                          tags$b("ARIMA: "),
                          "Model untuk menganalisis dan meramalkan data time series dengan mempertimbangkan autoregressive dan moving average components"
                        ),
                        tags$li(
                          tags$b("Exponential Smoothing (ETS): "),
                          "Metode peramalan yang memberikan bobot lebih besar pada observasi terbaru"
                        ),
                        tags$li(
                          tags$b("Simple Moving Average (SMA): "),
                          "Teknik smoothing sederhana untuk mengidentifikasi tren"
                        ),
                        tags$li(
                          tags$b("Evaluasi Forecast: "),
                          "Menggunakan metrik RMSE, MAE, dan MAPE untuk menilai akurasi peramalan"
                        )
                      )
                    ),
                    
                    # Data Description
                    tags$h3(
                      class = "section-header",
                      style = "color: #3c8dbc; margin: 30px 0 20px; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px;", 
                      "Deskripsi Data"
                    ),
                    tags$ul(
                      style = "list-style-type: disc; margin-left: 20px;",
                      tags$li("Dataset mencakup informasi dari enam negara: Brasil, China, India, Indonesia, Jepang, dan Amerika Serikat"),
                      tags$li("Periode data: Januari 2024 hingga Februari 2025"),
                      tags$li("Total entri: 10.000 records"),
                      tags$li("Variabel yang dicakup:",
                              tags$ul(
                                style = "list-style-type: circle; margin-left: 20px;",
                                tags$li("Jenis bencana (kebakaran hutan, angin topan, tornado, banjir, gempa bumi)"),
                                tags$li("Lokasi dan waktu kejadian"),
                                tags$li("Jumlah korban jiwa"),
                                tags$li("Estimasi kerugian ekonomi"),
                                tags$li("Magnitudo atau intensitas bencana")
                              )
                      )
                    ),
                    
                    # Source
                    tags$h3(
                      class = "section-header",
                      style = "color: #3c8dbc; margin: 30px 0 20px; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px;", 
                      "Sumber Data"
                    ),
                    tags$p(
                      "Data diperoleh dari ",
                      tags$a(
                        href = "https://www.kaggle.com/datasets/umeradnaan/prediction-of-disaster-management-in-2024",
                        target = "_blank",
                        "Kaggle - Prediction of Disaster Management in 2024"
                      ),
                      ". Dataset ini telah melalui proses pembersihan dan validasi untuk memastikan kualitas dan konsistensi analisis."
                    ),
                    
                    # Development Team
                    tags$h3(
                      class = "section-header",
                      style = "color: #3c8dbc; margin: 30px 0 20px; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px;", 
                      "Tim Pengembang"
                    ),
                    tags$div(
                      class = "team-container",
                      style = "display: flex; flex-wrap: wrap; gap: 20px; margin-top: 20px;",
                      
                      # Team member cards
                      lapply(
                        list(
                          list(name = "Tukhfatur Rizmah A.", id = "G1501231023"),
                          list(name = "Devi Permata Sari", id = "G1501231026"),
                          list(name = "Adhiyatma Nugraha", id = "G1501231085")
                        ),
                        function(member) {
                          tags$div(
                            class = "team-card",
                            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; flex: 1; min-width: 250px;",
                            tags$div(
                              style = "font-weight: bold; font-size: 1.1em; color: #3c8dbc;",
                              member$name
                            ),
                            tags$div(
                              style = "color: #666; margin-top: 5px;",
                              member$id
                            )
                          )
                        }
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
  )
)
