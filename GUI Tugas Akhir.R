install.packages("shiny")
install.packages("lmtest")
install.packages("quantreg")
library(lmtest)
library(shiny)
library(quantreg)

# UI
ui <- fluidPage(
  titlePanel("Tugas Akhir | Marwa Fadlila Amalia | 24050121120005"),
      tabsetPanel(
        tabPanel("Cover",
             h4(strong("PEMODELAN REGRESI KUANTIL UNTUK MENGATASI OUTLIER DAN HETEROSKEDASTISITAS PADA INDEKS KETAHANAN PANGAN (IKP) PROVINSI ACEH"),
                style = "text-align:center"),
             br(),
             
             a(href = "https://1.bp.blogspot.com/-tThKR0i2DdQ/XrO4fFiulNI/AAAAAAAAB_s/4_UY2xeR3SsE9_5MGBdvsQtBJgNxf9e_wCLcBGAsYHQ/s400-rw/Logo%2BUndip%2BUniversitas%2BDiponegoro.png", target = "_blank",
               img(src = "https://1.bp.blogspot.com/-tThKR0i2DdQ/XrO4fFiulNI/AAAAAAAAB_s/4_UY2xeR3SsE9_5MGBdvsQtBJgNxf9e_wCLcBGAsYHQ/s400-rw/Logo%2BUndip%2BUniversitas%2BDiponegoro.png", alt = "Deskripsi Gambar", width = "300px", height = "300px", style = "display: block; margin:0 auto;")
             ),
             br(),
             h5("Disusun oleh:", style = "text-align:center"),
             h5("Marwa Fadlila Amalia", style = "text-align:center"),
             h5("24050121120005", style = "text-align:center"),
             div(style = "display: flex; justify-content: space-between; width: 80%; margin: 0 auto;",
                 div(style = "text-align: center;width: 40%;",
                     h5("Pembimbing I:"),
                     h5("Arief Rachman Hakim, S.Si., M.Si."),
                     h5("NIP. 199307302018031001"),
                     ),
                 div(style = "text-align: center;width: 40%",
                     h5("Pembimbing II:"),
                     h5("Sugito, S.Si., M.Si."),
                     h5("NIP. 197610192005011001"),
                    )
                 ),
             br(),
             h5(strong("DEPARTEMEN STATISTIKA"), style = "text-align:center"),
             h5(strong("FAKULTAS SAINS DAN MATEMATIKA"), style = "text-align:center"),
             h5(strong("UNIVERSITAS DIPONEGORO"), style = "text-align:center"),
             h5(strong("SEMARANG"), style = "text-align:center"),
             h5(strong("2024"), style = "text-align:center")
    ),
      tabPanel("Analisis Data",
        sidebarLayout(
          sidebarPanel(
            fileInput("file", "Unggah File CSV", accept = ".csv"),
                selectInput("separator", "Pilih Separator CSV:",
                            choices = c("Koma (,)" = ",",
                                    "Titik koma (;)" = ";",
                                    "Tab" = "\t")),
                uiOutput("varSelectY"),
                uiOutput("varSelectX"),
                numericInput("tau", "Pilih Nilai Tau (Kuantil):", 0.5, min = 0, max = 1, step = 0.05),
                actionButton("stat_desc","Statistika Deskriptif"),
                actionButton("detect_outlier","Deteksi Outlier"),
                actionButton("detect_corr","Analisis Korelasi"),
                actionButton("run_linier","Model Regresi Linier"),
                actionButton("run_kuantil", "Model Regresi Kuantil")
    ),
      mainPanel(
        tableOutput("dataPreview"),
        verbatimTextOutput("statistic_desc"),
        verbatimTextOutput("outlierDetection"),
        verbatimTextOutput("correlation"),
        verbatimTextOutput("summary_linier"),
        verbatimTextOutput("significance_linier"),
        verbatimTextOutput("assumptionCheck"),
        verbatimTextOutput("aicValue_linier"),
        verbatimTextOutput("summary_kuantil"),
        verbatimTextOutput("significance_kuantil"),
        verbatimTextOutput("aicValue_kuantil"),
        )
      )
    )
  )
)

# Server
  server <- function(input, output) {
    data <- reactive({
      req(input$file)
      sep <- input$separator
      df <- read.csv(input$file$datapath, sep=sep)
  
# standarisasi data
    if (all(c("X1", "X2", "X3", "X4") %in% colnames(df))) {
      df[, c("X1", "X2", "X3", "X4")] <- scale(df[, c("X1", "X2", "X3", "X4")])
    }
    return(df)
  })

# Menampilkan data yang diunggah di UI
  output$dataPreview <- renderTable({
    req(data())
  })
  
# Memilih variabel respon dan prediktor
  output$varSelectY <- renderUI({
    req(data())
    selectInput("yVar", "Pilih Variabel Respon:", choices = names(data()))
  })
  output$varSelectX <- renderUI({
    req(data())
    selectInput("xVars", "Pilih Variabel Prediktor:", choices = names(data()), multiple=TRUE)
  })
  
# Statistika Deskriptif
  stat_desk <- eventReactive(input$stat_desc, {
    output$statistic_desc <- renderPrint({
      req(data())
      desc_stats <- summary(data())
      return(desc_stats)
    })
  })
  
# Menampilkan hasil statistika deskriptif
  output$statistic_desc <- renderPrint({
    req(stat_desk()) 
    cat("Statistika Deskriptif:\n")
    print(stat_desk())
  })
  
# Deteksi outlier pada variabel respon
  detect_outliers <- function(y) {
    Q1 <- quantile(y, 0.25,type=1)
    Q3 <- quantile(y, 0.75,type=1)
    IQR_value <- (Q3-Q1)
    
    batas_bawah <- Q1 - 1.5 * IQR_value
    batas_atas <- Q3 + 1.5 * IQR_value
    
    outlier_indices <- which(y < batas_bawah | y > batas_atas)
    
    return(list(indices = outlier_indices, bawah = batas_bawah, atas = batas_atas))
  }
  
# Menampilkan deteksi outlier pada variabel respon
  output$outlierDetection <- renderPrint({
    req(data(), input$yVar)
    y_data <- data()[[input$yVar]]
    outlier_info <- detect_outliers(y_data)
    
    if (length(outlier_info$indices) > 0) {
      cat("Outlier terdeteksi pada indeks:", outlier_info$indices, "\n")
      cat("Nilai outlier:", y_data[outlier_info$indices], "\n")
      cat("Batas bawah:", outlier_info$bawah, "\n")
      cat("Batas atas:", outlier_info$atas, "\n")
      return(outlier_info)
    } else {
      cat("Tidak ada outlier yang terdeteksi.\n")
      return(NULL)
    }
  })
  
# Menjalankan analisis korelasi
  analisis_korelasi <- eventReactive(input$detect_corr,{
    req(input$yVar, input$xVars)
    data_selected <- data()[,c(input$yVar, input$xVars)]
    cor(data_selected, method="pearson")
  })

# Menampilkan hasil analisis korelasi
  output$correlation <- renderPrint({
    req(analisis_korelasi()) 
    cat("Matriks Korelasi antara Variabel Respon dan Prediktor:\n")
    print(analisis_korelasi())
  })
  
# Menjalankan model regresi linier
  model_linier <- eventReactive(input$run_linier, {
    req(input$yVar, input$xVars)
    if (length(input$xVars) < 1) {
      stop("Pilih minimal 1 variabel independen.")
    }
    
    formula_linier <- as.formula(paste(input$yVar, "~", paste(input$xVars, collapse = "+")))
    lm(formula_linier, data = data())
  })
  
# Menampilkan hasil ringkasan model regresi linier
  output$summary_linier <- renderPrint({
    req(model_linier())
    summary(model_linier())
  })
  
# Menampilkan hasil signifikansi parameter model regresi linier 
  output$significance_linier <- renderPrint({
    req(model_linier())
    summary_model_linier<- summary(model_linier())
    coef(summary_model_linier)
  })
  
# Menampilkan hasil uji asumsi klasik model regresi linier
  output$assumptionCheck <- renderPrint({
    req(model_linier())
    error <- residuals(model_linier())
    
    cat("1. Uji Non Multikolinieritas.\n")
    non_multikolinieritas <- vif(model_linier())
    print(non_multikolinieritas)
    
    cat("2. Uji Normalitas.\n")
    normalitas <- ks.test(error,'pnorm',mean(error),sd(error))
    print(normalitas)
    
    cat("3. Uji Non Autokorelasi.\n")
    non_autokorelasi <- dwtest(model_linier(), alternative = "two.sided")
    print(non_autokorelasi)
    
    cat("4. Uji Homoskedastisitas.\n")
    abs_error = abs(error)
    prediktor <- data()[,c(input$xVars)]
    homoskedastisitas <- summary(lm(abs_error~.,data = prediktor))
    print(homoskedastisitas)
  })
  
# Menampilkan nilai AIC model regresi linier berganda
  output$aicValue_linier <- renderPrint({
    req(model_linier())
    aic_value_linier <- AIC(model_linier())
    cat("Nilai AIC dari model regresi linier:", aic_value_linier)
  })
  
# Menjalankan model regresi kuantil
  model_kuantil <- eventReactive(input$run_kuantil, {
    req(input$xVars,input$yVar, input$tau)
    if (length(input$xVars)<1){
      stop("Pilih minimal 1 variabel independen.")
    }
    formula_kuantil <- as.formula(paste(input$yVar, "~", paste(input$xVars, collapse="+")))
    rq(formula_kuantil, data = data(), tau = input$tau)
  })
  
# Menampilkan hasil ringkasan model regresi kuantil
  output$summary_kuantil <- renderPrint({
    req(model_kuantil())
    summary(model_kuantil())
  })
  
# Menampilkan hasil signifikansi parameter model regresi kuantil 
  output$significance_kuantil <- renderPrint({
    req(model_kuantil())
    summary_model_kuantil <- summary(model_kuantil(), se = "nid") # Menggunakan standard errors nid
    coef(summary_model_kuantil) # Menampilkan koefisien beserta p-value
  })
  
# Menampilkan nilai AIC model regresi kuantil
  output$aicValue_kuantil <- renderPrint({
    req(model_kuantil())
    aic_value_kuantil <- AIC(model_kuantil())
    cat("Nilai AIC dari model:", aic_value_kuantil)
  })
}

shinyApp(ui=ui,server = server)