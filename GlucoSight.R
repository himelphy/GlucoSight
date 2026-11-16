 # Auto-install missing dependencies
packages <- c("shiny", "ggplot2", "ega", "tidyr", "dplyr", "DT", "openxlsx")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if(length(new_packages)) {
  install.packages(new_packages)
}

library(shiny)
library(ggplot2)
library(ega)
library(tidyr)
library(dplyr)
library(DT)
library(openxlsx)

# =========================
# UI
# =========================

ui <- fluidPage(
  
  titlePanel("GlucoSight"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("1. Data Input"),
      
      downloadButton("dl_sample", "Download Example CSV"),
      
      br(), br(),
      
      fileInput(
        "data_file",
        "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      
      helpText(
        "Ensure columns are named 'Reference' and 'Meter'."
      ),
      
      br(),
      
      selectInput(
        "diabetes_type",
        "Select PEGA Type:",
        choices = c(
          "Type 1 Diabetes Mellitus" = 1,
          "Type 2 Diabetes Mellitus" = 2
        ),
        selected = 2
      ),
      
      hr(),
      
      h4("2. Run Analysis"),
      
      actionButton(
        "analyze",
        "Analyze",
        class = "btn-primary",
        width = "100%"
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # =========================
        # Numerical Statistics
        # =========================
        
        tabPanel(
          "Numerical Statistics",
          br(),
          h4("Accuracy Criteria Results"),
          tableOutput("num_stats"),
          hr(),
          h4("Required vs. Achieved Accuracy"),
          plotOutput("acc_bar_chart", height = "400px")
        ),
        
        # =========================
        # Raw Data Analysis
        # =========================
        
        tabPanel(
          "Raw Data Analysis",
          br(),
          h4("Raw Data & ISO Compliance"),
          h5(textOutput("total_obs")),
          p(
            strong("Color Legend:"), br(),
            "Reference: ", span("Blue", style="background-color:#D9EDF7; color:#31708F; padding: 2px 6px;"),
            " for < 100 mg/dL | ", span("Yellow", style="background-color:#FCF8E3; color:#8A6D3B; padding: 2px 6px;"), " for >= 100 mg/dL",
            br(),
            "Difference & Deviation: ", span("Green", style="background-color:#DFF0D8; color:#3C763D; padding: 2px 6px;"),
            " for within ISO limits | ", span("Red", style="background-color:#F2DEDE; color:#A94442; padding: 2px 6px;"),
            " for beyond ISO limits"
          ),
          downloadButton("dl_colored_excel", "Download Colored Excel"),
          br(), br(),
          DTOutput("raw_data_table")
        ),
        
        # =========================
        # MARD & Paired Analysis
        # =========================
        
        tabPanel(
          "MARD & Paired Analysis",
          br(),
          h4("Descriptive Statistics"),
          tableOutput("desc_stats_table"),
          hr(),
          h4("MARD & Relative Difference Summary"),
          tableOutput("mard_table"),
          hr(),
          h4("Paired t-Test (Meter vs. Reference)"),
          tableOutput("ttest_table"),
          uiOutput("ttest_interpretation"),
          hr(),
          h4("Connected Paired Values (Reference vs. Meter)"),
          plotOutput("paired_plot", height = "480px"),
          fluidRow(
            column(
              6,
              radioButtons(
                "paired_ext",
                "Format:",
                choices = c("png", "jpg", "pdf"),
                inline = TRUE
              )
            ),
            column(
              6,
              downloadButton("dl_paired", "Download Paired Plot")
            )
          )
        ),
        
        # =========================
        # Error Grid Analysis
        # =========================
        
        tabPanel(
          "Error Grid Analysis",
          br(),
          h4("Clarke Error Grid Analysis (CEGA)"),
          tableOutput("cega_table"),
          plotOutput("cega_plot"),
          fluidRow(
            column(
              6,
              radioButtons(
                "cega_ext",
                "Format:",
                choices = c("png", "jpg", "pdf"),
                inline = TRUE
              )
            ),
            column(
              6,
              downloadButton("dl_cega", "Download CEGA")
            )
          ),
          hr(),
          h4("Parkes Error Grid Analysis (PEGA)"),
          tableOutput("pega_table"),
          plotOutput("pega_plot"),
          fluidRow(
            column(
              6,
              radioButtons(
                "pega_ext",
                "Format:",
                choices = c("png", "jpg", "pdf"),
                inline = TRUE
              )
            ),
            column(
              6,
              downloadButton("dl_pega", "Download PEGA")
            )
          )
        ),
        
        # =========================
        # Bland Altman
        # =========================
        
        tabPanel(
          "Modified Bland-Altman",
          br(),
          h4("Modified Bland-Altman Plot"),
          p("Green dashed lines represent the ISO 15197:2013 acceptance limits."),
          plotOutput("bland_altman"),
          fluidRow(
            column(
              6,
              radioButtons(
                "bland_ext",
                "Format:",
                choices = c("png", "jpg", "pdf"),
                inline = TRUE
              )
            ),
            column(
              6,
              downloadButton("dl_bland", "Download Plot")
            )
          )
        ),
        
        # =========================
        # Credit
        # =========================
        
        tabPanel(
          "Credit",
          br(),
          h4("Prepared by:"),
          p("Dr. Himel Mondal"),
          p("Department of Physiology"),
          p("All India Institute of Medical Sciences, Deoghar, India. PIN: 814117"),
          p("Email: himel.physiology@aiimsdeoghar.edu.in"),
          hr(),
          p(
            strong("How to cite the application: "),
            "Please cite the paper that describes the development of GlucoSight"
          )
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
  # Publication-ready theme
  pub_theme <- theme(
    axis.text = element_text(size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 16, face = "bold", color = "black"),
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = "top"
  )
  
  # =========================
  # Download Example CSV
  # =========================
  
  output$dl_sample <- downloadHandler(
    filename = function() {
      "GlucoSight_Example.csv"
    },
    content = function(file) {
      set.seed(42)
      Reference <- sample(40:400, 100, replace = TRUE)
      Meter <- Reference + rnorm(100, mean = 0, sd = 12)
      df <- data.frame(Reference, Meter)
      write.csv(df, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # =========================
  # Dataset
  # =========================
  
  dataset <- eventReactive(input$analyze, {
    req(input$data_file)
    df <- read.csv(input$data_file$datapath)
    if(!all(c("Reference", "Meter") %in% colnames(df))) {
      stop("Error: The CSV file must contain exactly 'Reference' and 'Meter' columns.")
    }
    return(df)
  })
  
  # =========================
  # Raw Data Reactive
  # =========================
  
  raw_processed_data <- reactive({
    df <- dataset()
    df$Difference <- df$Meter - df$Reference
    df$Pct_Deviation <- (df$Difference / df$Reference) * 100
    
    # Check ISO 15197:2013 Criteria
    df$ISO_Pass <- ifelse(
      df$Reference < 100,
      abs(df$Difference) <= 15,
      abs(df$Difference) / df$Reference <= 0.15
    )
    
    return(df)
  })
  
  # =========================
  # Raw Data Outputs
  # =========================
  
  output$total_obs <- renderText({
    req(dataset())
    paste("Total number of observations calculated:", nrow(dataset()))
  })
  
  output$raw_data_table <- renderDT({
    req(raw_processed_data())
    df <- raw_processed_data()
    
    datatable(
      df,
      rownames = FALSE,
      colnames = c("Reference", "Meter", "Difference", "Deviation (%)", "ISO_Pass"),
      options = list(
        pageLength = 15,
        columnDefs = list(list(visible = FALSE, targets = 4))
      )
    ) %>%
      formatStyle(
        'Reference',
        backgroundColor = styleInterval(99.99, c('#D9EDF7', '#FCF8E3')), 
        color = styleInterval(99.99, c('#31708F', '#8A6D3B'))
      ) %>%
      formatStyle(
        c('Difference', 'Pct_Deviation'), 'ISO_Pass',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#DFF0D8', '#F2DEDE')),
        color = styleEqual(c(TRUE, FALSE), c('#3C763D', '#A94442'))
      ) %>%
      formatRound(c('Difference', 'Meter', 'Reference', 'Pct_Deviation'), digits = 2)
  })
  
  output$dl_colored_excel <- downloadHandler(
    filename = function() {
      "GlucoSight_RawData.xlsx"
    },
    content = function(file) {
      req(raw_processed_data())
      df <- raw_processed_data()
      
      wb <- createWorkbook()
      addWorksheet(wb, "Raw_Analysis")
      
      export_df <- df[, c("Reference", "Meter", "Difference", "Pct_Deviation")]
      colnames(export_df)[4] <- "Deviation (%)"
      writeData(wb, "Raw_Analysis", export_df)
      
      # Solid cell fills with clear black text
      style_ref_low <- createStyle(
        fgFill = "#D9EDF7",
        fontColour = "#000000"
      )
      style_ref_high <- createStyle(
        fgFill = "#FCF8E3",
        fontColour = "#000000"
      )
      style_diff_pass <- createStyle(
        fgFill = "#DFF0D8",
        fontColour = "#000000"
      )
      style_diff_fail <- createStyle(
        fgFill = "#F2DEDE",
        fontColour = "#000000"
      )
      
      # Row indices offset by 1 for the Excel header row
      low_ref_rows  <- which(df$Reference < 100) + 1
      high_ref_rows <- which(df$Reference >= 100) + 1
      pass_rows     <- which(df$ISO_Pass) + 1
      fail_rows     <- which(!df$ISO_Pass) + 1
      
      # Column 1 (Reference) styling
      if(length(low_ref_rows) > 0) {
        addStyle(wb, "Raw_Analysis", style = style_ref_low, rows = low_ref_rows, cols = 1, gridExpand = TRUE)
      }
      if(length(high_ref_rows) > 0) {
        addStyle(wb, "Raw_Analysis", style = style_ref_high, rows = high_ref_rows, cols = 1, gridExpand = TRUE)
      }
      
      # Columns 3 & 4 (Difference & Deviation (%)) styling
      if(length(pass_rows) > 0) {
        addStyle(wb, "Raw_Analysis", style = style_diff_pass, rows = pass_rows, cols = 3:4, gridExpand = TRUE)
      }
      if(length(fail_rows) > 0) {
        addStyle(wb, "Raw_Analysis", style = style_diff_fail, rows = fail_rows, cols = 3:4, gridExpand = TRUE)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ==========================================
  # Descriptive Statistics Table (Ref & Meter)
  # ==========================================
  
  output$desc_stats_table <- renderTable({
    req(dataset())
    df <- dataset()
    
    calc_desc <- function(x) {
      q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
      c(
        Mean = sprintf("%.2f mg/dL", mean(x, na.rm = TRUE)),
        SD = sprintf("%.2f mg/dL", sd(x, na.rm = TRUE)),
        Median = sprintf("%.2f mg/dL", median(x, na.rm = TRUE)),
        Q1 = sprintf("%.2f mg/dL", q[1]),
        Q3 = sprintf("%.2f mg/dL", q[2]),
        Min = sprintf("%.2f mg/dL", min(x, na.rm = TRUE)),
        Max = sprintf("%.2f mg/dL", max(x, na.rm = TRUE))
      )
    }
    
    stats_ref <- calc_desc(df$Reference)
    stats_meter <- calc_desc(df$Meter)
    
    data.frame(
      Variable = c("Reference", "Meter"),
      Mean = c(stats_ref["Mean"], stats_meter["Mean"]),
      SD = c(stats_ref["SD"], stats_meter["SD"]),
      Median = c(stats_ref["Median"], stats_meter["Median"]),
      Q1 = c(stats_ref["Q1"], stats_meter["Q1"]),
      Q3 = c(stats_ref["Q3"], stats_meter["Q3"]),
      Min = c(stats_ref["Min"], stats_meter["Min"]),
      Max = c(stats_ref["Max"], stats_meter["Max"]),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  })
  
  # ==========================================
  # MARD & Relative Difference Summary Table
  # ==========================================
  
  output$mard_table <- renderTable({
    req(dataset())
    df <- dataset()
    
    ard <- abs(df$Meter - df$Reference) / df$Reference * 100
    diff <- df$Meter - df$Reference
    
    data.frame(
      Metric = c(
        "MARD (Mean Absolute Relative Difference)",
        "Median ARD",
        "Mean Difference (Meter - Reference)",
        "Standard Deviation of Difference"
      ),
      Value = c(
        sprintf("%.2f%%", mean(ard, na.rm = TRUE)),
        sprintf("%.2f%%", median(ard, na.rm = TRUE)),
        sprintf("%.2f mg/dL", mean(diff, na.rm = TRUE)),
        sprintf("%.2f mg/dL", sd(diff, na.rm = TRUE))
      ),
      stringsAsFactors = FALSE
    )
  })
  
  # ==========================================
  # Paired t-Test Logic
  # ==========================================
  
  paired_test_res <- reactive({
    req(dataset())
    df <- dataset()
    t.test(df$Meter, df$Reference, paired = TRUE)
  })
  
  output$ttest_table <- renderTable({
    res <- paired_test_res()
    p_val_display <- if (res$p.value < 0.001) "< 0.001" else sprintf("%.4f", res$p.value)
    
    data.frame(
      Statistic = c(
        "Mean Difference (Meter - Reference)",
        "t-Statistic",
        "Degrees of Freedom (df)",
        "95% Confidence Interval",
        "p-Value"
      ),
      Value = c(
        sprintf("%.2f mg/dL", res$estimate),
        sprintf("%.4f", res$statistic),
        as.character(res$parameter),
        sprintf("[%.2f, %.2f] mg/dL", res$conf.int[1], res$conf.int[2]),
        p_val_display
      )
    )
  })
  
  output$ttest_interpretation <- renderUI({
    res <- paired_test_res()
    if(res$p.value < 0.05) {
      p(
        strong("Statistical Inference: "),
        span("There is a statistically significant difference between the Meter and Reference measurements (p < 0.05).", 
             style = "color: #c0392b; font-weight: bold;")
      )
    } else {
      p(
        strong("Statistical Inference: "),
        span("There is no statistically significant systematic difference between the Meter and Reference measurements (p >= 0.05).", 
             style = "color: #27ae60; font-weight: bold;")
      )
    }
  })
  
  # ==========================================
  # Connected Paired Plot
  # ==========================================
  
  plot_paired <- reactive({
    req(dataset())
    df <- dataset()
    df$ID <- seq_len(nrow(df))
    
    df_long <- tidyr::pivot_longer(
      df,
      cols = c("Reference", "Meter"),
      names_to = "Method",
      values_to = "Glucose"
    )
    
    df_long$Method <- factor(df_long$Method, levels = c("Reference", "Meter"))
    
    ggplot(df_long, aes(x = Method, y = Glucose)) +
      geom_line(aes(group = ID), color = "#7f8c8d", alpha = 0.45, linewidth = 0.7) +
      geom_point(aes(color = Method), size = 3, alpha = 0.85) +
      stat_summary(fun = "mean", geom = "point", shape = 18, size = 5, color = "#c0392b") +
      stat_summary(fun = "mean", geom = "line", aes(group = 1), linewidth = 1.2, linetype = "dashed", color = "#c0392b") +
      scale_color_manual(values = c("Reference" = "#2980b9", "Meter" = "#e67e22")) +
      labs(
        title = "Paired Comparison: Reference vs. Meter Values",
        subtitle = "Gray lines connect individual pairs | Red diamonds & dashed line show group means",
        x = "Measurement Method",
        y = "Glucose Concentration (mg/dL)"
      ) +
      theme_minimal() +
      pub_theme
  })
  
  output$paired_plot <- renderPlot({
    plot_paired()
  })
  
  output$dl_paired <- downloadHandler(
    filename = function() {
      paste0("Paired_Comparison_Plot.", input$paired_ext)
    },
    content = function(file) {
      file.copy(
        download_plot(plot_paired(), input$paired_ext),
        file
      )
    },
    contentType = NA
  )
  
  # =========================
  # Accuracy Data
  # =========================
  
  acc_data <- reactive({
    df <- dataset()
    ref <- df$Reference
    meter <- df$Meter
    n <- nrow(df)
    
    iso_pass <- sum(
      ifelse(
        ref < 100,
        abs(meter - ref) <= 15,
        abs(meter - ref) / ref <= 0.15
      )
    )
    iso_pct <- (iso_pass / n) * 100
    
    fda_15 <- sum(abs(meter - ref) / ref <= 0.15) / n * 100
    fda_20 <- sum(abs(meter - ref) / ref <= 0.20) / n * 100
    ada_pass <- sum(abs(meter - ref) / ref <= 0.05)
    ada_pct <- (ada_pass / n) * 100
    clia_pass <- sum(abs(meter - ref) <= pmax(6, ref * 0.10))
    clia_pct <- (clia_pass / n) * 100
    
    data.frame(
      Criteria = factor(
        c(
          "ISO 15197", "ISO 15197",
          "FDA (15%)", "FDA (15%)",
          "FDA (20%)", "FDA (20%)",
          "ADA", "ADA",
          "CLIA", "CLIA"
        ),
        levels = c("ISO 15197", "FDA (15%)", "FDA (20%)", "ADA", "CLIA")
      ),
      Type = factor(rep(c("Required", "Achieved"), 5), levels = c("Required", "Achieved")),
      Value = c(95, iso_pct, 95, fda_15, 99, fda_20, 100, ada_pct, 80, clia_pct)
    )
  })
  
  # =========================
  # Numerical Statistics Table
  # =========================
  
  output$num_stats <- renderTable({
    df_raw <- acc_data()
    achieved <- df_raw$Value[df_raw$Type == "Achieved"]
    
    data.frame(
      Criteria = c("ISO 15197:2013", "FDA (2016)", "ADA (1994)", "CLIA (1988)"),
      Requirement = c(
        "95% within ±15mg/dL (<100) or ±15% (≥100)",
        "95% within ±15% and 99% within ±20%",
        "100% within ±5%",
        "80% within ±10% or ±6 mg/dL"
      ),
      Result = c(
        paste0(round(achieved[1], 2), "%"),
        paste0(round(achieved[2], 2), "% within ±15%, ", round(achieved[3], 2), "% within ±20%"),
        paste0(round(achieved[4], 2), "%"),
        paste0(round(achieved[5], 2), "%")
      )
    )
  })
  
  # =========================
  # Accuracy Bar Plot
  # =========================
  
  output$acc_bar_chart <- renderPlot({
    df <- acc_data()
    ggplot(df, aes(x = Criteria, y = Value, fill = Type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
      geom_text(
        aes(label = paste0(round(Value, 1), "%")),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        size = 5,
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("Required" = "#bdc3c7", "Achieved" = "#2ecc71")) +
      labs(y = "Accuracy Percentage (%)", x = NULL) +
      theme_minimal() +
      coord_cartesian(ylim = c(0, max(df$Value) + 15)) +
      pub_theme
  })
  
  # =========================
  # Download Helper
  # =========================
  
  download_plot <- function(plot_obj, ext) {
    ext <- tolower(ext)
    temp_file <- file.path(tempdir(), paste0("temp_plot.", ext))
    ggsave(temp_file, plot = plot_obj, device = ext, width = 9, height = 7, dpi = 300)
    return(temp_file)
  }
  
  # =========================
  # CEGA
  # =========================
  
  output$cega_table <- renderTable({
    df <- dataset()
    zones <- getClarkeZones(df$Reference, df$Meter)
    tbl <- as.data.frame(table(Zone = factor(zones, levels = c("A", "B", "C", "D", "E"))))
    tbl$Percentage <- sprintf("%.2f%%", (tbl$Freq / nrow(df)) * 100)
    colnames(tbl) <- c("Zone", "Count", "Percentage")
    tbl
  })
  
  plot_cega <- reactive({
    df <- dataset()
    plotClarkeGrid(df$Reference, df$Meter) + pub_theme
  })
  
  output$cega_plot <- renderPlot({
    plot_cega()
  })
  
  output$dl_cega <- downloadHandler(
    filename = function() {
      paste0("CEGA_Plot.", input$cega_ext)
    },
    content = function(file) {
      file.copy(download_plot(plot_cega(), input$cega_ext), file)
    },
    contentType = NA
  )
  
  # =========================
  # PEGA
  # =========================
  
  output$pega_table <- renderTable({
    df <- dataset()
    pega_type <- as.numeric(input$diabetes_type)
    zones <- getParkesZones(df$Reference, df$Meter, type = pega_type)
    tbl <- as.data.frame(table(Zone = factor(zones, levels = c("A", "B", "C", "D", "E"))))
    tbl$Percentage <- sprintf("%.2f%%", (tbl$Freq / nrow(df)) * 100)
    colnames(tbl) <- c("Zone", "Count", "Percentage")
    tbl
  })
  
  plot_pega <- reactive({
    df <- dataset()
    pega_type <- as.numeric(input$diabetes_type)
    plotParkesGrid(df$Reference, df$Meter, type = pega_type) +
      ggtitle(paste("Parkes Error Grid Analysis (Type", pega_type, "Diabetes)")) +
      pub_theme
  })
  
  output$pega_plot <- renderPlot({
    plot_pega()
  })
  
  output$dl_pega <- downloadHandler(
    filename = function() {
      paste0("PEGA_Plot.", input$pega_ext)
    },
    content = function(file) {
      file.copy(download_plot(plot_pega(), input$pega_ext), file)
    },
    contentType = NA
  )
  
  # =========================
  # Bland Altman Plot
  # =========================
  
  plot_bland <- reactive({
    df <- dataset()
    max_ref <- max(df$Reference, na.rm = TRUE)
    
    ggplot(df, aes(x = Reference, y = (Meter - Reference))) +
      geom_point(color = "#2c3e50", alpha = 0.7, size = 3) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray20", linewidth = 1) +
      annotate("segment", x = 0, y = 15, xend = 100, yend = 15, color = "#27ae60", linetype = "dashed", linewidth = 1.2) +
      annotate("segment", x = 0, y = -15, xend = 100, yend = -15, color = "#27ae60", linetype = "dashed", linewidth = 1.2) +
      annotate("segment", x = 100, y = 15, xend = max_ref, yend = max_ref * 0.15, color = "#27ae60", linetype = "dashed", linewidth = 1.2) +
      annotate("segment", x = 100, y = -15, xend = max_ref, yend = -max_ref * 0.15, color = "#27ae60", linetype = "dashed", linewidth = 1.2) +
      theme_minimal() +
      labs(
        x = "Reference Glucose (mg/dL)",
        y = "Difference (Meter - Reference) (mg/dL)"
      ) +
      pub_theme
  })
  
  output$bland_altman <- renderPlot({
    plot_bland()
  })
  
  output$dl_bland <- downloadHandler(
    filename = function() {
      paste0("BlandAltman_Plot.", input$bland_ext)
    },
    content = function(file) {
      file.copy(download_plot(plot_bland(), input$bland_ext), file)
    },
    contentType = NA
  )
}

# =========================
# Run App
# =========================

shinyApp(ui = ui, server = server)
