library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(scales)
library(plotly)
library(reshape2)
library(lubridate)
library(tidyr)
library(forecast)
library(tseries)
library(zoo)

function(input, output, session) {
  # Landing page transition
  observeEvent(input$enter_dashboard, {
    shinyjs::hide("landing-page")
    shinyjs::show("dashboard-container")
  })
  
  # Define color palette untuk setiap tipe bencana
  disaster_colors <- c(
    "Kebakaran" = "#FF6B6B", 
    "Angin Topan" = "#c649ea", 
    "Tornado" = "#32cd32",   
    "Banjir" = "#7cb7f1",     
    "Gempa Bumi" = "#FFBE0B" 
  )
  
  # Read and prepare data
  disaster_data <- reactive({
    df <- read.csv("natural_disasters_2024.csv", check.names = FALSE)
    df$Date <- as.POSIXct(df$Date)
    df$Month <- month(df$Date, label = TRUE)
    return(df)
  })
  
  # Filter data based on selected disaster type
  filtered_data <- reactive({
    data <- disaster_data()
    if(input$disaster_type != "All") {
      data <- data %>% filter(Disaster_Type == input$disaster_type)
    }
    return(data)
  })
  
  # Calculate summary statistics
  output$total_disasters <- renderText({
    nrow(filtered_data())
  })
  
  output$total_fatalities <- renderText({
    format(sum(filtered_data()$Fatalities), big.mark = ",")
  })
  
  output$avg_loss <- renderText({
    mean_loss <- mean(filtered_data()$`Economic_Loss($)`)
    paste0("$", format(round(mean_loss/1e6, 1), big.mark = ","), "M")
  })
  
  output$max_magnitude <- renderText({
    round(max(filtered_data()$Magnitude), 2)
  })
  
  # Plot1 output
  output$plot1 <- renderPlotly({
    data <- filtered_data()
    
    # Helper function to create text display configuration
    getTextInfo <- function() {
      if(input$show_values && input$show_percentage) {
        return('label+value+percent')
      } else if(input$show_values) {
        return('label+value')
      } else if(input$show_percentage) {
        return('label+percent')
      }
      return('label')
    }
    
    if(input$plot_type == "fatalities") {
      if(input$disaster_type == "All") {
        summary_data <- data %>%
          group_by(Disaster_Type) %>%
          summarise(Total = sum(Fatalities)) %>%
          ungroup() %>%
          arrange(desc(Total))
        
        summary_data$Percentage <- round(summary_data$Total/sum(summary_data$Total) * 100, 1)
        total_value <- format(sum(summary_data$Total), big.mark = ",")
        
        p <- plot_ly(
          data = summary_data,
          labels = ~Disaster_Type,
          values = ~Total,
          type = 'pie',
          textposition = 'outside',
          textinfo = getTextInfo(),
          hoverinfo = 'text',
          text = ~paste(
            Disaster_Type,
            "<br>Total Korban Jiwa:", format(Total, big.mark = ","),
            "<br>Persentase:", Percentage, "%"
          ),
          marker = list(
            colors = c("#FF4136", "#7FDBFF", "#2ECC40", "#B10DC9", "#FF851B"),
            line = list(color = '#FFFFFF', width = 2)
          ),
          hole = input$hole_size,
          rotation = 90,
          pull = rep(input$pull_size, nrow(summary_data)),
          direction = 'clockwise',
          sort = FALSE
        ) %>% add_annotations(
          text = paste("Total\nFatalities\n", total_value),
          x = 0.5,
          y = 0.5,
          font = list(size = 12, color = '#333333', family = 'Arial'),
          showarrow = FALSE
        )
        title <- "Distribusi Korban Jiwa Berdasarkan Jenis Bencana"
        
      } else {
        total_fatalities <- sum(data$Fatalities)
        percentage <- "100%"
        
        p <- plot_ly(
          labels = ~input$disaster_type,
          values = ~total_fatalities,
          type = 'pie',
          textposition = 'outside',
          textinfo = getTextInfo(),
          hoverinfo = 'text',
          text = paste(
            input$disaster_type,
            "<br>Total Korban Jiwa:", format(total_fatalities, big.mark = ","),
            "<br>Persentase:", percentage
          ),
          marker = list(
            colors = input$plot_color,
            line = list(color = '#FFFFFF', width = 2)
          ),
          hole = input$hole_size,
          pull = input$pull_size
        ) %>% add_annotations(
          text = paste("Total\n", format(total_fatalities, big.mark = ",")),
          x = 0.5,
          y = 0.5,
          font = list(size = 12, color = '#333333', family = 'Arial'),
          showarrow = FALSE
        )
        title <- paste("Korban Jiwa -", input$disaster_type)
      }
      
    } else if(input$plot_type == "loss") {
      if(input$disaster_type == "All") {
        summary_data <- data %>%
          group_by(Disaster_Type) %>%
          summarise(Total = sum(`Economic_Loss($)`)/1e6) %>%
          ungroup() %>%
          arrange(desc(Total))
        
        summary_data$Percentage <- round(summary_data$Total/sum(summary_data$Total) * 100, 1)
        total_value <- paste("$", format(round(sum(summary_data$Total)), big.mark = ","), "M")
        
        p <- plot_ly(
          data = summary_data,
          labels = ~Disaster_Type,
          values = ~Total,
          type = 'pie',
          textposition = 'outside',
          textinfo = getTextInfo(),
          hoverinfo = 'text',
          text = ~paste(
            Disaster_Type,
            "<br>Economic Loss: $", format(round(Total), big.mark = ","), "M",
            "<br>Percentage:", Percentage, "%"
          ),
          marker = list(
            colors = c("#FF4136", "#7FDBFF", "#2ECC40", "#B10DC9", "#FF851B"),
            line = list(color = '#FFFFFF', width = 2)
          ),
          hole = input$hole_size,
          rotation = 90,
          pull = rep(input$pull_size, nrow(summary_data)),
          direction = 'clockwise',
          sort = FALSE
        ) %>% add_annotations(
          text = paste("Total Loss\n", total_value),
          x = 0.5,
          y = 0.5,
          font = list(size = 12, color = '#333333', family = 'Arial'),
          showarrow = FALSE
        )
        title <- "Distribusi Kerugian Ekonomi berdasarkan Jenis Bencana"
      } else {
        total_loss <- sum(data$`Economic_Loss($)`)/1e6
        percentage <- "100%"
        
        p <- plot_ly(
          labels = ~input$disaster_type,
          values = ~total_loss,
          type = 'pie',
          textposition = 'outside',
          textinfo = getTextInfo(),
          hoverinfo = 'text',
          text = paste(
            input$disaster_type,
            "<br>Economic Loss: $", format(round(total_loss), big.mark = ","), "M",
            "<br>Percentage:", percentage
          ),
          marker = list(
            colors = input$plot_color,
            line = list(color = '#FFFFFF', width = 2)
          ),
          hole = input$hole_size,
          pull = input$pull_size
        ) %>% add_annotations(
          text = paste("Total Loss\n$", format(round(total_loss), big.mark = ","), "M"),
          x = 0.5,
          y = 0.5,
          font = list(size = 12, color = '#333333', family = 'Arial'),
          showarrow = FALSE
        )
        title <- paste("Economic Loss -", input$disaster_type)
      }
    } else if(input$plot_type == "magnitude") {
      if(input$disaster_type == "All") {
        summary_data <- data %>%
          group_by(Disaster_Type) %>%
          summarise(
            Avg_Magnitude = mean(Magnitude, na.rm = TRUE),
            Total_Events = n()
          ) %>%
          ungroup() %>%
          arrange(desc(Avg_Magnitude))
        
        summary_data$Percentage <- round(summary_data$Total_Events/sum(summary_data$Total_Events) * 100, 1)
        overall_avg <- round(mean(data$Magnitude, na.rm = TRUE), 2)
        
        p <- plot_ly(
          data = summary_data,
          labels = ~Disaster_Type,
          values = ~Avg_Magnitude,
          type = 'pie',
          textposition = 'outside',
          textinfo = getTextInfo(),
          hoverinfo = 'text',
          text = ~paste(
            Disaster_Type,
            "<br>Average Magnitude:", round(Avg_Magnitude, 2),
            "<br>Total Events:", Total_Events,
            "<br>Percentage of Events:", Percentage, "%"
          ),
          marker = list(
            colors = c("#FF4136", "#7FDBFF", "#2ECC40", "#B10DC9", "#FF851B"),
            line = list(color = '#FFFFFF', width = 2)
          ),
          hole = input$hole_size,
          rotation = 90,
          direction = 'clockwise',
          sort = FALSE
        ) %>% add_annotations(
          text = paste("Overall Avg\nMagnitude\n", overall_avg),
          x = 0.5,
          y = 0.5,
          font = list(size = 12, color = '#333333', family = 'Arial'),
          showarrow = FALSE
        )
        title <- "Rata-rata Magnitudo berdasarkan Jenis Bencana"
        
      } else {
        avg_magnitude <- mean(data$Magnitude, na.rm = TRUE)
        total_events <- nrow(data)
        
        p <- plot_ly(
          labels = ~input$disaster_type,
          values = ~avg_magnitude,
          type = 'pie',
          textposition = 'outside',
          textinfo = getTextInfo(),
          hoverinfo = 'text',
          text = paste(
            input$disaster_type,
            "<br>Average Magnitude:", round(avg_magnitude, 2),
            "<br>Total Events:", total_events
          ),
          marker = list(
            colors = input$plot_color,
            line = list(color = '#FFFFFF', width = 2)
          ),
          hole = input$hole_size
        ) %>% add_annotations(
          text = paste("Average\nMagnitude\n", round(avg_magnitude, 2)),
          x = 0.5,
          y = 0.5,
          font = list(size = 12, color = '#333333', family = 'Arial'),
          showarrow = FALSE
        )
        title <- paste("Magnitude Analysis -", input$disaster_type)
      }
    }
    
    # Common layout for all plots
    p %>% layout(
      title = list(
        text = title,
        y = 0.95,
        x = 0.5,
        xanchor = 'center',
        yanchor = 'top',
        font = list(size = 16, color = '#333333', family = 'Arial')
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2,
        bgcolor = 'rgba(255, 255, 255, 0.9)',
        bordercolor = 'rgba(0, 0, 0, 0.2)',
        borderwidth = 1,
        font = list(size = 11, family = 'Arial')
      ),
      margin = list(
        l = 30,
        r = 30,
        t = 80,
        b = 100,
        pad = 4
      ),
      height = 450,
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 11, family = 'Arial'),
        bordercolor = "black"
      )
    )
    
    # Common layout for all plots
    p %>% layout(
      title = list(
        text = title,
        y = 0.95,
        x = 0.5,
        xanchor = 'center',
        yanchor = 'top',
        font = list(size = 16, color = '#333333', family = 'Arial')
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2,  # Nilai sebelumnya -0.1, diubah menjadi -0.2 untuk menurunkan posisi
        bgcolor = 'rgba(255, 255, 255, 0.9)',
        bordercolor = 'rgba(0, 0, 0, 0.2)',
        borderwidth = 1,
        font = list(size = 11, family = 'Arial')
      ),
      margin = list(
        l = 30,
        r = 30,
        t = 80,
        b = 100,  # Ditambahkan margin bottom agar ada ruang untuk legend
        pad = 4
      ),
      height = 450,
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 11, family = 'Arial'),
        bordercolor = "black"
      )
    )
  })
  
  
  # Location plot with sorted bars
  output$location_plot <- renderPlotly({
    data <- filtered_data()
    
    if(input$disaster_type == "All") {
      location_disaster_summary <- data %>%
        group_by(Location, Disaster_Type) %>%
        summarise(Total_Fatalities = sum(Fatalities), .groups = 'drop')
      
      # Calculate total fatalities per location for sorting
      location_totals <- location_disaster_summary %>%
        group_by(Location) %>%
        summarise(Total = sum(Total_Fatalities)) %>%
        arrange(desc(Total))
      
      # Set the order of locations based on total fatalities
      location_disaster_summary <- location_disaster_summary %>%
        mutate(Location = factor(Location, levels = location_totals$Location))
      
      p <- ggplot(location_disaster_summary, 
                  aes(x = Location, y = Total_Fatalities, fill = Disaster_Type,
                      text = paste("Lokasi:", Location,
                                   "<br>Tipe:", Disaster_Type,
                                   "<br>Total:", format(Total_Fatalities, big.mark = ",")))) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = format(Total_Fatalities, big.mark = ",")),
                  position = position_stack(vjust = 0.5),
                  color = "white", size = 4) +
        scale_fill_manual(values = disaster_colors) +
        theme_minimal() +
        labs(title = "Korban Jiwa Berdasarkan Lokasi dan Jenis Bencana",
             x = "Lokasi",
             y = "Total Korban Jiwa") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, hjust = 0.5))
    } else {
      location_summary <- data %>%
        group_by(Location) %>%
        summarise(Total_Fatalities = sum(Fatalities)) %>%
        arrange(desc(Total_Fatalities)) %>%  # Sort in descending order
        mutate(Location = factor(Location, levels = Location))  # Preserve the order
      
      p <- ggplot(location_summary, 
                  aes(x = Location, y = Total_Fatalities,
                      text = paste("Lokasi:", Location,
                                   "<br>Total:", format(Total_Fatalities, big.mark = ",")))) +
        geom_bar(stat = "identity", fill = input$plot_color) +
        geom_text(aes(label = format(Total_Fatalities, big.mark = ",")),
                  position = position_stack(vjust = 0.5),
                  color = "white", size = 4) +
        theme_minimal() +
        labs(title = "Korban Jiwa Berdasarkan Lokasi",
             x = "Lokasi",
             y = "Total Korban Jiwa") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, hjust = 0.5))
    }
    
    # Convert to plotly with adjusted layout
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(
          tickangle = 0,
          tickfont = list(size = 10)
        ),
        margin = list(b = 50, l = 50, r = 50, t = 50)
      )
  })
  
  # Location plot with sorted bars
  output$location_plot <- renderPlotly({
    data <- filtered_data()
    
    if(input$disaster_type == "All") {
      # Calculate total per location and disaster type
      location_disaster_summary <- data %>%
        group_by(Location, Disaster_Type) %>%
        summarise(Total_Fatalities = sum(Fatalities), .groups = 'drop')
      
      # Calculate total fatalities per location for sorting
      location_totals <- location_disaster_summary %>%
        group_by(Location) %>%
        summarise(Total = sum(Total_Fatalities)) %>%
        arrange(desc(Total))  # Sort in descending order
      
      # Set the factor levels based on total fatalities
      location_disaster_summary <- location_disaster_summary %>%
        mutate(Location = factor(Location, levels = location_totals$Location))
      
      p <- ggplot(location_disaster_summary, 
                  aes(x = Location, y = Total_Fatalities, fill = Disaster_Type,
                      text = paste("Lokasi:", Location,
                                   "<br>Tipe:", Disaster_Type,
                                   "<br>Total:", format(Total_Fatalities, big.mark = ",")))) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = format(Total_Fatalities, big.mark = ",")),
                  position = position_stack(vjust = 0.5),
                  color = "white", size = 4) +
        scale_fill_manual(values = disaster_colors) +
        theme_minimal() +
        labs(title = "Korban Jiwa Berdasarkan Lokasi dan Jenis Bencana",
             x = "Lokasi",
             y = "Total Korban Jiwa") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, hjust = 0.5))
    } else {
      # For single disaster type, sort by total fatalities
      location_summary <- data %>%
        group_by(Location) %>%
        summarise(Total_Fatalities = sum(Fatalities)) %>%
        arrange(desc(Total_Fatalities)) %>%  # Sort in descending order
        mutate(Location = factor(Location, levels = Location))  # Preserve the order
      
      p <- ggplot(location_summary, 
                  aes(x = Location, y = Total_Fatalities,
                      text = paste("Lokasi:", Location,
                                   "<br>Total:", format(Total_Fatalities, big.mark = ",")))) +
        geom_bar(stat = "identity", fill = input$plot_color) +
        geom_text(aes(label = format(Total_Fatalities, big.mark = ",")),
                  position = position_stack(vjust = 0.5),
                  color = "white", size = 4) +
        theme_minimal() +
        labs(title = "Korban Jiwa berdasarkan Lokasi",
             x = "Lokasi",
             y = "Total Korban Jiwa") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, hjust = 0.5))
    }
    
    # Convert to plotly with adjusted layout
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(
          tickangle = 0,
          tickfont = list(size = 10)
        ),
        margin = list(b = 50, l = 50, r = 50, t = 50)
      )
  })
  
  output$summary_stats_table <- renderDT({
    data <- disaster_data()
    var <- input$desc_var
    group_by <- input$desc_group
    
    if(group_by == "none") {
      stats <- data %>%
        summarise(
          N = n(),
          Mean = mean(get(var)),
          SD = sd(get(var)),
          Min = min(get(var)),
          Q1 = quantile(get(var), 0.25),
          Median = median(get(var)),
          Q3 = quantile(get(var), 0.75),
          Max = max(get(var))
        )
    } else {
      stats <- data %>%
        group_by(get(group_by)) %>%
        summarise(
          N = n(),
          Mean = mean(get(var)),
          SD = sd(get(var)),
          Min = min(get(var)),
          Q1 = quantile(get(var), 0.25),
          Median = median(get(var)),
          Q3 = quantile(get(var), 0.75),
          Max = max(get(var))
        )
      names(stats)[1] <- if(group_by == "Disaster_Type") "Tipe Bencana" else "Lokasi"
    }
    
    # Format numbers
    stats <- stats %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    datatable(
      stats,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(stats),
        backgroundColor = 'rgba(240, 240, 240, 0.5)'
      )
  })
  
  output$distribution_stats_table <- renderDT({
    data <- disaster_data()
    var <- input$desc_var
    group_by <- input$desc_group
    
    if(group_by == "none") {
      stats <- data %>%
        summarise(
          Skewness = moments::skewness(get(var)),
          Kurtosis = moments::kurtosis(get(var)),
          Range = diff(range(get(var))),
          IQR = IQR(get(var)),
          CV = sd(get(var)) / mean(get(var)) * 100,  # Coefficient of Variation
          SE = sd(get(var)) / sqrt(n())  # Standard Error
        )
    } else {
      stats <- data %>%
        group_by(get(group_by)) %>%
        summarise(
          Skewness = moments::skewness(get(var)),
          Kurtosis = moments::kurtosis(get(var)),
          Range = diff(range(get(var))),
          IQR = IQR(get(var)),
          CV = sd(get(var)) / mean(get(var)) * 100,
          SE = sd(get(var)) / sqrt(n())
        )
      names(stats)[1] <- if(group_by == "Disaster_Type") "Tipe Bencana" else "Lokasi"
    }
    
    # Format numbers
    stats <- stats %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    datatable(
      stats,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(stats),
        backgroundColor = 'rgba(240, 240, 240, 0.5)'
      )
  })
  
  output$dist_plot <- renderPlotly({
    data <- disaster_data()
    var <- input$desc_var
    group_by <- input$desc_group
    
    if(group_by == "none") {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~`", var, "`")),
        type = "histogram",
        name = var,
        marker = list(
          color = "#3c8dbc",
          line = list(color = "white", width = 1)
        )
      ) %>%
        add_trace(
          type = "violin",
          side = "negative",
          line = list(color = "gray"),
          showlegend = FALSE
        )
    } else {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~`", var, "`")),
        color = as.formula(paste0("~`", group_by, "`")),
        type = "histogram",
        opacity = 0.7
      )
    }
    
    p %>% layout(
      title = paste("Distribusi Berdasarkan", gsub("_", " ", var)),
      xaxis = list(title = gsub("_", " ", var)),
      yaxis = list(title = "Jumlah"),
      barmode = "overlay",
      bargap = 0.1,
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      showlegend = TRUE
    )
  })
  
  # Time Series Plot
  output$time_series_plot <- renderPlotly({
    data <- disaster_data()
    
    daily_summary <- data %>%
      group_by(Date, Disaster_Type) %>%
      summarise(
        Total_Fatalities = sum(Fatalities),
        Total_Loss = sum(`Economic_Loss($)`)/1e6,
        .groups = 'drop'
      )
    
    p <- ggplot(daily_summary, aes(x = Date, y = Total_Fatalities, 
                                   color = Disaster_Type,
                                   text = paste("Date:", format(Date, "%Y-%m-%d"),
                                                "<br>Tipe:", Disaster_Type,
                                                "<br>Korban Jiwa:", format(Total_Fatalities, big.mark = ","),
                                                "<br>Kerugian: $", format(round(Total_Loss, 1), big.mark = ","), "M"))) +
      geom_line() +
      geom_point(size = 2) +
      scale_color_manual(values = disaster_colors) +
      theme_minimal() +
      labs(title = "Dampak Bencana dari Waktu ke Waktu",
           x = "Tanggal",
           y = "Total Korban Jiwa") +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Monthly Heatmap
  output$monthly_heatmap <- renderPlotly({
    data <- disaster_data()
    
    monthly_summary <- data %>%
      group_by(Month, Disaster_Type) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Disaster_Type, values_from = Count, values_fill = 0)
    
    melted_data <- melt(monthly_summary, id.vars = "Month")
    
    p <- ggplot(melted_data, aes(x = Month, y = variable, fill = value,
                                 text = paste("Bulan:", Month,
                                              "<br>Tipe:", variable,
                                              "<br>Jumlah:", value))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#3c8dbc") +
      theme_minimal() +
      labs(title = "Distribusi Bencana Berdasarkan Bulan",
           x = "Bulan",
           y = "Tipe Bencana",
           fill = "Jumlah") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 0, hjust = 0.5),  # Mengubah sudut teks menjadi 0
            axis.text.y = element_text(angle = 0, hjust = 1))    # Memastikan teks y-axis juga lurus
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          tickangle = 0,  # Memastikan tick labels lurus di plotly
          tickfont = list(size = 11)
        ),
        yaxis = list(
          tickangle = 0,  # Memastikan tick labels lurus di plotly
          tickfont = list(size = 11)
        ),
        margin = list(
          l = 100,  # Menambah margin kiri untuk akomodasi label yang lebih panjang
          r = 50,
          b = 50,
          t = 50
        )
      )
  })
  
  # trend
  output$trend_analysis <- renderPlotly({
    data <- disaster_data()
    
    if(input$trend_disaster_type != "All") {
      data <- data %>% filter(Disaster_Type == input$trend_disaster_type)
    }
    
    monthly_data <- data %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth, Disaster_Type) %>%
      summarise(
        Disaster_Count = n(),
        Total_Loss = sum(`Economic_Loss($)`)/1e6,
        Avg_Magnitude = mean(Magnitude),
        .groups = 'drop'
      )
    
    if(input$trend_disaster_type == "All") {
      monthly_data <- monthly_data %>%
        group_by(YearMonth) %>%
        summarise(
          Disaster_Count = sum(Disaster_Count),
          Total_Loss = sum(Total_Loss),
          Avg_Magnitude = mean(Avg_Magnitude),
          .groups = 'drop'
        )
    }
    
    plot_data <- monthly_data %>%
      mutate(
        Value = case_when(
          input$trend_metric == "count" ~ Disaster_Count,
          input$trend_metric == "loss" ~ Total_Loss,
          input$trend_metric == "magnitude" ~ Avg_Magnitude
        ),
        BarText = case_when(  # Text for inside bars
          input$trend_metric == "count" ~ as.character(Disaster_Count),
          input$trend_metric == "loss" ~ paste0("$", format(round(Total_Loss, 1), big.mark = ",")),
          input$trend_metric == "magnitude" ~ as.character(round(Avg_Magnitude, 2))
        ),
        HoverText = paste("Date:", format(YearMonth, "%b<br>%Y"),
                          "<br>", case_when(
                            input$trend_metric == "count" ~ paste(Disaster_Count, "disasters"),
                            input$trend_metric == "loss" ~ paste0("$", format(round(Total_Loss, 1), big.mark = ","), "M"),
                            input$trend_metric == "magnitude" ~ paste("Magnitude:", round(Avg_Magnitude, 2))
                          ))
      )
    
    y_min <- min(plot_data$Value, na.rm = TRUE)
    y_max <- max(plot_data$Value, na.rm = TRUE)
    y_range <- y_max - y_min
    y_min <- max(0, y_min - y_range * 0.1)
    y_max <- y_max + y_range * 0.1
    
    y_title <- case_when(
      input$trend_metric == "count" ~ "Jumlah Bencana",
      input$trend_metric == "loss" ~ "Kerugian Ekonomi (Juta $)",
      input$trend_metric == "magnitude" ~ "Rata-rata Magnitudo"
    )
    
    if(input$trend_view == "line") {
      p <- plot_ly(plot_data, x = ~YearMonth, y = ~Value,
                   type = 'scatter', mode = 'lines+markers',
                   line = list(width = 3),
                   marker = list(size = 8),
                   hoverinfo = 'text',
                   text = ~HoverText)
    } else if(input$trend_view == "bar") {
      p <- plot_ly(plot_data, x = ~YearMonth, y = ~Value,
                   type = 'bar',
                   text = ~BarText,  # Use simplified text for bars
                   textposition = 'inside',
                   insidetextanchor = 'middle',
                   textangle = 0,
                   hoverinfo = 'text',
                   hovertext = ~HoverText)
    } else {
      p <- plot_ly(plot_data, x = ~YearMonth, y = ~Value,
                   type = 'scatter', mode = 'none', fill = 'tozeroy',
                   hoverinfo = 'text',
                   text = ~HoverText)
    }
    
    p %>% layout(
      title = paste(y_title, "Berdasarkan Bulan"),
      xaxis = list(
        title = list(
          text = "Bulan",
          standoff = 0
        ),
        tickformat = "%b<br>%Y",
        tickangle = 0,
        tickfont = list(size = 11),
        dtick = "M1",
        automargin = TRUE
      ),
      yaxis = list(
        title = y_title,
        tickfont = list(size = 11),
        range = c(y_min, y_max)
      ),
      showlegend = FALSE,
      margin = list(
        l = 50,
        r = 30,
        t = 50,
        b = 60,
        pad = 4
      ),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)'
    )
  })
  
  # Correlation Plot
  output$correlation_plot <- renderPlotly({
    data <- disaster_data()
    
    correlation_data <- data %>%
      select(Magnitude, Fatalities, `Economic_Loss($)`) %>%
      cor()
    
    melted_correlation <- melt(correlation_data)
    
    p <- plot_ly(
      x = melted_correlation$Var1,
      y = melted_correlation$Var2,
      z = melted_correlation$value,
      type = "heatmap",
      colors = colorRamp(c("#FF6B6B", "white", "#4ECDC4")),
      text = ~paste0(round(melted_correlation$value, 3)),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Matriks Korelasi",
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        annotations = list(
          x = melted_correlation$Var1,
          y = melted_correlation$Var2,
          text = round(melted_correlation$value, 3),
          showarrow = FALSE,
          font = list(size = 14)
        )
      )
    
    p
  })
  
  # Box Plot Analysis with Adjusted Text Position
  output$boxplot_analysis <- renderPlotly({
    data <- disaster_data()
    
    # Get selected variable
    var <- input$boxplot_var
    
    # Create box plot data
    plot_data <- data.frame(
      Disaster_Type = data$Disaster_Type,
      Value = data[[var]]
    )
    
    # Calculate summary statistics for hover text
    summary_stats <- plot_data %>%
      group_by(Disaster_Type) %>%
      summarise(
        Min = min(Value),
        Q1 = quantile(Value, 0.25),
        Median = median(Value),
        Q3 = quantile(Value, 0.75),
        Max = max(Value),
        Mean = mean(Value),
        n = n()
      )
    
    # Create box plot
    p <- plot_ly(
      data = plot_data,
      x = ~Disaster_Type,
      y = ~Value,
      type = "box",
      color = ~Disaster_Type,
      colors = c("#FFBE0B", "#7cb7f1", "#84de2f", "#c649ea", "#FF6B6B"),
      boxpoints = if(input$show_outliers) 'suspectedoutliers' else FALSE,
      jitter = 0.3,
      pointpos = -1.8,
      marker = list(
        size = 4,
        opacity = 0.5
      ),
      hoverinfo = "text",
      text = ~paste(
        "Tipe Bencana:", Disaster_Type,
        "<br>Nilai:", round(Value, 2)
      )
    )
    
    # Add summary statistics text with increased yshift
    for(i in 1:nrow(summary_stats)) {
      p <- add_annotations(
        p,
        x = summary_stats$Disaster_Type[i],
        y = max(plot_data$Value),
        text = paste(
          "n =", summary_stats$n[i],
          "<br>Mean =", round(summary_stats$Mean[i], 2)
        ),
        showarrow = FALSE,
        font = list(size = 10),
        yshift = 30  # Increased yshift from 10 to 40
      )
    }
    
    # Customize layout with adjusted margins
    p %>% layout(
      title = paste("Distribusi Berdasarkan", gsub("_", " ", var), "Berdasarkan Tipe Bencana"),
      xaxis = list(
        title = "Tipe Bencana",
        tickangle = 0
      ),
      yaxis = list(
        title = gsub("_", " ", var),
        zeroline = TRUE,
        zerolinecolor = '#ddd',
        gridcolor = '#eee',
        # Add extra space at the top for annotations
        range = c(min(plot_data$Value), max(plot_data$Value) * 1.15)  # Increase top range by 15%
      ),
      showlegend = FALSE,
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      font = list(family = "Arial"),
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial", size = 12)
      ),
      margin = list(
        l = 50,
        r = 50,
        b = 50,
        t = 80  # Increased top margin
      )
    )
  })
  
  # Severity Analysis Plot
  output$severity_analysis <- renderPlotly({
    data <- disaster_data()
    
    # Create severity score
    severity_data <- data %>%
      mutate(
        fatality_score = scale(Fatalities),
        economic_score = scale(`Economic_Loss($)`),
        magnitude_score = scale(Magnitude),
        severity_score = (fatality_score + economic_score + magnitude_score)/3
      ) %>%
      mutate(
        severity_category = case_when(
          severity_score <= -0.5 ~ "Low",
          severity_score <= 0.5 ~ "Medium",
          TRUE ~ "High"
        )
      )
    
    # Hitung distribusi severity untuk setiap tipe bencana
    severity_dist <- severity_data %>%
      group_by(Disaster_Type, severity_category) %>%
      summarise(
        count = n(),
        .groups = 'drop'
      ) %>%
      group_by(Disaster_Type) %>%
      mutate(
        percentage = count / sum(count) * 100,
        severity_category = factor(severity_category, levels = c("Low", "Medium", "High"))
      )
    
    # Buat stacked bar chart
    p <- plot_ly(
      data = severity_dist,
      x = ~Disaster_Type,
      y = ~percentage,
      color = ~severity_category,
      type = "bar",
      colors = c("Low" = "#90CAF9", "Medium" = "#FFF59D", "High" = "#EF9A9A"),
      text = ~paste0(round(percentage, 1), "%"),
      textposition = "inside",
      insidetextanchor = "middle",
      hoverinfo = "text",
      hovertext = ~paste(
        "Tipe Bencana:", Disaster_Type,
        "<br>Tingkat Keparahan:", severity_category,
        "<br>Jumlah:", count,
        "<br>Persentase:", round(percentage, 1), "%"
      )
    ) %>%
      layout(
        title = "Distribusi Tingkat Keparahan berdasarkan Jenis Bencana",
        barmode = "stack",
        showlegend = TRUE,
        legend = list(
          title = list(text = "Tingkat Keparahan"),
          x = 1.1,            # Posisi legend di kanan
          y = 0.5,            # Vertikal di tengah
          xanchor = "left",   # Anchor dari sisi kiri legend
          yanchor = "middle", # Anchor dari tengah legend secara vertikal
          bgcolor = "rgba(255, 255, 255, 0.8)"
        ),
        xaxis = list(
          title = "Tipe Bencana",
          tickangle = 45
        ),
        yaxis = list(
          title = "Persentase (%)",
          range = c(0, 100)
        ),
        uniformtext = list(
          minsize = 8,
          mode = "hide"
        ),
        margin = list(
          l = 50,  # margin kiri
          r = 150, # margin kanan diperbesar untuk legend
          b = 100, # margin bawah
          t = 50   # margin atas
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    
    p
  })
  
  # Regression Analysis
  output$regression_plot <- renderPlotly({
    # Get data
    data <- disaster_data()
    
    # Filter by disaster type if needed
    if(input$reg_type != "All") {
      data <- subset(data, Disaster_Type == input$reg_type)
    }
    
    # Prepare variables
    x_var <- input$reg_x_var
    y_var <- input$reg_y_var
    
    # Define color palette for disaster types
    disaster_colors <- c(
      "Kebakaran" = "#FF6B6B",    
      "Angin Topan" = "#c649ea",    
      "Tornado" = "#84de2f",      
      "Banjir" = "#7cb7f1",        
      "Gempa Bumi" = "#FFBE0B"    
    )
    
    # Create and fit the regression model
    model_data <- data.frame(
      x = data[[x_var]],
      y = data[[y_var]]
    )
    fit <- lm(y ~ x, data = model_data)
    summary_fit <- summary(fit)
    
    # Calculate additional statistics
    f_stat <- summary_fit$fstatistic[1]
    f_p_value <- pf(summary_fit$fstatistic[1], 
                    summary_fit$fstatistic[2], 
                    summary_fit$fstatistic[3], 
                    lower.tail = FALSE)
    
    # Create regression details text
    reg_details <- sprintf(
      "&nbsp;&nbsp;Regression Statistics:&nbsp;&nbsp;<br>
       &nbsp;&nbsp;R² = %.3f   |   Adj.R² = %.3f&nbsp;&nbsp;<br>
       &nbsp;&nbsp;F-stat = %.2f   |   p-value = %.4f&nbsp;&nbsp;<br>
       &nbsp;&nbsp;Res.Std.Error = %.2f&nbsp;&nbsp;<br>
       &nbsp;&nbsp;N = %d&nbsp;&nbsp;",
      summary_fit$r.squared,
      summary_fit$adj.r.squared,
      f_stat,
      f_p_value,
      summary_fit$sigma,
      nrow(data)
    )
    
    # Buat plot dasar dengan warna berdasarkan Disaster_Type
    p <- plot_ly() %>%
      add_trace(
        data = data,
        x = as.formula(paste0("~`", x_var, "`")),
        y = as.formula(paste0("~`", y_var, "`")),
        type = 'scatter',
        mode = 'markers',
        color = ~Disaster_Type,  
        colors = disaster_colors, 
        marker = list(
          size = 10,
          opacity = 0.6
        ),
        name = ~Disaster_Type,   
        text = ~paste(
          "Tipe Bencara:", Disaster_Type,
          "<br>", gsub("_", " ", x_var), ":", round(get(x_var), 2),
          "<br>", gsub("_", " ", y_var), ":", round(get(y_var), 2)
        ),
        hoverinfo = 'text'
      )
    
    # Add regression line if requested
    if(input$show_reg_eq) {
      # Create prediction data
      new_x <- seq(min(model_data$x), max(model_data$x), length.out = 100)
      pred_data <- data.frame(
        x = new_x,
        y = predict(fit, newdata = data.frame(x = new_x))
      )
      
      # Add regression line
      p <- p %>% add_trace(
        data = pred_data,
        x = ~x,
        y = ~y,
        type = 'scatter',
        mode = 'lines',
        line = list(
          color = 'black',
          width = 3
        ),
        name = 'Regression Line',
        showlegend = TRUE
      )
      
      # Add equation
      eq <- sprintf(
        "&nbsp;&nbsp;y = %.2f + %.2fx<br>R² = %.3f&nbsp;&nbsp;",
        coef(fit)[1],
        coef(fit)[2],
        summary_fit$r.squared
      )
      
      p <- p %>% add_annotations(
        text = eq,
        x = 0.02,
        y = 0.98,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        bgcolor = "rgba(255, 255, 255, 0.8)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1,
        pad = list(
          t = 10,
          b = 10,
          l = 10,
          r = 10
        )
      )
      
      # Add regression details box with padding
      p <- p %>% add_annotations(
        text = reg_details,
        x = 1.55,
        y = 0.35,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "left",
        bgcolor = "rgba(255, 255, 255, 0.8)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1,
        font = list(size = 11),
        pad = list(
          t = 15,    # Top padding
          b = 15,    # Bottom padding
          l = 15,    # Left padding
          r = 15     # Right padding
        )
      )
    }
    
    # Final layout adjustments with increased right margin
    p %>% layout(
      title = paste("Regression Analysis:", gsub("_", " ", y_var), "vs", gsub("_", " ", x_var)),
      xaxis = list(
        title = gsub("_", " ", x_var),
        zeroline = TRUE,
        zerolinecolor = '#ddd',
        gridcolor = '#eee'
      ),
      yaxis = list(
        title = gsub("_", " ", y_var),
        zeroline = TRUE,
        zerolinecolor = '#ddd',
        gridcolor = '#eee'
      ),
      showlegend = TRUE,
      legend = list(
        x = 1.15,    # Adjusted legend position
        y = 0.9,
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1
      ),
      margin = list(
        l = 50,
        r = 250,     # Increased right margin
        b = 50,
        t = 50
      ),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      hovermode = 'closest'
    )
  })
  
  output$bubble_chart <- renderPlotly({
    # Get data
    data <- disaster_data()
    
    # Agregasi data per tipe bencana
    summary_data <- data %>%
      group_by(Disaster_Type) %>%
      summarise(
        Avg_Fatalities = mean(Fatalities),
        Avg_Economic_Loss = mean(`Economic_Loss($)`)/1e6, # Dalam jutaan
        Avg_Magnitude = mean(Magnitude),
        Count = n()
      )
    
    # Create bubble chart
    p <- plot_ly(
      data = summary_data,
      x = ~Avg_Economic_Loss,
      y = ~Avg_Fatalities,
      size = ~Count,
      color = ~Disaster_Type,
      colors = c("#FFBE0B", "#7cb7f1", "#c649ea", "#84de2f", "#FF6B6B"),
      type = 'scatter',
      mode = 'markers',
      marker = list(
        sizemode = 'area',
        sizeref = 2 * max(summary_data$Count) / (50^2),  # Adjusted size reference
        sizemin = 4,
        opacity = 0.8,  # Slightly increased opacity
        line = list(color = '#FFFFFF', width = 1)  # Added white border
      ),
      text = ~paste(
        "Tipe Bencana:", Disaster_Type,
        "<br>Rata-Rata Korban Jiwa:", round(Avg_Fatalities, 1),
        "<br>Rata-rata Kerugian Ekonomi: $", round(Avg_Economic_Loss, 1), "M",
        "<br>Rata-rata Magnitude:", round(Avg_Magnitude, 2),
        "<br>Jumlah Kejadian:", Count
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = "Analisis Dampak Bencana - Ukuran menunjukkan jumlah kejadian/peristiwa",
          y = 0.95  # Adjusted title position
        ),
        xaxis = list(
          title = "Rata-rata Kerugian Ekonomi (Juta $)",
          zeroline = FALSE,
          gridcolor = '#E5E5E5',  # Lighter grid
          automargin = TRUE
        ),
        yaxis = list(
          title = "Rata-rata Korban Jiwa",
          zeroline = FALSE,
          gridcolor = '#E5E5E5',  # Lighter grid
          automargin = TRUE
        ),
        showlegend = TRUE,
        legend = list(
          x = 0.02,  # Adjusted legend position
          y = 0.98,
          bgcolor = 'rgba(255, 255, 255, 0.9)',
          bordercolor = 'rgba(0, 0, 0, 0.2)',
          borderwidth = 1
        ),
        hovermode = 'closest',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(
          l = 60,   # Left margin
          r = 30,   # Right margin
          t = 50,   # Top margin
          b = 60,   # Bottom margin
          pad = 4   # Padding
        ),
        autosize = TRUE,  # Enable auto-sizing
        height = 350      # Set specific height to match box
      )
    
    p
  })
  
  # Time series forecast plot
  # Complete revised forecast plot implementation
  output$forecast_plot <- renderPlotly({
    # Get and prepare data
    data <- disaster_data()
    
    # Filter by disaster type if needed
    if(input$forecast_type != "All") {
      data <- subset(data, Disaster_Type == input$forecast_type)
    }
    
    # Aggregate data by month based on selected variable
    monthly_data <- data %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month) %>%
      summarise(
        count = n(),
        fatalities = sum(Fatalities),
        economic_loss = sum(`Economic_Loss($)`)/1e6,
        .groups = 'drop'
      ) %>%
      arrange(Month)
    
    # Create time series object
    ts_data <- ts(monthly_data[[input$forecast_var]], 
                  frequency = 12, 
                  start = c(year(min(monthly_data$Month)), 
                            month(min(monthly_data$Month))))
    
    # Generate forecast based on selected model
    tryCatch({
      forecast_result <- if(input$forecast_model == "sma") {
        # Simple Moving Average implementation
        # Calculate optimal window size
        acf_values <- acf(ts_data, plot = FALSE)
        optimal_window <- max(2, min(
          which.min(abs(acf_values$acf[-1])),
          floor(length(ts_data)/4)
        ))
        
        # Calculate moving averages
        ma_values <- stats::filter(ts_data, 
                                   rep(1/optimal_window, optimal_window), 
                                   sides = 1)
        
        # Prepare data for forecasting
        valid_ma <- ma_values[!is.na(ma_values)]
        time_idx <- seq_along(valid_ma)
        
        # Fit linear model on MA data
        ma_model <- lm(valid_ma ~ time_idx)
        
        # Generate forecasts
        new_time_idx <- seq(max(time_idx) + 1, 
                            max(time_idx) + input$forecast_periods)
        point_forecasts <- predict(ma_model, 
                                   newdata = data.frame(time_idx = new_time_idx))
        
        # Calculate prediction intervals
        pred_intervals <- predict(ma_model, 
                                  newdata = data.frame(time_idx = new_time_idx),
                                  interval = "prediction", 
                                  level = 0.95)
        
        # Create forecast object
        structure(
          list(
            mean = point_forecasts,
            lower = cbind(
              pred_intervals[,"lwr"] - 0.84 * (pred_intervals[,"upr"] - pred_intervals[,"lwr"])/2,
              pred_intervals[,"lwr"]
            ),
            upper = cbind(
              pred_intervals[,"upr"] + 0.84 * (pred_intervals[,"upr"] - pred_intervals[,"lwr"])/2,
              pred_intervals[,"upr"]
            ),
            fitted = c(rep(NA, length(ts_data) - length(valid_ma)), fitted(ma_model)),
            method = "Simple Moving Average (SMA)",
            x = ts_data
          ),
          class = "forecast"
        )
      } else if(input$forecast_model == "arima") {
        # ARIMA implementation
        model <- auto.arima(ts_data,
                            seasonal = TRUE,
                            stepwise = FALSE,
                            approximation = FALSE)
        forecast(model, h = input$forecast_periods, level = c(80, 95))
      } else {
        # ETS implementation
        model <- ets(ts_data, model = "ZZZ")
        forecast(model, h = input$forecast_periods, level = c(80, 95))
      }
      
      # Calculate forecast dates
      forecast_dates <- seq(max(monthly_data$Month), 
                            by = "month", 
                            length.out = input$forecast_periods + 1)[-1]
      
      # Create base plot with historical data
      p <- plot_ly() %>%
        add_trace(
          x = monthly_data$Month,
          y = monthly_data[[input$forecast_var]],
          type = "scatter",
          mode = "lines+markers",
          name = "Historis",
          line = list(color = "#3c8dbc"),
          marker = list(color = "#3c8dbc", size = 8),
          hoverinfo = "text",
          text = ~paste(
            "Date:", format(monthly_data$Month, "%B %Y"),
            "<br>Value:", round(monthly_data[[input$forecast_var]], 2)
          )
        )
      
      # Add forecast line
      p <- p %>% add_trace(
        x = forecast_dates,
        y = forecast_result$mean,
        type = "scatter",
        mode = "lines+markers",
        name = "Prediksi",
        line = list(color = "#FF6B6B", dash = "dash"),
        marker = list(color = "#FF6B6B", size = 8),
        hoverinfo = "text",
        text = ~paste(
          "Bulan:", format(forecast_dates, "%B %Y"),
          "<br>Peramalan:", round(forecast_result$mean, 2)
        )
      )
      
      # Add confidence intervals if requested
      if(input$show_ci) {
        # Add 95% confidence interval
        p <- p %>% add_ribbons(
          x = forecast_dates,
          ymin = forecast_result$lower[,2],
          ymax = forecast_result$upper[,2],
          name = "95% CI",
          fillcolor = "rgba(60, 141, 188, 0.2)",
          line = list(color = "transparent"),
          showlegend = TRUE
        )
        
        # Add 80% confidence interval
        p <- p %>% add_ribbons(
          x = forecast_dates,
          ymin = forecast_result$lower[,1],
          ymax = forecast_result$upper[,1],
          name = "80% CI",
          fillcolor = "rgba(60, 141, 188, 0.1)",
          line = list(color = "transparent"),
          showlegend = TRUE
        )
      }
      
      # Calculate accuracy metrics
      if(input$forecast_model == "sma") {
        # Manual calculation for SMA
        fitted_values <- forecast_result$fitted[!is.na(forecast_result$fitted)]
        actual_values <- forecast_result$x[(length(forecast_result$x) - length(fitted_values) + 1):length(forecast_result$x)]
        
        errors <- actual_values - fitted_values
        accuracy_metrics <- c(
          RMSE = sqrt(mean(errors^2, na.rm = TRUE)),
          MAE = mean(abs(errors), na.rm = TRUE),
          MAPE = mean(abs(errors/actual_values), na.rm = TRUE) * 100
        )
      } else {
        # Use accuracy function for ARIMA and ETS
        accuracy_metrics <- accuracy(forecast_result)[, c("RMSE", "MAE", "MAPE")]
      }
      
      # Create title with accuracy metrics
      title_text <- paste(
        "Peramalan Deret Waktu -",
        switch(input$forecast_var,
               "count" = "Jumlah Bencana",
               "fatalities" = "Korban Jiwa",
               "economic_loss" = "Kerugian Ekonomi (Juta $)"),
        "<br>",
        "<sub>Model:", input$forecast_model, "| ",
        "RMSE:", round(accuracy_metrics["RMSE"], 2), "| ",
        "MAE:", round(accuracy_metrics["MAE"], 2), "| ",
        "MAPE:", round(accuracy_metrics["MAPE"], 2), "%</sub>"
      )
      
      # Customize layout
      p %>% layout(
        title = list(
          text = title_text,
          y = 0.95,
          x = 0.5,
          xanchor = "center",
          yanchor = "top"
        ),
        xaxis = list(
          title = "Date",
          tickfont = list(size = 10),
          tickangle = 0,
          tickformat = "%b %Y"
        ),
        yaxis = list(
          title = switch(input$forecast_var,
                         "count" = "Jumlah Bencana",
                         "fatalities" = "Korban Jiwa",
                         "economic_loss" = "Kerugian Ekonomi (Juta $)"),
          tickfont = list(size = 10)
        ),
        showlegend = TRUE,
        legend = list(
          x = 0.02,
          y = 0.98,
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "rgba(0, 0, 0, 0.2)",
          borderwidth = 1,
          font = list(size = 10)
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 100, l = 60, r = 40, b = 60)
      )
    }, error = function(e) {
      # Return error plot if forecasting fails
      plot_ly() %>%
        add_annotations(
          x = 0.5,
          y = 0.5,
          text = paste("Error in forecasting:", e$message),
          showarrow = FALSE,
          font = list(size = 14, color = "red")
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
  })
  
  output$forecast_table <- renderDT({
    # Get and prepare data
    data <- disaster_data()
    
    # Filter by disaster type if needed
    if(input$forecast_type != "All") {
      data <- subset(data, Disaster_Type == input$forecast_type)
    }
    
    # Aggregate data by month
    monthly_data <- data %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month) %>%
      summarise(
        count = n(),
        fatalities = sum(Fatalities),
        economic_loss = sum(`Economic_Loss($)`)/1e6,
        .groups = 'drop'
      ) %>%
      arrange(Month)
    
    # Create time series object
    ts_data <- ts(monthly_data[[input$forecast_var]], 
                  frequency = 12, 
                  start = c(year(min(monthly_data$Month)), 
                            month(min(monthly_data$Month))))
    
    # Generate forecast and create results table
    tryCatch({
      # Generate forecast based on selected model
      forecast_result <- if(input$forecast_model == "sma") {
        # SMA implementation
        acf_values <- acf(ts_data, plot = FALSE)
        optimal_window <- max(2, min(
          which.min(abs(acf_values$acf[-1])),
          floor(length(ts_data)/4)
        ))
        
        ma_values <- stats::filter(ts_data, 
                                   rep(1/optimal_window, optimal_window), 
                                   sides = 1)
        
        valid_ma <- ma_values[!is.na(ma_values)]
        time_idx <- seq_along(valid_ma)
        
        ma_model <- lm(valid_ma ~ time_idx)
        
        new_time_idx <- seq(max(time_idx) + 1, 
                            max(time_idx) + input$forecast_periods)
        point_forecasts <- predict(ma_model, 
                                   newdata = data.frame(time_idx = new_time_idx))
        
        pred_intervals <- predict(ma_model, 
                                  newdata = data.frame(time_idx = new_time_idx),
                                  interval = "prediction", 
                                  level = 0.95)
        
        list(
          mean = point_forecasts,
          lower = pred_intervals[, "lwr"],
          upper = pred_intervals[, "upr"],
          model = ma_model,
          method = "Simple Moving Average (SMA)"
        )
        
      } else if(input$forecast_model == "arima") {
        model <- auto.arima(ts_data,
                            seasonal = TRUE,
                            stepwise = FALSE,
                            approximation = FALSE)
        forecast(model, h = input$forecast_periods)
      } else {
        model <- ets(ts_data, model = "ZZZ")
        forecast(model, h = input$forecast_periods)
      }
      
      # Create forecast dates
      forecast_dates <- seq(max(monthly_data$Month), 
                            by = "month", 
                            length.out = input$forecast_periods + 1)[-1]
      
      # Prepare forecast values and intervals
      forecast_values <- as.numeric(forecast_result$mean)
      lower_bounds <- if(input$forecast_model == "sma") {
        forecast_result$lower
      } else {
        forecast_result$lower[, 2]
      }
      upper_bounds <- if(input$forecast_model == "sma") {
        forecast_result$upper
      } else {
        forecast_result$upper[, 2]
      }
      
      # Get appropriate variable name for column header
      var_name <- switch(input$forecast_var,
                         "count" = "Bencana",
                         "fatalities" = "Korban Jiwa",
                         "economic_loss" = "Kerugian Ekonomi")
      
      # Create results dataframe with clean column names
      results_df <- data.frame(
        "Period" = format(forecast_dates, "%B %Y"),
        "Forecast" = format(round(forecast_values, 2), big.mark = ","),
        "Lower Limit" = format(round(lower_bounds, 2), big.mark = ","),
        "Upper Limit" = format(round(upper_bounds, 2), big.mark = ",")
      )
      
      # Add currency formatting for economic loss
      if(input$forecast_var == "economic_loss") {
        results_df$Forecast <- paste0("$", results_df$Forecast, "M")
        results_df$`Lower Limit` <- paste0("$", results_df$`Lower Limit`, "M")
        results_df$`Upper Limit` <- paste0("$", results_df$`Upper Limit`, "M")
      }
      
      # Create and style the table
      datatable(
        results_df,
        options = list(
          pageLength = input$forecast_periods,
          dom = 't',
          scrollX = TRUE,
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Period",
          paste("Forecast", var_name),
          "Lower Bound",
          "Upper Bound"
        )
      ) %>%
        formatStyle(
          columns = colnames(results_df),
          backgroundColor = 'rgba(240, 240, 240, 0.5)',
          fontSize = '12px',
          borderBottom = '1px solid #ddd'
        )
      
    }, error = function(e) {
      # Return error message if forecasting fails
      datatable(
        data.frame(
          Error = paste("Forecasting error:", e$message)
        ),
        options = list(
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
      )
    })
  })
  # Data table
  output$data_table <- renderDT({
    data <- disaster_data()
    
    # Filter based on disaster type
    if (input$data_disaster_filter != "All") {
      data <- data[data$Disaster_Type == input$data_disaster_filter, ]
    }
    
    # Filter based on location
    if (input$data_location_filter != "All") {
      data <- data[data$Location == input$data_location_filter, ]
    }
    
    # Format data for display
    data <- data %>%
      mutate(
        Date = format(Date, "%Y-%m-%d %H:%M"),
        `Economic_Loss($)` = paste0("$", format(round(`Economic_Loss($)`, 2), big.mark = ","))
      )
    
    # Create datatable
    datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      rownames = FALSE,
      selection = 'none',
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Disaster_Type',
        backgroundColor = styleEqual(
          c("Kebakaran", "Angin Topan", "Tornado", "Banjir", "Gempa Bumi"),
          c("#FF6B6B", "#c649ea", "#84de2f", "#7cb7f1", "#FFBE0B")
        ),
        color = 'white'
      )
  })
}