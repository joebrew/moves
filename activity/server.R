library(shiny)
source('helpers.R')
shinyServer(function(input, output) {

  agg_reactive <- reactive({
    agg[which(agg$Name %in% input$person),]
  })
  
  individual <- reactive({
    agg[which(agg$Name == input$person2),]
  })
  
  
  output$plot1 <- renderPlot({

    plot(agg_reactive()$diary_time,
         agg_reactive()$app_time,
        xlim = c(0, 150),
        ylim = c(0, 150),
         pch = agg_reactive()$shape,
         cex = 1,
         col = agg_reactive()$color,
        xlab = 'Minutes (per diary)',
        ylab = 'Minutes (per phone)',
        main = 'Agreement between diary and phone app\n(one point = a person\'s cumulative activity-specific time each day)')
    grid()
    
    
    legend('bottomright',
           pch = c(21, 15:18),
           legend = c('Joe', 'Xu', 'Sheldon', 'Jake', 'Yun'),
           col = 'grey',
           title = 'Who',
           cex = 0.8,
           ncol = 2)
    
    legend('bottom',
           fill = adjustcolor(c('darkgreen', 'darkred', 'darkblue'), alpha.f = 0.6),
           legend = c('Walking', 'Running', 'Cycling'),
           title = 'What', cex = 0.8,
           border = NA)
    
    abline(a = 0, b = 1)
    text(x = 120, y = 120,
         labels = 'Line of\nperfect match',
         col = adjustcolor('black', alpha.f = 0.6))
    
    fit <- lm(app_time ~ diary_time,
              data = agg_reactive())
    legend('top',
           lty = 2,
           col = 'red',
           bty = 'n',
           legend = paste0('R-squared:\n', 
                           round(summary(fit)$r.squared, digits = 3) ))
    
    text(x = 10,
         y = fit$coefficients[1],
         col = adjustcolor('red', alpha.f = 0.6), 
         labels = 'Line of best fit')
    abline(fit,
           col = 'red', lty = 2)

  })
  
  output$plot2 <- renderPlot({
    plot(individual()$diary_time,
         individual()$app_time,
         xlim = c(0, 150),
         ylim = c(0, 150),
         pch = individual()$shape,
         cex = 1,
         col = individual()$color,
         xlab = 'Minutes (per diary)',
         ylab = 'Minutes (per phone)',
         main = paste0('Agreement between diary and phone app\n(one point = ', input$person2,'\'s cumulative activity-specific time each day)'))
    grid()
    
    
    legend('bottomright',
           pch = c(21, 15:18),
           legend = c('Joe', 'Xu', 'Sheldon', 'Jake', 'Yun'),
           col = 'grey',
           title = 'Who',
           cex = 0.8,
           ncol = 2)
    
    legend('bottom',
           fill = adjustcolor(c('darkgreen', 'darkred', 'darkblue'), alpha.f = 0.6),
           legend = c('Walking', 'Running', 'Cycling'),
           title = 'What', cex = 0.8,
           border = NA)
    
    abline(a = 0, b = 1)
    text(x = 120, y = 120,
         labels = 'Line of\nperfect match',
         col = adjustcolor('black', alpha.f = 0.6))
    
    fit <- lm(app_time ~ diary_time,
              data = individual())
    legend('top',
          lty = 2,
          col = 'red',
          bty = 'n',
           legend = paste0('R-squared:\n', 
                           round(summary(fit)$r.squared, digits = 3) ))
    
    abline(fit,
           col = 'red', lty = 2)
    text(x = 10,
         y = fit$coefficients[1],
         col = adjustcolor('red', alpha.f = 0.6), 
         labels = 'Line of best fit')
    
    })
  
  output$plot3 <- renderPlot({
    par(mfrow = c(1,2))
    par(mar = c(4,4,2,1))
    temp <- 
      individual() %>%
      group_by(Activity) %>%
      summarise(app_time = sum(app_time),
                diary_time = sum(diary_time))
    
    cols <- adjustcolor(c('darkblue', 'darkgreen'), alpha.f = 0.6)
    obj <- t(as.matrix(temp[,2:3]))
    bp <- barplot(obj, beside = T,
            col = cols,
            border = 'darkgrey',
            names.arg = temp$Activity,
            ylim = c(0, max(obj) * 1.35),
            main = 'Cumulative minutes')
    box('plot')
    legend('topleft', 
           fill = cols,
           border = 'darkgrey',
           legend = c('Phone', 'Diary'))
    text(x = as.numeric(bp),
         y = as.numeric(obj),
         labels = round(as.numeric(obj), digits = 1),
         col = adjustcolor('black', alpha.f = 0.6),
         cex = 0.8,
         pos = 3)
    
    
    
    # PLOT COMPARE
    temp <- 
      individual() %>%
      group_by(Activity) %>%
      summarise(app_time = sum(app_time),
                diary_time = sum(diary_time),
                color = color[1])
    
    vals <- temp$app_time - temp$diary_time
    
    y_min <- ifelse(min(vals) >= 0, 
                    -0.1 * max(vals),
                    min(vals)-(0.1*max(vals)))
    
    bp <- barplot(vals, col = temp$color,
                  border = 'darkgrey',
                  main = 'Phone minutes minus diary minutes',
                  xlab = 'Activity',
                  ylab = 'Minutes',
                  yaxt = 'n',
                  names.arg = temp$Activity,
                  ylim = c(y_min, max(vals) * 1.35))
    skipper <- ifelse(max(vals) > 300, 100, 25)
    axis(side = 2, 
         at = seq(-1000, 1000, skipper),
         labels = seq(-1000, 1000, skipper))
    abline(h = 0, lty = 2)
    
    text(x = bp[,1],
         y = vals,
         pos = ifelse(vals < 0, 1, 3),
         labels = round(vals, digits = 2))
    
    box('plot')
    
    text(x = mean(bp[,1]),
         y = 0, 
         pos = 1,
         labels = 'Line of agreement')
    legend('topleft',
           fill = c(NA, NA),
           border = NA,
           legend = c('Positive = phone recorded more',
                      'Negative = diary recorded more'))
    
    
    
  })
  
  output$plot0 <- renderPlot({
    temp <- 
      agg_reactive() %>%
      group_by(Activity) %>%
      summarise(app_time = sum(app_time),
                diary_time = sum(diary_time))
    
    cols <- adjustcolor(c('darkblue', 'darkgreen'), alpha.f = 0.6)
    obj <- t(as.matrix(temp[,2:3]))
    bp <- barplot(obj, beside = T,
                  col = cols,
                  border = 'darkgrey',
                  names.arg = temp$Activity,
                  ylim = c(0, max(obj) * 1.15),
                  main = 'Cumulative minutes')
    box('plot')
    
    legend('topleft', 
           fill = cols,
           border = 'darkgrey',
           legend = c('Phone', 'Diary'))
    text(x = as.numeric(bp),
         y = as.numeric(obj),
         labels = round(as.numeric(obj), digits = 1),
         col = adjustcolor('black', alpha.f = 0.6),
         cex = 1.6,
         pos = 3)
  })
  
  output$plot_ts <- renderPlot({
    temp <- 
      individual() %>%
      group_by(Date, Activity) %>%
      summarise(app_time = sum(app_time),
                diary_time = sum(diary_time),
                color = color[1])
    
    plot(temp$Date,
         temp$app_time,
         ylim = c(0, max(c(temp$app_time, temp$diary_time))* 1.1),
         type = 'n',
         xlab = 'Date',
         ylab = 'Minutes',
         main = 'Activity by day')
    
    for (i in unique(temp$Activity)){
      sub_temp <- temp[which(temp$Activity == i),]
      lines(x = sub_temp$Date,
            y = sub_temp$app_time,
            col = sub_temp$color[1],
            lty = 2)
      points(x = sub_temp$Date,
             y = sub_temp$app_time,
             col = sub_temp$color[1],
             pch = 17)
      
      lines(x = sub_temp$Date,
            y = sub_temp$diary_time,
            col = sub_temp$color[1],
            lty = 1)
      points(x = sub_temp$Date,
             y = sub_temp$diary_time,
             col = sub_temp$color[1],
             pch = 16)
    }
    
    legend('topright',
           fill = adjustcolor(c('darkgreen', 'darkred', 'darkblue'), alpha.f = 0.6),
           legend = c('Walking', 'Running', 'Cycling'),
           border = NA)
    legend('top',
           col = 'grey',
           legend = c('Phone', 'Diary'),
           pch = c(17, 16),
           lty = c(2,1))
    
    
    
  })


  
  output$who <- renderText({
    paste0('Individual analysis of ', input$person2)
  })
})
