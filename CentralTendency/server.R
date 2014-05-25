setwd("/MSAN/Github/Statistics/CentralTendency")
library(shiny)
library(ggplot2)

shape1 <- c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5,5,5,5,5)
shape2 <- c(5,5,5,5,5,4.75,4.5,4.25,4,3.75,3.5,3.25,3,2.75,2.5,2.25,2,1.75,1.5,1.25,1)

gen_random <- function(indeces) {
  x <- rbeta(100000,shape1[indeces],shape2[indeces])
  return(x)
}

compute_median <- function(target) {
  len <- length(target)
  if (len %% 2 == 0) {
    med <- (sort(target)[len/2] + sort(target)[len/2 + 1])/2
  } else {
    med <- sort(target)[floor(len/2) + 1]
  } 
  return(med)
}

compute_mode <- function(target) {
  a <- data.frame(sort(table(target),decreasing=T))
  mod <- as.numeric(row.names(a))[1]
  return(mod)
}

compute_mode2 <- function(target) {
  den_x <- density(target)
  mod <- den_x$x[which.max(den_x$y)]
  return(mod)
}

get_density_plot <- function(indeces) {
  
  x <- gen_random(indeces)
  
  r_x <- round(x,2)
  dfr <- data.frame(x)
  compute_mean <- sum(x)/length(x)
  mean_X <- compute_mean
  median_X <- compute_median(x)
  mode_X <- compute_mode2(r_x)
  p <- ggplot(dfr,aes(x))+geom_density()+
        geom_vline(xintercept=mean_X,color="#e41a1c") +
        annotate("text",label="Mean",x=mean_X,y=2,fontface=3) +
        geom_vline(xintercept=median_X,color="#377eb8") +
        annotate("text",label="Median",x=median_X,y=1.5,fontface=3) +
        geom_vline(xintercept=mode_X,color="#4daf4a") +
        annotate("text",label="Mode",x=mode_X,y=1,fontface=3) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(plot.background = element_blank()) +
        theme(axis.ticks.x = element_blank()) +
        scale_y_continuous(expand=c(0,0))
  return(p)
}

### Shiny Server ###
shinyServer(function(input, output) {
  cat("Press \'ESC\' to exit…\n")
  
  output$densityPlot <- renderPlot({
    viz <- get_density_plot(input$start)
    print(viz)
  },bg="transparent")
})
