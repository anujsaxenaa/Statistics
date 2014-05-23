#setwd("/MSAN/Github/Statistics/task1")
library(shiny)
library(ggplot2)
library(modeest)

shape1 <- c(1,1.5,2,2.5,3,3.5,4,4.5,5,5,5,5,5)
shape2 <- c(5,5,5,5,5,4.5,4,3.5,3,2.5,2,1.5,1)

gen_random <- function(indeces) {
  x <- rbeta(10000,shape1[indeces],shape2[indeces])
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

get_density_plot <- function(indeces) {
  
  x <- gen_random(indeces)
  
  r_x <- round(x,3)
  dfr <- data.frame(x)
  compute_mean <- sum(x)/length(x)
  mean_X <- compute_mean
  median_X <- compute_median(x)
  mode_X <- compute_mode(r_x)
  p <- ggplot(dfr,aes(x))+geom_density()+
        geom_vline(xintercept=mean_X,color="#e41a1c") +
        annotate("text",label="Mean",x=mean_X,y=2) +
        geom_vline(xintercept=median_X,color="#377eb8") +
        annotate("text",label="Median",x=median_X,y=1.5) +
        geom_vline(xintercept=mode_X,color="#4daf4a") +
        annotate("text",label="Mode",x=mode_X,y=1) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank())
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
