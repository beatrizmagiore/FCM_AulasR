library(ggplot2)

plotar <- function(x,y){
  df <- data.frame(x,y)
  
  ggplot(df)+
    geom_point(aes(x=x,y=y))
}

plotar(c(1,2,3),c(2,4,6))