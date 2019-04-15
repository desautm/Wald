library(tidyverse)

wald <- c(400,320,32,20,4,2,2)

# wald <- tibble(N = 400, Si = c(320,32,20,4,2,2))
# wald <- wald %>%
#   mutate(
#     S = sum(Si),
#     D = N - S,
#     si = Si/N
#     )


waldQ <- function(data){

  n <- length(data)
  pz <- polyroot(c(-data[n:3],data[1]-data[2]))
  id <- (abs(Im(pz))<0.005)
  q <- Re(pz[id])
  return(q)

}

waldD <- function(data){

  n <- length(data)
  q <- waldQ(data)
  p <- 1-q

  D <- rep(0,n-1)

  for (i in 2:(n-1)) {

    D[i] <- p*(data[1]-sum(data[2:i])-sum(D[2:i]))

  }

  return(D)

}

D <- waldD(wald)

polynome <- function(x,data){

  n <- length(data)
  coeff <- c(-data[n:3],data[1]-data[2])
  result <- 0
  for (i in 1:length(coeff)){

    result <- result + coeff[i]*x^i

  }

  return(result)

}


ggplot(data = data.frame(x = c(0,1)), aes(x))+
  stat_function(fun = polynome,
                args = list(data = wald),
                geom = "line",
                colour = "blue")

