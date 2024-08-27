library(ruin)
model <- CramerLundberg(initial_capital = 10,
                        premium_rate = 1,
                        claim_poisson_arrival_rate = 0.1,
                        claim_size_generator = rexp,
                        claim_size_parameters = list(rate = 1))
path <- simulate_path(model = model, max_time_horizon = 10)
plot_path(path)



mygenerator <- function(n=1, mass, values) {
  if (length(mass) != length(values)){
    stop("mass and values are not the same size")
  }
  if (sum(mass) != 1) {
    stop("mass function does not sum to 1")
  }


  res <- rep(0, n)
  for (j in seq(length(res))) {
    k <- runif(1)
    i <- 1
    acc <- mass[1]
    while (i <= length(mass)) {
      if (k < acc) {
        res[j] = values[i]
        break
      }
      acc = acc + mass[i+1]
      i = i + 1
    }
  }
  return(res)
}

mass <- c(0.4,0.22,0.3,0.08)
values <- c(0.5, 1, 2, 2.5)
model2 <- CramerLundberg(initial_capital = 10,
                        premium_rate = 1,
                        claim_poisson_arrival_rate = 1,
                        claim_size_generator = mygenerator,
                        claim_size_parameters = (list(n=1,mass=mass,values=values)))
path2 <- simulate_path(model = model2, max_time_horizon = 1000)
plot_path(path2)
