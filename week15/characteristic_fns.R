library(tidyverse)
library(plotly)

bern_char <- function(t, p = .5) {1 - p + p * exp( 1i * t ) }

results <- list()

for (t in seq(-10, 10, length.out=100)) {
  phi_t <- bern_char(t)
  results[[length(results)+1]] <- 
    c(t = t, `phi_t_Re` = Re(phi_t), phi_t_Im = Im(phi_t)) 
}

results <- bind_rows(results)

plot_ly(results, x = ~t, y = ~phi_t_Re, z = ~phi_t_Im, type = 'scatter3d', mode = 'lines+markers',
        line = list(width = 6, color = ~phi_t_Im, colorscale = 'Viridis'),
        marker = list(size = 3.5, color = ~c, colorscale = 'Greens', cmin = -20, cmax = 50))


char_fn_plotter <- function(
  char_fn, 
  range = seq(-10,10,length.out = 1000),
  ...) {

  # evaluate characteristic function on given range 
  results <- list()
  for (t in range) {
    phi_t <- char_fn(t, ...)
    results[[length(results)+1]] <- 
      c(t = t, `phi_t_Re` = Re(phi_t), phi_t_Im = Im(phi_t)) 
  }
  results <- bind_rows(results) # construct dataframe 

  # apply 3d plotly
  plot_ly(results, x = ~t, y = ~phi_t_Re, z = ~phi_t_Im, 
      type = 'scatter3d', mode = 'lines',
      line = list(width = 6, color = ~phi_t_Im, colorscale = 'Viridis'))
}

degenerate_char <- function(t, a = 1) { exp(1i * t * a)}
binomial_char <- function(t, p = 0.5, n = 10) { (1 - p + p * exp( 1i * t ))^n }
poisson_char <- function(t, lambda = 0.3) { exp(lambda * (exp(1i * t - 1 )))}
unif_cont_char <- function(t, a = 0, b = 1) { (exp(1i * t * b) - exp(1i * t * a)) / (1i * t * (b - a)) }
laplace_char <- function(t, mu, b) { exp(1i * t * mu) / (1 + b^2 * t^2) }
normal_char <- function(t, mu, sigma_sqrd) {  exp(1i * t * mu - .5 * sigma_sqrd * t^2) }
chi_sqrd_char <- function(t, k) { (1-2 * 1i * t)^{-k / 2} }
cauchy_char <- function(t, mu, theta) { exp(1i * t * mu - theta * abs(t)) }
gamma_char <- function(t, k, theta) { (1 - 1i * t * theta)^{-k} }
exponential_char <- function(t, lambda) { (1-1i*t*lambda^{-1})^{-1} }



char_fn_plotter(degenerate_char) |> layout(title = 'Degenerate Characteristic Function')
char_fn_plotter(bern_char, p = .5) |> layout(title = 'Bernoulli Characteristic Function')
char_fn_plotter(binomial_char, p = .5) |> layout(title = 'Binomial Characteristic Function')

char_fn_plotter(poisson_char, lambda = 100, range = seq(-10,10,length.out = 10000)) |>
  layout(title = 'Poisson Characteristic Function')

char_fn_plotter(unif_cont_char, range = seq(-100, 100, length.out = 1000)) |> 
  layout(title = "Continuous Uniform Characteristic Function")

char_fn_plotter(laplace_char, mu = 1, b = 5, range = seq(-50, 50, length.out = 10000)) %>% layout(title = "Laplace Characteristic Function")

char_fn_plotter(normal_char, mu = 1, sigma_sqrd = 1, range = seq(-10, 10, length.out = 10000)) %>% layout(title = "Normal Characteristic Function")

char_fn_plotter(chi_sqrd_char, k = 1, range = seq(-100, 100, length.out = 10000)) %>% layout(title = "Chi^2 Characteristic Function")

char_fn_plotter(cauchy_char, mu = 1, theta = 1, range = seq(-10, 10, length.out = 10000)) %>% layout(title = "Cauchy Characteristic Function")

char_fn_plotter(gamma_char, k = 10, theta = 12, range = seq(-1, 1, length.out = 10000)) %>% layout(title = "Gamma Characteristic Function")

char_fn_plotter(exponential_char, lambda = 0.1, range = seq(-10, 10, length.out = 10000)) %>% layout(title = "Exponential Characteristic Function")
