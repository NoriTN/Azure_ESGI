# Databricks notebook source
#################################
# Exercice 2
#################################

require(graphics)
library(ggplot2)
library(functional)

#1
#f (x, y) = exp(x*y) - exp(x) + 2
#Calcul du gradient de f 
f_expr = expression(exp(x * y) - exp(x) + 2)
dfx     = D(f_expr, 'x')
dfy     = D(f_expr, 'y')
#(x=0, y=1) est un point critique parce que ???f(X0) = 0
#Il est clair � partir du gradient que : 
  # Quand y = 1 : dx = 0
  # Quand y < 1 : dx < 0
  # Quand y > 1 : dx > 0

#Matrice haussienne de f
ddx    = D(dx, 'x')
ddy    = D(dy, 'y')

f <- function(x, y) {exp(x * y) - exp(x) + 2}
#Attribuer � y les valeurs 0, 1, 2
f_given_y_zero <- Curry(f, y = 0)
f_given_y_one <- Curry(f, y = 1)
f_given_y_two <- Curry(f, y = 2)

minimum_f = optimize(f_given_y_zero, c(0,2), maximum = FALSE)
maximum_f = optimize(f_given_y_zero, c(0,2), maximum = TRUE)

plot(f_given_y_zero, from=0, to=2, ylab = paste( "f | y = ", 0))
plot(f_given_y_one, from=0, to=2, ylab = paste( "f | y = ", 1))
plot(f_given_y_two, from=0, to=2, ylab = paste( "f | y = ", 2))

#2
#g(x, y, z) = (x + z^2) * exp(y^2 + z^2 + 1)
g_expr = expression((x + z^2) * exp(y^2 + z^2 + 1))
dgx = D(g_expr, 'x')
dgy = D(g_expr, 'y')
dgz = D(g_expr, 'z')


g <- function(x) {(x[1] + x[3]^2) * exp(x[2]^2 + x[3]^2 + 1) }
minimum_g = optim(c(0,0,0), g)$par
maximum_g = optim(c(0,0,0), g,control=list(fnscale=-1))$par

#3
# h(x, y, z) = ln(xyz) ??? ln(x) *ln(y)*ln(z)
h_expr = expression(log(x*y*z) ??? (log(x) *log(y)*log(z)))
dhx = D(h_expr, 'x')
dhy = D(h_expr, 'y')
dhz = D(h_expr, 'z')

h <- function(x) {log(x[1]*x[2]*x[3]) ??? (log(x[1]) *log(x[2])*log(x[3]))}
minimum_h = optim(c(0.1,0.1,0.1), h)



