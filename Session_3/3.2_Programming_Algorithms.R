# 3.2 Programming Algorithms
# This section concerns translating pseudo-code and written algorithms into
# code. This approach to programming is particulary helpful because all it 
# requires is a clear step-by-step understanding of the procedure and 
# implementation easily follows.


#----------------------------------------------------------------------------
# Newton-Raphson optimisation algorithm: 
# Reference: https://www.cup.uni-muenchen.de/ch/compchem/geom/nr.html
# Univariate case:

# An example: Find the maximum of 
# f(x) = 3x^3 - 10x^2 - 56x + 5 over the interval (-4,6)
f = function(x) 3*x^3 - 10*x^2 -56*x +5
curve(f,-4,6)

# first and second derivatives (f prime and f prime prime):
fp = function(x) 9*x^2 -20*x - 56
fpp = function(x) 18*x - 20

# code the algorithm:
x1=x0=k=0
tol=1e-07
dev = 10
while(abs(dev)>tol){
  k = k+1
  x0=x1
  x1 = x0 - (fp(x0)/fpp(x0))
  dev = x1-x0
}
#
x1-x0 # difference at covergence
# Let us print out the results
cat("The maximum is ", x0,"at f(x) = ", f(x0), "after k = ",k," iterations.")
points(x0,f(x0)) # mark the maximum

# Can you modify the above code to find the minimum
#----------------------------------------------------------------------------










