# Scratchpad for interactive testing before integration in a script

x <- 52
p <- 0.0064

p2x <- \(p) -1 / log2(1 - p)
x2p <- \(x) 1 - 2^(-1 / x)

p2x(p)
x2p(52)
