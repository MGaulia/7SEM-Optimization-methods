library(lpSolve)

# 1

# Task
# Maximize x1 + 9x2 + 3x3 + x4 + 6x5 + 7x6 + 5x7

# Constraints
# 6x1 + 2x2 − 15x3 + 12x4 − 4x5 + 16x6 − 6x7 <= 160
# −x1 + 8x2 + 7x3 + x4 + 8x5 − x6 + x7 <= 100
# 4x1 − 6x2 + 5x3 − 3x4 + 5x5 + 5x6 + 2x7 <= 110
# 3x1 + 5x2 + 2x3 − 3x4 + x5 + 2x6 + x7 <= 157
# 6x1 + 4x2 − 5x3 + 3x4 + 5x5 + 3x6 + 5x7 <= 166
# −4x1 + 5x2 − 4x3 + 5x4 + 4x5 + 5x6 + 4x7 <= 140
# x1, x2, x4, x5, x6, sveiki, x3, x7 realūs

one.in <- c(1, 9, 3, 1, 6, 7, 5)
one.mat <- matrix(
    c(
        6, 2, -15, 12, -4, 16, -6,
        -1, 8, 7, 1, 8, -1, 1,
        4, -6, 5, -3, 5, 5, 2,
        3, 5, 2, -3, 1, 2, 1,
        6, 4, -5, 3, 5, 3, 5,
        -4, 5, -4, 5, 4, 5, 4
    ), 
    nrow = 6, byrow = TRUE
    )

one.dir <- rep("<=", NROW(one.mat))
one.rhs <- c(160, 100, 110, 157, 166, 140)

oneresult <- lp("max", one.in, one.mat, one.dir, one.rhs, int.vec = c(1,2,4,5,6))

# Results

oneresult
# Max is 285.8 

oneresult$solution
# x1 = 6
# x2 = 9
# x3 = 5.1
# x4 = 0
# x5 = 0
# x6 = 16
# x7 = 14.3




# 2 

# Uzduotis
# Maximize revenue 

# Constraints, x,y,z means number of products
# 4x + 6y + 8z <= 50
# 10x + 15y + 25z <= 140
# 6x + 10y + 14z <= 80
# x, y, z sveiki
# Revenue = 400x + 600y + 900z

two.in <- c(400, 600, 900)
two.mat <- matrix(
  c(
    4, 6, 8, 
    10, 15, 25,
    6, 10, 14
  ),
  nrow = 3, byrow = TRUE
  )

two.dir <- rep("<=",NCOL(two.mat))
two.rhs <- c(50, 140, 80)

tworesult <- lp("max", two.in, two.mat, two.dir, two.rhs,all.int = TRUE)

# Result

tworesult
# Revenue 5200

tworesult$solution
# 4 products with price 400
# 0 products with price  600
# 4 products with price  900




# 3 

# Uzduotis
# Assign workers to departments

three.in <- c(
  600, 600, 600, 500, 800, 600,
  700, 600, 700, 600, 800, 1000,
  600, 800, 400, 900, 1000, 900,
  500, 400, 500, 400, 500, 400,
  600, 700, 800, 700, 600, 600,
  700, 800, 600, 700, 600, 500
)

three.mat <- numeric()
print("Pirma lenteles puse")
for (i in 0:5){
  three.mat <- rbind(three.mat,c(rep(0, i*6), rep(1, 6), rep(0, (5-i)*6)))
  print(c(rep(0, i*6), rep(1, 6), rep(0, (5-i)*6)))
}
print("Antra lenteles puse")
for (i in 0:5){
  three.mat <- rbind(three.mat,head(c(rep(0, i), rep(c(1, 0, 0, 0, 0, 0), 6)), 36))
  print(head(c(rep(0, i), rep(c(1, 0, 0, 0, 0, 0), 6)), 36))

}

three.dir <- rep("=" ,6)
three.rhs <- rep(1, 6)

threeresult <- lp("min", three.in, three.mat, three.dir, three.rhs,all.int = TRUE)

# Result

threeresult
# Total cost 3100

lapply(split(threeresult$solution, cut(seq_along(threeresult$solution), 6, labels = FALSE)), which.max)
# Best assignment
# D1 -> A4, price 500
# D2 -> A2, price 600
# D3 -> A3, price 400
# D4 -> A1, price 500
# D5 -> A5, price 600
# D6 -> A6, price 500
