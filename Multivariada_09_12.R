y11 <- c(33, 36, 35, 38, 40)
y12 <- c(60, 61, 64, 63, 65)
G1 <- cbind(y11, y12); G1

y21 <- c(35, 36, 38, 39, 41, 43, 41)
y22 <- c(57, 59, 59, 61, 63, 65, 59)
G2 <- cbind(y21, y22); G2

ybar1 = apply(G1,2,mean); ybar1
ybar2 = apply(G2,2,mean); ybar2

plot(G1, xlim = c(32,43), ylim = c(56,66), lwd = 2)
points(G2, col="blue", lwd = 2, pch = 2)

n1 = length(y11);n1
n2 = length(y22);n2

S1 = var(G1)
S2 = var(G2)
Sc = ( (n1 - 1)*S1 + (n2 - 1)*S2)/(n2 + n1 - 2);Sc

a = solve(Sc)%*%(ybar1 - ybar2);a

#Grupo 1
G1_z = G1 %*% a; G1_z

#Grupo2
G2_z = G2 %*% a; G2_z

pm = t(a) %*% (ybar1 + ybar2)/2;pm
pm/a[2]; -(a[1]/a[2]) # obtido por a_1 + y_1 + a_2 + y_2 = pm

abline(pm/a[2], -(a[1]/a[2]))

Bruno = c(40, 62)
Leticia = c(37,61)

points(t(Leticia), col = "blue", pch = 3, lwd = 2)
points(t(Bruno), col = "blue", pch = 3, lwd = 2)

## ----------------------------Atividade prática----------------------------- ##

x1 = c(2,5,4,4,3,3,3,4,4,4,4,5,4,4,4,3,4,5,5,4,4,4,3,5,5,3,4,3,4,4,4,4,4,4,4,3,
       4,3,4,3,4,5,4,4,4,3,5,4,3,5,4,5,4,2,3,4,4,4,3,4)
x2 = c(3,5,5,3,3,3,4,4,5,4,4,5,4,3,4,3,5,5,5,4,4,4,4,3,5,3,4,3,4,4,4,5,4,5,4,3,
       3,4,4,4,5,5,4,4,4,4,5,5,4,3,4,3,5,5,4,3,4,4,4,4)
x3 = c(5,4,5,4,5,4,4,5,5,3,5,4,4,5,5,4,4,5,4,4,4,4,5,5,3,4,4,5,3,5,5,5,5,5,5,4,
       5,5,5,4,5,5,5,4,5,4,5,4,4,4,5,4,4,5,5,5,4,5,4,5)
x4 = c(5,4,5,4,5,5,4,5,5,3,5,4,4,5,5,5,4,5,4,4,4,4,5,5,3,4,4,5,3,5,5,5,5,5,5,4,
       4,5,4,4,5,5,5,4,5,4,5,4,4,4,4,4,5,5,5,4,5,4,4,5)
conjunge = c("Husband","Husband","Husband","Husband","Husband","Husband","Husband","Husband",
             "Husband","Husband","Husband","Husband","Husband","Husband","Husband","Husband",
             "Husband","Husband","Husband","Husband","Husband","Husband","Husband","Husband",
             "Husband","Husband","Husband","Husband","Husband","Husband","Wife","Wife","Wife",
             "Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife",
             "Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife","Wife")
dados = data.frame(conjunge, x1, x2, x3, x4)
# Pacote
require(ggplot2)
require(profileR)