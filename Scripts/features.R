#libraries
library(ggplot2)
library(corrplot)
library(pvclust)

#features group validation with hclust & pvclust
result <- pvclust(veri, method.dist="correlation", nboot=500)
plot(result)
pvrect(result, alpha=0.90)

#Comparison of the dimensions
#subset questions by dimensions

Z1 <- veri[c(9,24,31,39,41,43,45,50,53,60)]
Z2 <- veri[c(3,12,15,21,23,28,32,35,49,57)]
Z3 <- veri[c(1,8,10,16,17,22,27,34,40,47)]
Z4 <- veri[c(5,7,33,36,38,52,54,55,58,59)]
Z5 <- veri[c(2,4,11,14,18,19,26,30,37,44)]
Z6 <- veri[c(6,13,20,25,29,42,46,48,51,56)]

#Correlation among all the variables 60x60
M <- cor(veri)
corrplot(M, method = "color")
corrplot(M, method = "number")

#compute the p value of the correlations

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(veri)
head(p.mat)

corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.10)

#correlation matrices

Z1 <- veri[c(9,24,31,39,41,43,45,50,53,60)]
Z4 <- veri[c(7,33,36,38,52,54,55,58,59)]

Z12 <- veri[c(9,24,31,39,41,43,45,50,53,60,3,12,15,21,23,28,32,35,49,57)]
Z14 <- veri[c(9,24,31,39,41,43,45,50,53,60,5,7,33,36,38,52,54,55,58,59)]
Z24 <- veri[c(3,12,15,21,23,28,32,35,49,57,5, 7,33,36,38,52,54,55,58,59)]

Z124 <-veri[c(9,24,31,39,41,43,45,50,53,60,3,12,15,21,23,28,32,35,49,57, 5,7,33,36,38,52,54,55,58,59)]


