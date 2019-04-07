install.packages("e1071")
library(e1071)

# 2 Vectors__________________

# SVM Implementation using R Studio
flower_data <- iris[1:100,c(1,2,5)]
levels(flower_data[,3])

colo <- c(rep(-1,50),rep(+1,50)) 

my.data <- data.frame(Length = flower_data[,1],
                      Width = flower_data[,2],
                      flower = as.factor(flower_data[,3]))

#Plot the data
plot(my.data[,-3],pch=19);abline(h = 0,v = 0,lty=3)

svm.model <- svm(flower ~ .,data = my.data,type='C-classification',kernel='linear',scale=FALSE)
summary(svm.model)

points(my.data[svm.model$index,c(1,2)],col="blue",cex=2)

# get parameters of the hyperplane

w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho

# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*2 + b = 0


abline(a = -b/w[1,2],b = -w[1,1]/w[1,2], col = "blue",lty = 3)


# new data set flower_data "setosa,versicolor,setosa" "5.2 , 3.1 ; 6.5 , 3.3 ; 4.5 , 2.5"

observation <- data.frame(Length = c(5.2,6.5,4.5),Width = c(3.1,3.3,2.5))

#plot new data

plot(my.data[,-3],col=(colo+3),pch=19);abline(h = 0,v = 0,lty=3)
points(observation[1,],col="green",pch=19)
points(observation[2,],col="orange",pch=19)
points(observation[3,],col="forestgreen",pch=19)
abline(a = -b/w[1,2],b = -w[1,1]/w[1,2], col = "blue",lty = 3)

# Verify the results
predict(svm.model,observation)

