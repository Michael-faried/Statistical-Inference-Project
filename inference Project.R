install.packages("odbc")
library("odbc")

# Task 1 (done) 
f1 <- function(value, vector)
{
  for(i in 1:length(vector))
  {
    if(vector[i]==value)
    {
      return(i)
    }
  }
  
}


# Task 2 (done) 
data(rock)
rock$shape[rock$area<11111&rock$perm==82.4&rock$peri>4000]


# Task 3 (done)
den1<- density(rock$shape*30000)
den2<- density(rock$area)
plot(den1,main="Area and Shape Density Functions",col="red",lwd=2)
lines(den2,col="green",lwd=2)

legend("topright", c("Area", "Shape"),
       col =c("green","red"), lty=1)



# Task 4 (done)
plot(rock$perm,rock$area, main="area of the rock relative to its 
perm", xlab="The Perm of the rock", ylab="Area of the rock",pch=3)


# Task 5 (done)
install.packages("dplyr")
library(dplyr)

data_rock <- rock %>%
  filter(shape > 0.3) %>%
  group_by(perm) %>%
  summarize(shape_mean = mean(shape))
View(data_rock)


# Task 6 (done)

#first way 
L <- lm (area ~ perm+peri+shape , data = rock)
pred_area <- data.frame(shape=0.5,perm=100,peri=5000)
predict(L,newdata = pred_area)

#second way
pre <- lm(rock$area ~ rock$peri + rock$shape + rock$perm)
pre
newvol <- pre$coefficients[1] + pre$coefficients[2]*5000 + pre$coefficients[3]*0.5 + pre$coefficients[4]*100
newvol


#Bonus (Done)
install.packages("e1071")
library(e1071)

new_data <- rbind(rock[1:2,],rock[5:6,],rock[9:10,],rock[13:14,])
new_data <- rbind(new_data,rock[17:18,],rock[21:22,],rock[25:26,])
new_data <- rbind(new_data,rock[29:30,],rock[33:34,],rock[37:40,])
new_data <- rbind(new_data,rock[41:42,],rock[45:46,])

new_test <- rbind(rock[3:4,],rock[7:8,],rock[11:12,],rock[15:16,])
new_test <- rbind(new_test,rock[19:20,],rock[23:24,],rock[27:28,])
new_test <- rbind(new_test,rock[31:32,],rock[35:36,],rock[37:40,])
new_test <- rbind(new_test,rock[43:44,],rock[47:48,])


class_perm <- naiveBayes(perm~area+shape, data = new_data)
permpredict <- predict(class_perm ,newdata = new_test)
permpredict

table(permpredict)
