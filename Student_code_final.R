install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(readxl)
num <- read_xlsx("C:\\Users\\kaina\\Desktop\\Score.xlsx")
num
#Histogram
numeric_columns <- sapply(num, is.numeric)
for (col in names(num)[numeric_columns]) {
  hist(num[[col]], main = paste("Histogram of", col), xlab = col, col = "lightblue", border = "black")
}
#normality
shapiro_test <- shapiro.test(num$Scores)
shapiro_test
#transformation
trans1<- log(num$Scores)
#NORMALITY
shapiro_test1 <- shapiro.test(trans1)
shapiro_test1
qqnorm(trans1)
qqline(trans1,col=2)
plot(trans1 ~ Hours, data = num)
#LINEAR MODEL
model1<- lm(trans1~Hours,data=num)
summary(model1)
plot(model1)
#Test fro homoscedasticity
library(lmtest)
homo<- bptest(model1)
homo
#visualization

graph<-ggplot(num, aes(x=Hours, y=trans1))+
  geom_point()
graph

graph1 <- graph + geom_smooth(method="lm", col="black")

graph1
#Prediction
new_data2 <- data.frame(Hours = 9.25)
predicted_score1 <- predict(model1, newdata = new_data2)
print(predicted_score1)

