#Bar Plot
rev = c(7,12,28,3,41)
Mon = c("Mar","Apr","May","Jun","Jul")

barplot(rev,names.arg=Mon,xlab="Month",ylab="Revenue",col="blue",
        main="Bar Chart for Revenue",border="red")

#Stacked Bar chart
regions <- c("East","West","north")

values <- matrix(c(2,9,3,11,9,4,8,7,3,12,5,2,8,10,11),
                 nrow=3,ncol=5,byrow = T)
# Create the plot
barplot(values,names.arg=Mon,main="Revenue of all regions")

#create the plot with colours
colors <- c("green","orange","brown")
barplot(values,names.arg=Mon,main="Revenue of all regions",
        xlab = "Month",ylab="Revenue",col=colors)

#Add the legend
legend("topleft",regions, cex=1.3,fill=colors)

# Dot Plot
month <- month.name
month
expected <- c(15,16,20,31,11,6,17,22,32,12,19,20)
sold <- c(8,18,12,10,41,2,19,26,14,16,9,13)
quarter <- c(rep(1,3),rep(2,3),rep(3,3),rep(4,3))

df1 <- data.frame(month,expected,sold,quarter)

#create a dot plot
dotchart(df1$sold, pch=21, labels=df1$month, bg="green",
         pt.cex=1.5, xlim=range(df1$expected, df1$sold)+c(-2,2))
points(df1$expected, 1:nrow(df1), col="red",pch=19, cex=1.5)

#Scatter Plot
data('mtcars')
plot(mtcars$wt,mtcars$mpg)
plot(mtcars$wt,mtcars$mpg, main="weight vs mileage",
     xlab="Weight", ylab="Mileage", pch=19, col="Red")

x=mtcars$wt
y=mtcars$mpg
abline(lm(y~x,data=mtcars),col="blue")

#scatter Plot Matrix
data('iris')
head(iris)
pairs(iris[,1:4],pch=21, lower.panel=NULL)


vec <- c(9,13,21,8,36,22,12,41,31,33,19)
hist(vec, xlab='x values', col='green', border='blue')

