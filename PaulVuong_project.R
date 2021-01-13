auto <- read.csv("~/Documents/sta2260/Project/UCI_Data_Sets/automobile/imports-85.csv")
dim(auto)
head(auto)
plot(auto)
plot(auto[,c(13,16,22,23,24,25)])

# Model with one-predictor
m1.curbweight = lm(price ~ curbweight, data=auto)
m1.enginesize = lm(price ~ enginesize, data=auto)
m1.citympg = lm(price ~ citympg, data=auto)
m1.hwmpg = lm(price ~ highwaympg, data=auto)

plot(auto$curbweight, auto$price)
abline(m1.curbweight)

plot(auto$enginesize, auto$price)
abline(m1.enginesize)

plot(auto$citympg, auto$price)
abline(m1.citympg)

plot(auto$highwaympg, auto$price)
abline(m1.hwmpg)

summary(m1.curbweight)
summary(m1.enginesize)
summary(m1.citympg)
summary(m1.hwmpg)

AIC(m1.curbweight, m1.enginesize, m1.citympg, m1.hwmpg)
# m1.enginesize had the lowest AIC and highest adj. R^2, so it is the best.

# Quaratic
m1.enginesize.sq = lm(price ~ enginesize + I(enginesize^2), data=auto)
# Inverse
m1.enginesize.inv = lm(price ~ enginesize + I(1/enginesize), data=auto)

summary(m1.enginesize.sq)
summary(m1.enginesize.inv)

AIC(m1.curbweight, m1.enginesize, m1.citympg, m1.hwmpg, m1.enginesize.sq, m1.enginesize.inv)

# Plot the model for more diagnostics
plot(m1.enginesize)
plot(m1.enginesize.sq)
plot(m1.enginesize.inv)

# Plot m1.enginesize.sq
plot(auto$enginesize, auto$price)
abline(m1.enginesize, col="red")
x <- auto$enginesize
xmesh <- seq(0.5*min(x), 2*max(x), by=0.1) # getting lots of x values to draw the line through
yhat <- predict(m1.enginesize.sq, 
                newdata=data.frame(enginesize=xmesh))
lines(xmesh, yhat, col="blue")
legend("topleft", # where to draw the legend
       c("Linear", "Quadratic"), # line names
       lty=c(1,1),  # just leave this as 1 for each line you're drawing)
       lwd=c(2,2), # line thicness
       col=c("red", "blue") # colors of lines
)
# so enginesize.sq is our choice for m1
m1 <- m1.enginesize.sq

# Multiple linear regression
# engine-size and curb-weight
m2.engine.curb = lm(price ~ enginesize + curbweight, data=auto)
# engine-size and curb-weight square
m2.engine.curb.sq = lm(price ~ enginesize + I(enginesize^2) + curbweight + I(curbweight^2), data=auto)
# engine-size and curb-weight inverse
m2.engine.curb.inv = lm(price ~ enginesize + I(1/enginesize) + curbweight + I(1/curbweight), data=auto)
# engine-size, curb-weight, city-mpg, highway-mpg
m2.engine.curb.city.hwy = lm(price ~ enginesize + curbweight + citympg + highwaympg, data=auto)

summary(m2.engine.curb)
summary(m2.engine.curb.sq)
summary(m2.engine.curb.inv)
summary(m2.engine.curb.city.hwy)

AIC(m1.enginesize, m1.enginesize.sq, m2.engine.curb, m2.engine.curb.sq, m2.engine.curb.inv, m2.engine.curb.city.hwy)

# so m2.engine.curb.inv is the best choice
m2 <- m2.engine.curb.inv

# Compare m1 and m2
summary(m1) 
summary(m2)
AIC(m1, m2)

