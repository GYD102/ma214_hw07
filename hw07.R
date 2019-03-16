# MA214 -- Problem Set 7

# 1)
data <- read.csv("BOILERStxt.csv")
attach(data)

model <- lm(ManHours ~ Capacity + Pressure + Boiler + Drum)
summary(model)
predict(model,
        newdata = data.frame(
          Capacity = 150000,
          Pressure = 500,
          Boiler = 1,
          Drum = 0),
        interval = "confidence",
        level = 0.95)

anova(model)
detach(data)

# 2)
ggplot() + 
  geom_abline(slope = 0) + 
  scale_y_continuous(limits = c(-3,3)) + 
  scale_x_continuous(limits = c(0,5)) + 
  geom_abline(slope = 0.269, intercept = 0.044, color = "red") + 
  geom_abline(slope = -0.673, intercept = 0.308, color = "blue")

#3) 9.126
data <- read.csv("CRASHtxt.csv")
attach(data)

DIF <- DRIVCHST - PASSCHST
setNames(c(length(DIF),mean(DIF),sd(DIF)),
         c("n","xbar.diff","sd.diff"))
qnorm(0.995)

detach(data)


# 6) 11.26
data <- read.csv("ANTStxt(1).csv")
attach(data)
model <- lm(Rain ~ Temp)
model

library(ggplot2)
ggplot(data = data, aes(x = Temp, y = Rain)) +
  geom_point() +
  geom_abline(slope = model$coefficients[2],
              intercept = model$coefficients[1],
              color = "red")

model <- lm(AntSpecies ~ Rain)
model

ggplot(data = data, aes(x = Rain, y = AntSpecies)) +
  geom_point() +
  geom_abline(slope = model$coefficients[2],
              intercept = model$coefficients[1],
              color = "red")


detach(data)