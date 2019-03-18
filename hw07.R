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

# 11.112)
data <- data.frame(
  cor = c(945,950,930,920,920,930,905,915,910,900,
          885,870,850,795,790,800,820,810,780)/1000,
  speed = c(8,10,15,18,30,34,44,50,64,44,53,54,74,
            72,72,80,85,94,90)/10,
  buckle = c(rep(0,9), rep(1,10))
)
data
# a)
lm(data$cor ~ data$speed)

# b)
library(dplyr)
no.buck <- filter(data, buckle == 0)
buck <- filter(data, buckle == 1)
no.buck; buck
lm(no.buck$cor ~ no.buck$speed); lm(buck$cor ~ buck$speed)
summary(lm(data$cor ~ data$speed + data$speed:data$buckle))

# 9.150)
data <- read.csv("MStxt.csv")
data <- cbind(data,
      AgeDiff = data$MS.AGE - data$NON.AGE,
      HDiff = data$MS.HEIGHT - data$NON.HEIGHT,
      WDiff = data$MS.WEIGHT - data$NON.WEIGHT
)
data
# sapply(data, mean)
# sapply(data, var)
# sapply(data, sd)
t.test(data$AgeDiff, conf.level = .9)
t.test(data$HDiff, conf.level = .9)
t.test(data$WDiff, conf.level = .9)

# Above is adequate for Txtbook Question
# Below is more thorough analysis

var.test(data$MS.AGE, data$NON.AGE)
var.test(data$MS.HEIGHT, data$NON.HEIGHT)
var.test(data$MS.WEIGHT, data$NON.WEIGHT)

# 9.151)
data <- read.csv("SMWTtxt.csv")
data$GENDER <- as.numeric(data$GENDER == "MALE")
data$GROUP <- as.numeric(data$GROUP == "SPILLOV")

data0 <- filter(data, GROUP == 0)
data1 <- filter(data, GROUP == 1)
names(data)
t.test(data0$GENDER, data1$GENDER)
t.test(data0$EDYRS, data1$EDYRS)
t.test(data0$AGE, data1$AGE)
t.test(data0$TASKID, data1$TASKID)
t.test(data0$SKILLS, data1$SKILLS)
t.test(data0$DECPERS, data1$DECPERS)
t.test(data0$INFO, data1$INFO)

# 11.151)
# a)
data <- read.csv("FLOURtxt.csv")
lm(Weight ~ Number - 1, data = data)

# b)
lm(Weight ~ Number, data = data)

# (a,b)
library(ggplot2)
ggplot() +
  geom_abline(slope = 46.4) +
  geom_abline(slope = 45.15, intercept = 478.44) +
  scale_x_continuous(limits = c(0,25)) +
  scale_y_continuous(limits = c(0,1000)) +
  xlab("Number of Bags") + ylab("Total Weight of Shipment")

# d)
summary(lm(Weight ~ Number, data = data))$coefficients
