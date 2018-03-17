library(ggplot2)
library(dplyr)

##Setting function that predicts the number of Foxes and Rabbits at time t+1

#Alpha is the Birth rate of Rabbits
#Beta is the Consumption rate of foxes eating rabbits
#Lambda is the death rate of foxes


alpha <- 0.05
beta <- 0.0001
lambda <- 0.02

##Initializing number of rabbits and foxes
rabbits <- c()
foxes <- c()

rabbits[1] <- 30
foxes[1] <- 40

##Writing for loop which projects the number of foxes and rabbits for a 104 week period (each time period between the equations is 1 week)

for (i in 1:103) {
  rabbits[i+1] <- rabbits[i] + alpha*rabbits[i] - beta*rabbits[i]*foxes[i]
  foxes[i+1] <- foxes[i] + beta*rabbits[i]*foxes[i] - lambda*foxes[i]
}

print("The number of rabbits in the deterministic model:")
print(rabbits)
print("The number of foxes in the deterministic model:")
print(foxes)

##setting seed to 60854

set.seed(60854)

sto_rabbits <- c()
sto_foxes <- c()
sto_rabbits[1] <- 30
sto_foxes[1] <- 40
##Implementing stochastic version of model
for (i in 1:103) {
  nrabbitseaten <- rbinom(1, sto_rabbits[i] * sto_foxes[i], beta)
  nrabbitsborn <- rbinom(1, sto_rabbits[i], alpha)
  nfoxesdeath <- rbinom(1, sto_foxes[i], lambda)
  sto_rabbits[i+1] <- sto_rabbits[i] + nrabbitsborn - nrabbitseaten
  sto_foxes[i+1] <- sto_foxes[i] + nrabbitseaten - nfoxesdeath
}


print("This is the number of rabbits in the stochastic model:")
print(sto_rabbits)
print("This is the number of foxes in the stochastic model:")
print(sto_foxes)


##Creating a Long Data Frame with three variables: time, group, and size

LV <- data.frame(
  time = c(rep(1:104,4)),
  group = c(rep("rabbits", 104), rep("foxes", 104), rep("sto_rabbits",104), rep("sto_foxes",104)),
  size = c(rabbits, foxes, sto_rabbits, sto_foxes)
)

##Plotting the Data Frame with lines to see the differences between the four groups

foxes_rabbits <- ggplot(LV) + geom_line(aes(x=time, y=size, colour=group, linetype=group)) + labs(x="Time (in weeks)", y="Size of Population (By week)") + ggtitle("Answer to Number 3: Lotka-Volterra Model")

##Saving the plot as foxes_robbits_31781792. Answer to number 3
ggsave(plot = foxes_rabbits,filename = "foxes_rabbits_31781792.png", width = 8.5, height = 7)
print("Saved the plot as foxes_rabbits_31781792.png to working directory")