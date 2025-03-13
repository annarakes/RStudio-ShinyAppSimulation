library(data.table)
library(ggplot2)


normalDistribution <- data.frame(
  x = seq(-4,4, by = 0.01),
  y = dnorm(seq(-4,4, by = 0.01))
)




criticalValues <- qnorm(c(.025,.975))

shadeNormalTwoTailedLeft <- rbind(c(criticalValues[1],0), subset(normalDistribution, x < criticalValues[1]))

shadeNormalTwoTailedRight <- rbind(c(criticalValues[2],0), subset(normalDistribution, x > criticalValues[2]), c(3,0))



ggplot(normalDistribution, aes(x,y)) +
  geom_line() +
  geom_polygon(data = shadeNormalTwoTailedLeft, aes(x=x, y=y, fill="red")) +
  geom_polygon(data = shadeNormalTwoTailedRight, aes(x=x, y=y, fill="red")) +
  guides(fill="none") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = criticalValues[1], y = 0, xend = criticalValues[1], yend = dnorm(criticalValues[1]))) +
  geom_segment(aes(x = criticalValues[2], y = 0, xend = criticalValues[2], yend = dnorm(criticalValues[2]))) 

pvalue <- 2*pnorm(criticalValues[1])


criticalValue <- qnorm(.95)

shadeNormal <- rbind(c(criticalValue,0), subset(normalDistribution, x > criticalValue), c(3,0))

ggplot(normalDistribution, aes(x,y)) +
  geom_line() +
  geom_polygon(data = shadeNormal, aes(x=x, y=y, fill="red")) +
  guides(fill="none") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = criticalValue, y = 0, xend = criticalValue, yend = dnorm(criticalValue))) 

pvalue <- pnorm(criticalValue, lower.tail=FALSE)

criticalValue <- qnorm(.15)

shadeNormal <- rbind(c(criticalValue,0), subset(normalDistribution, x < criticalValue))

ggplot(normalDistribution, aes(x,y)) +
  geom_polygon(data = shadeNormal, aes(x=x, y=y, fill="red")) +
  geom_line() +
  guides(fill="none") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = criticalValue, y = 0, xend = criticalValue, yend = dnorm(criticalValue))) 

pvalue <- pnorm(criticalValue)

