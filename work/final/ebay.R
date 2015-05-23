eBay = read.csv("ebay.csv", stringsAsFactors=FALSE)
eBay$sold = as.factor(eBay$sold)
eBay$condition = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color = as.factor(eBay$color)
eBay$material = as.factor(eBay$material)

set.seed(144)
library(caTools)
spl = sample.split(eBay$sold, 0.7)
training = subset(eBay, spl == TRUE)
testing = subset(eBay, spl == FALSE)

model = glm(sold ~ biddable + startprice + condition + heel + style +
              color + material, data = training, family = binomial)