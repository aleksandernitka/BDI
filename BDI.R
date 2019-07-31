bdi = read.csv(file = 'BDI.csv')

# fun to fix date
ts = function(date){
    date = as.character(date)
    date = strsplit(date, ' ')[[1]][1]
    return(date)
}

# fun to fix responses
scores = function(val){
    val = as.character(val)
    val = strsplit(val, ' ')[[1]][1]
    val = as.integer(val)
    return(val)
}

bdi$Timestamp = lapply(bdi[,1], ts)
bdi[,2:23] = apply(bdi[,2:23], c(1,2), scores)
bdi = as.data.frame(bdi[,1:22])
bdi$Week = c(1:nrow(bdi))

bdi$Total = apply(bdi[,2:22], c(1), sum)

#0–9: indicates minimal depression
#10–18: indicates mild depression
#19–29: indicates moderate depression
#30–63: indicates severe depression.

bdi$Class = NA
bdi$Class[bdi$Total < 10] = 'minimal'
bdi$Class[bdi$Total >= 10 & bdi$Total < 19] = 'mild'
bdi$Class[bdi$Total >= 20 & bdi$Total < 30] = 'moderate'
bdi$Class[bdi$Total >= 30] = 'severe'

table(bdi$Class)

library(ggplot2)
ggplot(data = bdi, aes(y = Total, x = Week)) + geom_line() + geom_point()
