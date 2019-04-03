
#import mbti pop avgs 

mbtiavg <- read.csv("mbtipopavg.csv", stringsAsFactors = FALSE)

mbtiavg$functionpair <- substr(mbtiavg$mbti,2,3)

# convert to two data frames - one of individual letter avgs then one of interior
singleletters <- c("E","N","T","J")

singleletters <- as.data.frame(singleletters)

colnames(singleletters) <- c("letter")

for (i in 1:nrow(singleletters)){
  singleletters$popavg[i] <- sum(subset(mbtiavg$popavg,substr(mbtiavg$mbti,i,i)==singleletters$letter[i]))
  singleletters$popavgm[i] <- sum(subset(mbtiavg$popavgm,substr(mbtiavg$mbti,i,i)==singleletters$letter[i]))
  singleletters$popavgf[i] <- sum(subset(mbtiavg$popavgf,substr(mbtiavg$mbti,i,i)==singleletters$letter[i]))
  singleletters$dataavg[i] <- nrow(subset(data,substr(data$mbti,i,i)==singleletters$letter[i]))/nrow(data)
  
}

functionpairs <- mbtiavg$mbti %>% substr(2,3) %>% unique() %>% as.data.frame() 
colnames(functionpairs) <- c("functionpair")

for (i in 1:nrow(functionpairs)){
  functionpairs$popavg[i] <- sum(subset(mbtiavg$popavg,mbtiavg$functionpair==functionpairs$functionpair[i]))
  functionpairs$popavgm[i] <- sum(subset(mbtiavg$popavgm,mbtiavg$functionpair==functionpairs$functionpair[i]))
  functionpairs$popavgf[i] <- sum(subset(mbtiavg$popavgf,mbtiavg$functionpair==functionpairs$functionpair[i]))
  functionpairs$dataavg[i] <-nrow(subset(data,data$mbtiinterior==functionpairs$functionpair[i]))/nrow(data)
  
}
