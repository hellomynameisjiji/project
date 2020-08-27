######################################################################################
### This is a sample of my works to show you how I've worked on processing dataset ###
######################################################################################

mass<-multi[multi$title=="mass",]
game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Mass Effect 3",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-03-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-04-10") - as.Date("2012-03-06")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(34-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mass$combi, mass$weekday, mass$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mass, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mass<-M3

##mass3<-multi[multi$title=="mass3",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Mass Effect 3",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-03-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-06-17") - as.Date("2012-03-06")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(102-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mass3$combi, mass3$weekday, mass3$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass3$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass3$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass3$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mass3, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mass3<-M3

##
mass4<-multi[multi$title=="mass4",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Mass Effect 3",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-03-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-10-09") - as.Date("2012-03-06")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(216-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mass4$combi, mass4$weekday, mass4$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass4$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass4$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass4$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mass4, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mass4<-M3


##
mass5<-multi[multi$title=="mass5",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Mass Effect 3",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-03-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2013-02-26") - as.Date("2012-03-06")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(356-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mass5$combi, mass5$weekday, mass5$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass5$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass5$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mass5$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mass5, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mass5<-M3

##
mine<-multi[multi$title=="mine",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Minecraft",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-05-09"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-07-13") - as.Date("2012-05-09")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(64-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mine$combi, mine$weekday, mine$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mine, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mine<-M3

##
mine2<-multi[multi$title=="mine2",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Minecraft",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-05-09"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-10-16") - as.Date("2012-05-09")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(159-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mine2$combi, mine2$weekday, mine2$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine2$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine2$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine2$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mine2, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mine2<-M3

##
mine3<-multi[multi$title=="mine3",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Minecraft",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-05-09"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-10-16") - as.Date("2012-05-09")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(223-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mine3$combi, mine3$weekday, mine3$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine3$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine3$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine3$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mine3, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mine3<-M3

##
mine4<-multi[multi$title=="mine4",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Minecraft",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-05-09"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-10-16") - as.Date("2012-05-09")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(329-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mine4$combi, mine4$weekday, mine4$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine4$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine4$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine4$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mine4, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mine4<-M3

##
mine5<-multi[multi$title=="mine5",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Minecraft",]
head(data1)
#table(data1$type)
#data1$date2<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$date <- NULL
#data1$Start.URL <- NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(2:3, 5)] <- c("ID", "title","date")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-05-09"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-10-16") - as.Date("2012-05-09")		# tt=1 - release 날짜 = XX

for (i in 1:120) {
  data1$tt[data1$game_time==(470-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
head(data2)
range(data1$tt)

MM <- data.frame(mine5$combi, mine5$weekday, mine5$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)
for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine5$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine5$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%mine5$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}


MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(mine5, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

mine5<-M3

##

battle<-multi[multi$title=="battle",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
#data<-read.csv("/Users/hyeongjihyeon/Downloads/battlefield-3.csv")
data1<-data
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$title <- "Battlefield 3"
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-10-25"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2011-12-13") - as.Date("2011-10-25")		# tt=1 - release 날짜 = XX
head(data1)
for (i in 1:120) {
  data1$tt[data1$game_time==(48-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(battle$combi, battle$weekday, battle$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(battle, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

battle<-M3

##
battle2<-multi[multi$title=="battle2",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
#data<-read.csv("/Users/hyeongjihyeon/Downloads/battle2field-3.csv")
data1<-data
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$title <- "Battlefield 3"
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-10-25"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2011-12-13") - as.Date("2011-10-25")		# tt=1 - release 날짜 = XX
head(data1)
for (i in 1:120) {
  data1$tt[data1$game_time==(244-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(battle2$combi, battle2$weekday, battle2$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle2$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle2$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle2$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(battle2, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

battle2<-M3

##
battle3<-multi[multi$title=="battle3",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
#data<-read.csv("/Users/hyeongjihyeon/Downloads/battle3field-3.csv")
data1<-data
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$title <- "Battlefield 3"
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-10-25"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2011-12-13") - as.Date("2011-10-25")		# tt=1 - release 날짜 = XX
head(data1)
for (i in 1:120) {
  data1$tt[data1$game_time==(321-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(battle3$combi, battle3$weekday, battle3$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle3$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle3$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle3$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(battle3, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

battle3<-M3

##
battle4<-multi[multi$title=="battle4",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
#data<-read.csv("/Users/hyeongjihyeon/Downloads/battle4field-3.csv")
data1<-data
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$title <- "Battlefield 3"
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-10-25"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2011-12-13") - as.Date("2011-10-25")		# tt=1 - release 날짜 = XX
head(data1)
for (i in 1:120) {
  data1$tt[data1$game_time==(405-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(battle4$combi, battle4$weekday, battle4$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle4$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle4$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle4$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(battle4, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

battle4<-M3

##
battle5<-multi[multi$title=="battle5",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
#data<-read.csv("/Users/hyeongjihyeon/Downloads/battle5field-3.csv")
data1<-data
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$title <- "Battlefield 3"
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-10-25"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2011-12-13") - as.Date("2011-10-25")		# tt=1 - release 날짜 = XX
head(data1)
for (i in 1:120) {
  data1$tt[data1$game_time==(503-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(battle5$combi, battle5$weekday, battle5$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle5$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle5$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%battle5$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(battle5, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

battle5<-M3


##bullet<-multi[multi$title=="bullet",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data<-read.csv("/Users/hyeongjihyeon/Downloads/bulletstorm.csv")
data1<-data
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-02-22"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2011-04-14") - as.Date("2011-02-22")		# tt=1 - release 날짜 = XX
head(data1)
for (i in 1:120) {
  data1$tt[data1$game_time==(50-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(bullet$combi, bullet$weekday, bullet$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%bullet$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%bullet$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%bullet$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(bullet, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

bullet<-M3

##
dead<-multi[multi$title=="dead",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
#data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data<-read.csv("/Users/hyeongjihyeon/Downloads/dead-island.csv")
data1<-data
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-09-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2013-04-23") - as.Date("2011-09-26")		# tt=1 - release 날짜 = XX
head(data1)
for (i in 1:120) {
  data1$tt[data1$game_time==(574-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(dead$combi, dead$weekday, dead$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%dead$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%dead$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%dead$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(dead, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

dead<-M3

##
halo<-multi[multi$title=="halo1",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Halo 4",]
head(data1)
#table(data1$type)
#data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$Start.URL <- NULL
#data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-11-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2013-03-06") - as.Date("2012-11-06")		# tt=1 - release 날짜 = XX
head(data1)
head(halo)
for (i in 1:120) {
  data1$tt[data1$game_time==(119-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(halo$combi, halo$weekday, halo$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%halo$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%halo$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%halo$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(halo, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

halo<-M3
##
halo3<-multi[multi$title=="halo3",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Halo 4",]
head(data1)
#table(data1$type)
#data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$Start.URL <- NULL
#data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-11-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2013-08-20") - as.Date("2012-11-06")		# tt=1 - release 날짜 = XX
head(data1)
head(halo3)
for (i in 1:120) {
  data1$tt[data1$game_time==(286-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(halo3$combi, halo3$weekday, halo3$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%halo3$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%halo3$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%halo3$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(halo3, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

halo3<-M3
##
gotham<-multi[multi$title=="gotham",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Gotham City Imposters",]
head(data1)
#table(data1$type)
#data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$Start.URL <- NULL
#data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-02-08"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-03-09") - as.Date("2012-02-08")		# tt=1 - release 날짜 = XX
head(data1)
head(gotham)
for (i in 1:120) {
  data1$tt[data1$game_time==(29-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(gotham$combi, gotham$weekday, gotham$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%gotham$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%gotham$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%gotham$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(gotham, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

gotham<-M3

##
##
binary<-single[single$title=="binary",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Binary Domain",]
head(data1)
#table(data1$type)
#data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$Start.URL <- NULL
#data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-02-16"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-06-01") - as.Date("2012-02-16")		# tt=1 - release 날짜 = XX
head(data1)
head(binary)
for (i in 1:120) {
  data1$tt[data1$game_time==(105-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(binary$combi, binary$weekday, binary$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%binary$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%binary$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%binary$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(binary, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

binary<-M3


##
##
walking<-single[single$title=="walking",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="The Walking Dead",]
head(data1)
#table(data1$type)
#data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$Start.URL <- NULL
#data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-04-27"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2013-07-05") - as.Date("2012-04-27")		# tt=1 - release 날짜 = XX
head(data1)
head(walking)
for (i in 1:120) {
  data1$tt[data1$game_time==(433-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(walking$combi, walking$weekday, walking$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)
head(data1)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%walking$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%walking$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%walking$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(walking, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

walking<-M3

##
##
batman<-single[single$title=="batman",]
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-read.csv("/Users/hyeongjihyeon/Downloads/batman--arkham-city.csv")
head(data1)
#table(data1$type)
data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
data1$Start.URL <- NULL
data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2011-10-18"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2012-05-29") - as.Date("2011-10-18")		# tt=1 - release 날짜 = XX
head(data1)
head(batman)
for (i in 1:120) {
  data1$tt[data1$game_time==(223-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(batman$combi, batman$weekday, batman$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")
head(MM)

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%batman$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%batman$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%batman$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(batman, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

batman<-M3
##
##
nrow(X)/60
tekken<-X
#game<-read.csv("/Volumes/jetDrive/2018-11-13/gamedata.csv")
data<-game
#data<-data[data$title %in% xb,]
#data$ach<-0
#data$ach <- as.numeric(data$type==0)
#title<- unique(data$title)
data1<-data[data$title=="Street Fighter X Tekken",]
head(data1)
#table(data1$type)
#data1$date<-as.Date(as.character(data1$date), "%d/%b/%y")
#data1$Start.URL <- NULL
#data1$played_hist<-NULL
data1$ach<-0
data1$ach <- as.numeric(data1$type==0)
data1$sum <- 1
#colnames(data1)[c(1:3)] <- c("date","ID", "title")
data1$combi <- paste(data1$ID, data1$title)
data1<-data1[order(data1$date),]
data1<-data1[order(data1$ID),]

data1$game_time <- as.numeric(as.Date(data1$date) - as.Date("2012-03-06"))			# release 날짜
data1$day <- NULL
data1$week <- NULL
data1$month <- NULL


as.Date("2013-06-01") - as.Date("2012-03-06")		# tt=1 - release 날짜 = XX
head(data1)
head(tekken)
for (i in 1:120) {
  data1$tt[data1$game_time==(451-30)+i] <- i-30				# 34 <- XX-1
  print(i)
}

data1 <- data1[is.na(data1$tt)==FALSE,]
data1 <- data1[order(data1$tt),]
data1 <- data1[order(data1$combi),]
data1$weekday<-weekdays(as.Date(data1$date))
data1<-data1[order(data1$tt),]
data1<-data1[order(data1$combi),]

MM <- data.frame(tekken$combi, tekken$weekday, tekken$tt)
colnames(MM)[1:3] <- c("combi", "weekday", "tt")

for(i in 1:60) {
  A <- aggregate(data1$sum[data1$tt<i & data1$tt>(i-28)&data1$combi%in%tekken$combi], by=list(data1$combi[data1$tt<i & data1$tt>(i-28)&data1$combi%in%tekken$combi], data1$weekday[data1$tt<i & data1$tt>(i-28)&data1$combi%in%tekken$combi]), sum)                  
  A[,4] <- i
  colnames(A)[4] <- "tt"
  MM <- merge(MM, A, by.x=c("combi", "weekday", "tt"), by.y=c("Group.1", "Group.2", "tt"), all.x=TRUE, all.y=TRUE)
  print(i)
}

MM<-MM[order(MM$tt),]
MM<-MM[order(MM$combi),]
colnames(MM)[4:63] <- c(1:60)
MM[is.na(MM)==TRUE] <- 0
MM$wsum <- rowSums(MM[,4:63])
MM$wsum[MM$wsum>4]<-4



M3 <- merge(tekken, MM[,c(1:3, 64)], by.x=c("combi","weekday","tt"), by.y=c("combi", "weekday","tt"), all.x=TRUE)
nrow(M3)/60

M3<-M3[order(M3$tt),]
M3<-M3[order(M3$combi),]
M3$wratio<-M3$wsum/4
M3$wratio[is.na(M3$wratio)] <- 0
range(M3$wratio)

tekken<-M3


##
multi<-rbind(mass, mass3)
multi<-rbind(multi, mass4)
multi<-rbind(multi, mass5)
multi<-rbind(multi, mine)
multi<-rbind(multi, mine2)
multi<-rbind(multi, mine3)
multi<-rbind(multi, mine4)
multi<-rbind(multi, mine5)
multi<-rbind(multi, battle)
multi<-rbind(multi, battle2)
multi<-rbind(multi, battle3)
multi<-rbind(multi, battle4)
multi<-rbind(multi, battle5)
multi<-rbind(multi, halo1)
multi<-rbind(multi, halo3)
multi<-rbind(multi, bullet)
multi<-rbind(multi, dead)
multi<-rbind(multi, gotham)

single<-rbind(walking, batman)
single<-rbind(single, binary)
single<-rbind(single, tekken)
tekken$combi2<-paste(tekken$combi,tekken$title)
colnames(tekken)
colnames(single)
tekken$lagged_play<-NULL
tekken$title2<-"tekken"
tekken$users<-NULL
tekken$lag_users<-NULL
str(single)
str(tekken)
summary(tekken)
tekken$first_date.x<-as.factor(tekken$first_date.x)
tekken$first_date.y<-as.factor(tekken$first_date.y)
tekken$lag_date2<-as.factor(tekken$lag_date2)
tekken$lag_date2_2<-as.factor(tekken$lag_date2_2)
tekken$date<-as.factor(tekken$date)
tekken$date3<-as.factor(tekken$date3)
tekken$date2_2<-as.factor(tekken$date2_2)
tekken$date3_2<-as.factor(tekken$date3_2)
head(tekken)
tekken$sum[is.na(tekken$sum)]<-0
tekken$sum[is.na(tekken$ach)]<-0
tekken$date<-as.Date(tekken$date)
tekken$gamett<-NULL
single$X<-NULL
single$playdiff<-NULL
single$playdate<-NULL
single$users<-NULL
single$gamett<-NULL
single$ln_mc_num<-NULL
single$ln_mc_num2<-NULL
colnames(single)
single$rank_rate3<-NULL
single$sq_rank_rate3<-NULL
single$mc_rank_rate3<-NULL
single$mc_sq_rank_rate3<-NULL
single$avg_recency<-NULL
single$avg_freq30<-NULL
single$max_freq30<-NULL
single$numr<-NULL
single$freq30p<-NULL
single$before<-NULL
single$after<-NULL
single$num<-NULL
single$numr2<-NULL
single$ln_mc_numr2<-NULL
single$max_recency<-NULL
single$num3<-NULL
single$numr3<-NULL
single$ln_mc_numr3<-NULL
single$users2<-NULL
single$lag_users<-NULL
single$lag_users2<-NULL
single$period<-NULL
single$ln_mc_numr<-NULL
single$ln_mc_rank_max<-NULL
single$a<-NULL
single$entropy<-NULL
single$month<-NULL
single$year<-NULL
single$entropy.x<-NULL
single$rankdiff<-NULL
single$rank_rate_diff<-NULL
single$diff<-NULL
single$play_diff_id<-NULL
single$entropy.y<-NULL
single$ln_mc_max_freq30<-NULL
single$ln_mc_max_recency<-NULL
single$ln_mc_avg_freq30<-NULL
single$ln_mc_avg_recency<-NULL
single$ln_mc_entropy<-NULL
single$ln_mc_num3<-NULL
single$num2<-NULL
single$game_time3<-NULL
single$ln_mc_num3<-NULL
single$mc_google<-NULL
single$mc_first_time<-NULL
single$first_time2<-NULL
single$ln_mc_freq_freq<-NULL
single$ln_mc_new_freq<-NULL
single$lagged_play<-NULL
colnames(single)[!colnames(single)%in%colnames(tekken)]
tekken$title <- "tekken"
tekken$title2 <- "tekken"
tekken$combi2<-paste(tekken$combi, tekken$title)
tekken<-tekken[order(tekken$tt),]
tekken<-tekken[order(tekken$combi),]
tekken$lagged_play <- as.vector(rbind(0,matrix(tekken$sum,nrow=60))[1:60,])
single<-single[order(single$tt),]
single<-single[order(single$combi),]
single$lagged_play <- as.vector(rbind(0,matrix(single$sum,nrow=60))[1:60,])



#####
multi$game_time2<-multi$game_time
multi$game_time2[multi$title=="mine2"] <- as.numeric(as.Date(multi$date[multi$title=="mine2"])-as.Date("2012-07-13"))
multi$game_time2[multi$title=="mine3"] <- as.numeric(as.Date(multi$date[multi$title=="mine3"])-as.Date("2012-10-16"))
multi$game_time2[multi$title=="mine4"] <- as.numeric(as.Date(multi$date[multi$title=="mine4"])-as.Date("2012-12-19"))
multi$game_time2[multi$title=="mine5"] <- as.numeric(as.Date(multi$date[multi$title=="mine5"])-as.Date("2013-04-04"))
multi$game_time2[multi$title=="mass3"] <- as.numeric(as.Date(multi$date[multi$title=="mass3"])-as.Date("2012-05-29"))
multi$game_time2[multi$title=="mass4"] <- as.numeric(as.Date(multi$date[multi$title=="mass4"])-as.Date("2012-06-17"))
multi$game_time2[multi$title=="mass5"] <- as.numeric(as.Date(multi$date[multi$title=="mass5"])-as.Date("2012-10-09"))
multi$game_time2[multi$title=="battle2"] <- as.numeric(as.Date(multi$date[multi$title=="battle2"])-as.Date("2011-12-13"))
multi$game_time2[multi$title=="battle3"] <- as.numeric(as.Date(multi$date[multi$title=="battle3"])-as.Date("2012-06-26"))
multi$game_time2[multi$title=="battle4"] <- as.numeric(as.Date(multi$date[multi$title=="battle4"])-as.Date("2012-09-11"))
multi$game_time2[multi$title=="battle5"] <- as.numeric(as.Date(multi$date[multi$title=="battle5"])-as.Date("2012-12-04"))
multi$game_time2[multi$title=="halo3"] <- as.numeric(as.Date(multi$date[multi$title=="halo3"])-as.Date("2013-04-08"))
multi$game_time2[multi$update_yn==1] <- 0

multi$since_adop <- multi$since_adop +1
multi$new_freq <- multi$cumsum2/multi$since_adop

multi$freq30_2 <- multi$freq30/30
multi$freq_freq <- multi$freq30_2/multi$new_freq

summary(multi)

write.csv(single, "2019-10-10 single.csv")
write.csv(multi, "2019-10-10 multi.csv")