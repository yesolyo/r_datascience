#9주차
# 1.데이터 준비하기

# Load
# 원본 데이터 불러오기
ods <- read.csv("통계청사망자데이터셋.csv" , stringsAsFactors = F , header = T)
rs <- read.csv("일반사망요약분류표.txt", sep="\t",stringsAsFactors = F , header = T) 

# Column Subset
# 필요한 컬럼만 선택하고, 컬럼이름 부여하기
mycols <- c(3,5,11)
dataset <- ods[ , mycols ]            
colnames(dataset) <- c("ymd","age","사망원인코드")

# Missing Value 및 R을 위한 컬럼 Format 설정
summary( dataset )
dataset$ymd <- as.Date( dataset$ymd ); summary( dataset )
dataset <- dataset[ which(dataset$age<150),]; summary( dataset )
dataset$사망원인코드 <- as.character(dataset$사망원인코드); summary( dataset )
# 나이 -> 연령대로 범주화
dataset <- within( dataset, {
  연령대 <- NA
  연령대[age<=9] = "09세이하"
  연령대[age>=10 & age<=19] = "10~19세"
  연령대[age>=20 & age<=29] = "20~29세"
  연령대[age>=30 & age<=39] = "30~39세"
  연령대[age>=40 & age<=49] = "40~49세"
  연령대[age>=50 & age<=59] = "50~59세"
  연령대[age>=60 & age<=69] = "60~69세"
  연령대[age>=70 & age<=79] = "70~79세"
  연령대[age>=80 & age<=89] = "80~89세"
  연령대[age>=90] = "90세이상"
})
dataset <- dataset[-2] 
summary(dataset); str(dataset)


# 2. 데이터 탐색

# 기본 집계표 생성
options( digits=5 )
Totals <- nrow( dataset )
TotalYmd <- as.data.frame(table( dataset$ymd ))
colnames(TotalYmd) <- c("ymd","x")
CountReason <- table( dataset$사망원인코드 )
TotalReason <- as.data.frame(CountReason)
CountAge <-  table( dataset$연령대 ) 
TotalAge <- as.data.frame(CountAge)
TotalReason$per <- 100*TotalReason$Freq/Totals
TotalAge$per <- 100*TotalAge$Freq/Totals

# 시각화
par(mar=c(2,4,2,2))
timeseries <- ts(TotalYmd$x, c(2014,01,01), frequency = 364)
tsdecomp  <- decompose(timeseries)
plot(tsdecomp)
ts_data <- data.frame( TotalYmd$ymd
                       ,tsdecomp$x  
                       ,tsdecomp$trend  
                       ,tsdecomp$seasonal  
                       ,tsdecomp$random )
barplot( CountReason[1:20], main="사망원인", xlab="사망원인", ylab="사망자수" )
barplot( CountReason[21:40], main="사망원인", xlab="사망원인", ylab="사망자수" )
barplot( CountReason[41:60], main="사망원인", xlab="사망원인", ylab="사망자수" )
barplot( CountReason[61:81], main="사망원인", xlab="사망원인", ylab="사망자수" )
barplot( CountAge, main="연령대",xlab="사망원인", ylab="사망자수" )

# 분석 대상 선정
# -> 10대 사망원인=>50대
TopReason <- TotalReason[ order(-TotalReason$Freq),c("Var1","Freq") ]
TopReason <- TopReason[1:50,] 
colnames( TopReason ) <- c("사망원인코드","사망자수")
data <- merge(x=dataset,y=TopReason,by='사망원인코드')
data <- data[,c("ymd","사망원인코드","연령대")]

# 시계열 분석을 위한 데이터변환
#install.packages("reshape")
library(reshape)
YmdReason <- cast( data, ymd~사망원인코드 )

# 50대 사망원인만 대상으로 재집계하고 시각화
CountReason <- table( data$사망원인코드 )
TotalReason <- as.data.frame(CountReason)
CountAge <-  table( data$연령대 ) 
TotalAge <- as.data.frame(CountAge)
TotalReason$per <- 100*TotalReason$Freq/Totals
TotalAge$per <- 100*TotalAge$Freq/Totals
barplot( CountReason, main="사망원인", xlab="사망원인", ylab="사망자수" )
barplot( CountAge, main="연령대",xlab="사망원인", ylab="사망자수" )

# Top1 94번 사망원인으로 시계열 분석 및 시각화
timeseries <- ts(YmdReason$"94", c(2014,01,01), frequency = 364)
tsdecomp  <- decompose(timeseries)
plot(tsdecomp)
ts_data <- data.frame( seq(1:1401)
                       ,TotalYmd$ymd
                       ,tsdecomp$x
                       ,tsdecomp$trend
                       ,tsdecomp$seasonal
                       ,tsdecomp$random )
colnames(ts_data)<-c("순서","년 월 일","X","T","S","R")
ts_data$i<-(ts_data$순서-1)%/%364
ts_data$j<-(ts_data$순서-1)%%364

#x(사망자수)그래프(색상지정)
#ann=F를 사용하여 축 제목이 안나오게 설정함
ts1<-ts_data[ts_data$i==0,]
plot(ts1$j, ts1$X, col='black', type='l',ann=F)
ts2<-ts_data[ts_data$i==1,]
lines(ts2$j,ts2$X,col='red')
ts2<-ts_data[ts_data$i==2,]
lines(ts2$j,ts2$X,col='blue')
ts2<-ts_data[ts_data$i==3,]
lines(ts2$j,ts2$X,col='green')
title("사망자수")

#Trend 그래프(색상지정)
#ann=F를 사용하여 축 제목이 안나오게 설정함
plot(ts_data$j, ts_data$T, type='n',ann=F)
ts2<-ts_data[ ts_data$i==0,]
lines(ts2$j,ts2$T, col='black')
ts2<-ts_data[ ts_data$i==1,]
lines(ts2$j,ts2$T, col='red')
ts2<-ts_data[ ts_data$i==2,]
lines(ts2$j,ts2$T, col='blue')
ts2<-ts_data[ ts_data$i==3,]
lines(ts2$j,ts2$T, col='green')
title("추이")

#Seasonal 그래프(색상지정)
#ann=F를 사용하여 축 제목이 안나오게 설정함
ts1<-ts_data[ts_data$i==0,]
plot(ts1$j, ts1$S, col='black', type='l',ann=F)
title("Seasonal")

#Random 그래프(색상지정)
#ann=F를 사용하여 축 제목이 안나오게 설정함
ts1<-ts_data[ts_data$i==0,]
plot(ts1$j, ts1$R, col='black', type='l',ann=F)
ts2<-ts_data[ts_data$i==1,]
lines(ts2$j,ts2$R,col='red')
ts2<-ts_data[ts_data$i==2,]
lines(ts2$j,ts2$R,col='blue')
ts2<-ts_data[ts_data$i==3,]
lines(ts2$j,ts2$R,col='green')
title("Random")

#10주차
# 사망원인별 연령대 집계표
AgeTable <- table( data$연령대, data$사망원인코드 )
AgeDF <- data.frame( AgeTable )
colnames( AgeDF ) <- c("연령대","사망원인코드","명")
ReasonAge <- cast( AgeDF, 사망원인코드~연령대, 
                   value='명', fun.aggregate=sum )
# 비교 시각화
barplot( AgeTable, main="연령대별 사망원인"
         ,xlab="사망원인", ylab="연령대", col=rainbow(10) )
AgeTableProp<-prop.table(AgeTable,2)
barplot(AgeTableProp,main="연령대별 사망원인"
        ,xlab="사망원인", ylab="연령대",col=rainbow(10))



# 연령대별 사망자수를 비율로 변경
ReasonAge$t1 <- ReasonAge[2]+ReasonAge[3]+ReasonAge[4]+ReasonAge[5]+ReasonAge[6]
ReasonAge$t2 <- ReasonAge[7]+ReasonAge[8]+ReasonAge[9]+ReasonAge[10]+ReasonAge[11]
ReasonAge$total <- ReasonAge[12]+ReasonAge[13]
ReasonAge$age0 <- 100*ReasonAge[2]/ReasonAge[14]
ReasonAge$age1 <- 100*ReasonAge[3]/ReasonAge[14]
ReasonAge$age2 <- 100*ReasonAge[4]/ReasonAge[14]
ReasonAge$age3 <- 100*ReasonAge[5]/ReasonAge[14]
ReasonAge$age4 <- 100*ReasonAge[6]/ReasonAge[14]
ReasonAge$age5 <- 100*ReasonAge[7]/ReasonAge[14]
ReasonAge$age6 <- 100*ReasonAge[8]/ReasonAge[14]
ReasonAge$age7 <- 100*ReasonAge[9]/ReasonAge[14]
ReasonAge$age8 <- 100*ReasonAge[10]/ReasonAge[14]
ReasonAge$age9 <- 100*ReasonAge[11]/ReasonAge[14]

ReasonAge2 <- data.frame( ReasonAge[1],ReasonAge[15:24] )
names(ReasonAge2)[2:11] <- colnames(ReasonAge)[2:11]
colnames(ReasonAge2)  

# 사망원인별 유사도 계산 및 시각화
rownames(ReasonAge2) <- ReasonAge2[,1]
ReasonAge2 <- ReasonAge2[-1]
ReasonDist <- dist(ReasonAge2, method="euclidean")
two_coord <- cmdscale(ReasonDist)
plot(two_coord, type="n", xlab="x", ylab="y")
text(two_coord, rownames(ReasonAge2) )

# 계층적 군집
library( cluster )
hcl <- hclust( dist(ReasonAge2), method="single")
plot(hcl, hang=-1, xlab="사망원인", ylab="거리")

# 분할적 군집
library( graphics )
kms <- kmeans( ReasonAge2, 4 )
kms 

#11주차
#트리맵을 이용하려고 했으나, 교수님께 보내드린 사진처럼 오류가 떠서 버블차트로 대체하였습니다.
library(MASS)
head(TopReason)
radius<-sqrt(TopReason$사망자수)
symbols(TopReason$사망원인코드,TopReason$사망자수,
        circles=radius, # 각각 써클의 반지름값         
        inches=0.4, # 각각 써클의 크기 조절값         
        fg="white", # 각각 써클의 테두리 색         
        bg="lightgray", # 각각 써클의 바탕색         
        lwd=1.5, # 각각 써클의 테두리선 두께         
        ylab="사만자수", # y 축 제목 설정 
        main="사망원인")
text(TopReason$사망원인코드,TopReason$사망자수, # 문자로 출력할 x,y 위치      
     TopReason$사망원인코드, # 문자로 출력할 값 
     cex=0.8, # 글자 크기
     col="brown")

#항목분류를 이용하여 사망원인코드를 원인으로 변환시켜서 나오게 작성하였습니다.
rs$X103항목분류 <- as.character(rs$X103항목분류)
reason <- inner_join(x=TopReason, y=rs, by=c("사망원인코드"="X103항목분류"))
head(reason)
radius<-sqrt(reason$사망자수)
symbols(reason$사망원인코드,reason$사망자수,
        circles=radius, # 각각 써클의 반지름값         
        inches=0.4, # 각각 써클의 크기 조절값         
        fg="white", # 각각 써클의 테두리 색         
        bg="lightgray", # 각각 써클의 바탕색         
        lwd=1.5, # 각각 써클의 테두리선 두께         
        ylab="사만자수", # y 축 제목 설정 
        main="사망원인")
text(reason$사망원인코드,reason$사망자수, # 문자로 출력할 x,y 위치      
     reason$원인, # 문자로 출력할 값 
     cex=0.8, # 글자 크기
     col="brown")
