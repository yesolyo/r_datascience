# 1.데이터 준비하기

# Load
# 원본 데이터 불러오기
ods <- read.csv("소방청.csv" , stringsAsFactors = F , header = T)

# Column Subset
# 필요한 컬럼만 선택하고, 컬럼이름 부여하기
#발생장소를 가지고 사고원인을 알아보고자 3개의 컬럼을 뽑았습니다.
mycols <- c(2,6,9)
dataset <- ods[ , mycols ]            
colnames(dataset) <- c("ymd","발생장소","사고원인")

# Missing Value 및 R을 위한 컬럼 Format 설정
summary( dataset )
dataset$ymd <- as.Date( dataset$ymd ); summary( dataset )
dataset$발생장소 <- as.character(dataset$발생장소); summary( dataset )
dataset$사망원인 <- as.character(dataset$사고원인); summary( dataset )

# 여러 광역시들을 해당하는 도의 이름으로 바꾸었습니다.
dataset <- within( dataset, {
  발생도 <- NA
  발생도[발생장소=="서울특별시"] = "서울"
  발생도[발생장소=="경기도"] = "경기도"
  발생도[발생장소=="강원도"] = "강원도"
  발생도[발생장소=="충청북도" ] = "충청북도"
  발생도[발생장소=="충청남도" ]="충청남도"
  발생도[발생장소=="대전광역시"]= "충청남도"
  발생도[발생장소=="세종특별자치도"] ="충청남도"
  발생도[발생장소=="경상북도"]="경상북도"
  발생도[발생장소=="대구광역시"] ="경상북도"
  발생도[발생장소=="경상남도"] = "경상남도"
  발생도[발생장소=="울산광역시"] = "경상남도"
  발생도[발생장소=="부산광역시"] = "경상남도"
  발생도[발생장소=="전라북도"] = "전라북도"
  발생도[발생장소=="전라남도"]= "전라남도"
  발생도[발생장소=="광주광역시"] = "전라남도"
  발생도[발생장소=="제주특별자치도"] = "제주도"
  
})
dataset <- dataset[-2] 
summary(dataset); str(dataset)
# 2. 데이터 탐색

# 기본 집계표 생성
options( digits=5 )
Totals <- nrow( dataset )
TotalYmd <- as.data.frame(table( dataset$ymd ))
colnames(TotalYmd) <- c("ymd","x")
CountReason <- table( dataset$사고원인 )
TotalReason <- as.data.frame(CountReason)
CountAge <-  table( dataset$발생도 ) 
TotalAge <- as.data.frame(CountAge)
TotalReason$per <- 100*TotalReason$Freq/Totals
TotalAge$per <- 100*TotalAge$Freq/Totals

# 시각화
#plot의 margin크기를 설정하였습니다.
par(mar=c(2,2,2,2))
#그냥 실행하면 지수로 표기되기때문에 숫자표기로 바꾸기 위해서 설정하였습니다.
options(scipen=5)
timeseries <- ts(TotalYmd$x, c(2015,01,01), frequency = 364)
tsdecomp  <- decompose(timeseries)
plot(tsdecomp)
ts_data <- data.frame( TotalYmd$ymd
                       ,tsdecomp$x  
                       ,tsdecomp$trend  
                       ,tsdecomp$seasonal  
                       ,tsdecomp$random )
#사고원인 수가 39개이기 때문에 양이 많아서 두번에 걸쳐서 barplot이 나타나게 설정하였습니다.
barplot( CountReason[1:20], main="사고원인", ylab="사고수" )
barplot( CountReason[21:39], main="사고원인", ylab="사고수" )
barplot( CountAge, main="발생장소",ylab="사고수" )

# 분석 대상 선정
# -> 10대 사고원인
TopReason <- TotalReason[ order(-TotalReason$Freq),c("Var1","Freq") ]
TopReason <- TopReason[1:10,] 
colnames( TopReason ) <- c("사고원인","사고수")
data <- merge(x=dataset,y=TopReason,by='사고원인')
data <- data[,c("ymd","사고원인","발생도")]

# 시계열 분석을 위한 데이터변환
#install.packages("reshape")
library(reshape)
YmdReason <- cast( data, ymd~사고원인 )

# 10대 사고원인만 대상으로 재집계하고 시각화
CountReason <- table( data$사고원인 )
TotalReason <- as.data.frame(CountReason)
CountAge <-  table( data$발생도 ) 
TotalAge <- as.data.frame(CountAge)
TotalReason$per <- 100*TotalReason$Freq/Totals
TotalAge$per <- 100*TotalAge$Freq/Totals
barplot( CountReason, main="사고원인", xlab="사고원인", ylab="사망자수" )
barplot( CountAge, main="발생장소",xlab="사고원인", ylab="사망자수" )

#사고원인별 파이차트
pie(CountReason,radius=3,main="사고원인"
    ,col=rainbow(10))

# 사고원인별 발생장소 집계표
AgeTable <- table( data$발생도, data$사고원인 )
AgeDF <- data.frame( AgeTable )
colnames( AgeDF ) <- c("발생도","사고원인","명")
ReasonAge <- cast( AgeDF, 사고원인~발생도, 
                   value='명', fun.aggregate=sum )

# 비교 시각화(막대그래프)
#발생장소별로 사고원인을 시각화하였습니다.
barplot( AgeTable, main="발생도별 사고원인", col=rainbow(10) )
AgeTableProp<-prop.table(AgeTable,2)
barplot(AgeTableProp,main="발생도별 사고원인",col=rainbow(10))

# 발생장소별 사고수를 비율로 변경
#t1에는
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

# 사고원인별 유사도 계산 및 시각화
rownames(ReasonAge2) <- ReasonAge2[,1]
ReasonAge2 <- ReasonAge2[-1]
ReasonDist <- dist(ReasonAge2, method="euclidean")
two_coord <- cmdscale(ReasonDist)
plot(two_coord, type="n", ylab="y")
#유사도별 텍스트마다 컬러를 설정하였습니다.
text(two_coord, rownames(ReasonAge2),col=c('blue','red','purple','blue','green',
                                           'purple','purple','purple','purple','red'))
#버블차트를 이용한 텍스트시각화
library(MASS)
head(TopReason)
radius<-sqrt(TopReason$사고수)
symbols(TopReason$사고원인,TopReason$사고수,
        circles=radius, # 각각 써클의 반지름값         
        inches=0.4, # 각각 써클의 크기 조절값         
        fg="white", # 각각 써클의 테두리 색         
        bg="lightgray", # 각각 써클의 바탕색         
        lwd=1.5, # 각각 써클의 테두리선 두께         
        ylab="사고수", # y 축 제목 설정 
        main="사고원인")
text(TopReason$사고원인,TopReason$사고수, # 문자로 출력할 x,y 위치      
     TopReason$사고원인, # 문자로 출력할 값 
     cex=0.8, # 글자 크기
     col="brown")

# 계층적 군집
library( cluster )
hcl <- hclust( dist(ReasonAge2), method="single")
plot(hcl, hang=-1, ylab="거리")



