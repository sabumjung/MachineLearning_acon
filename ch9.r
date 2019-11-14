#=======================================
#PerformanceAnalytics 패키지 설치
install.packages("PerformanceAnalytics")
#corrplot 패키지 설치
install.packages("corrplot")
#왜도/첨도 계산 패키지 설치
install.packages("fBasics")
#최빈값 계산 패키지 설치
install.packages('prettyR')

#PCA용 패키지 설치
install.packages("MVA")
#FA용 패키지 설치
install.packages(c('psych','GPArotation'))

#시퀀스패턴마이닝 분석 패키지 설치
install.packages("TraMineR")
#의사결정나무 분석 패키지 설치
install.packages("train")
install.packages("tree")
install.packages("party")
#svm분석 패키지 설치
install.packages("e1071")
install.packages("kernlab")
#군집개수분석 패키지 설치
install.packages("NbClust")
install.packages("mclust")
#이상치 찾기 위한 패키지 설치
install.packages("DMwR")
#knn분석 패키지 설치
install.packages("kknn")
#연관성 분석 패키지 설치
install.packages("arules")
install.packages('arulesNBMiner')



##=====================================
#==> 분석전 알아두면 좋은 팁
#데이터 프레임 추가
attach(mtcars)
summary(mpg)
plot(wt,mpg)

#데이터 프레임 제거
detach(mtcars)


#==> 통계함수 반복 적용하기
#행대상 반복 적용
a<-array(1:12, dim=c(3,4))
apply(a, 1, max)

#열대상 반복 적용
apply(a,2, max)


#==> 기초연산
#log함수
log(3)
log10(100)

#버림, 올림, 반올림
floor(3.14)
ceiling(5.87)
round(4.65)

#최소, 최대, 합계
min(c(1,5,7))
max(c(2,6,9))
sum(c(1,2,4,7))

#수학함수 사용(sin, sqrt, runif)
sin(pi)
sqrt(9)
runif(2,1,10)  #2차원 데이터 생성(1~10)

#==> 미적분 계산
#편미분
f<-expression(2*x^3-y*x^2+2*y^2+1)
D(f, "x")
D(f, "y")

#정적분
f<-function(x) 2*x^3-3*x^2+1
integrate(f, 0, 3)

#==> 주요통계값 계산
names(iris)

#평균
mean(iris$Sepal.Length)
iris_filter=iris[1:4]
names(iris_filter)

#여러열의 평균
colMeans(iris_filter)

#표준편차
sd(iris$Sepal.Length)
#분산
var(iris$Sepal.Length)
#최빈값
library(prettyR)
Mode(mtcars$cyl)
#중위수 절대편차
mad(iris$Sepal.Length)
#공분산
cov(iris$Sepal.Length, iris$Sepal.Width)
#중위수
median(iris$Sepal.Length)
#사분위범위(3번째 분위값 - 1번째 분위값)
IQR(iris$Sepal.Length)
#범위
range(iris$Sepal.Length)

#사분위
quantile(iris$Sepal.Length)
#Turkey Five-number summary
fivenum(iris$Sepal.Length)

#정규화((원래값-평균) / 표준편차)
scale(iris$Sepal.Length)
#변동계수(표준편차 / 산술평균)
sd(iris$Sepal.Length)/mean(iris$Sepal.Length)
#상관분석
cor(iris_filter)

#왜도
library(fBasics)
skewness(iris_filter)
#첨도
kurtosis(iris_filter)
#최대, 최소, 사분위, 평균, 빈도 계산
summary(iris_filter)

#표본수, 평균, 표준편차, 중위수, 최소, 최대, 범위, 왜도, 첨도 등을 계산
library(psych)
describe(iris_filter)

#사칙연산/관측치개수
sum(1,2,3)
prod(1,2,3)




#======================================
##==> 다변량(상관분석)
aq1<-airquality[,c(1:4)]
View(aq1)
#상관분석
cor(aq1)
#결측치 제거하기
aq2<-na.omit(aq1)
#상관분석
cor(aq2)
#Scatter Plot
plot(aq2)
#추세선 표시
pairs(aq2, panel=panel.smooth)

#histogram in scatter plot
library(PerformanceAnalytics)
chart.Correlation(aq2, histogram=TRUE, pch=19)

#그래프에 상관계수 표현
library(corrplot)
corrplot(cor(aq2), method="number")



#======================================
##==> 주성분분석(PCA)
library(MVA)
df<-heptathlon
df

#달리기 기록은 작을수록 좋음을 반영하여 변환
df$hurdles<-with(df,max(hurdles)-hurdles)
df$run200m<-with(df,max(run200m)-run200m)
df$run800m<-with(df,max(run800m)-run800m)
df
#주성분 분석 수행
df2<-df[df$hurdles>0,]
pca<-prcomp(df2[,-8], scale=T)
summary(pca)

pca

#Scree Plot 표시
plot(pca, type="|")
#pc1과 score간 상관관계 분석
cor(df2$score, pca$x[,1])
biplot(pca)


log.ir<-log(iris[,1:4])
ir.species<-iris[,5]
#변수별 scale이 다르므로 표준화하기 위해 scale=T로 설정
ir.pca<-prcomp(log.ir,center=T, scale.=T)
plot(ir.pca,type='l')
prc<-as.matrix(log.ir) %*% ir.pca$rotation
head(prc)



#PCA를 이용한 분류 모델링
train1<-cbind(ir.species,as.data.frame(prc))
train1[,1]<-as.factor(train1[,1])
colnames(train1)[1]<-'label'
fit1<-lm(label~PC1+PC2,data=train1)
fit1_pred<-predict(fit1,newdata=train1)
b<-round(fit1_pred)
b[b==0|b==1]<-'setosa'
b[b==2]<-'Versicolor'
b[b==3]<-'Virginica'
a<-ir.species
#confusion matrix를 이용한 분류정확도 계산
#b:예측값, a:실제값
table(b,a)




##==> 요인분석(FA)
med.data<-read.table('c:/temp/medFactor.txt',header=T)
head(med.data)
summary(med.data)
str(med.data)

#요인분석 실행
library(psych)
library(GPArotation)
med.factor<-principal(med.data,rotate='none')
names(med.factor)
med.factor$values
plot(med.factor$values, type='b')

# 요인은 앞에서 분석한대로 3개를 선정함
# 회전방법은 직교회전방법중 하나인 varimax를 사용함
# 회전이유는 개별변수의 해석을 용이하게 하기 때문임
med.Varimax = principal(med.data, nfactors = 3, rotate="varimax")
med.Varimax




##==> kmeans 군집분석
kmeans(x,  
       centers,  
       iter.max = 10,
       nstart = 1,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
       trace=FALSE)

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2), 
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
fit<-kmeans(x,10)
names(fit)
fit

#최적 군집개수 정하기
#NbClust 패키지를 사용하는 방법
library(NbClust)
nc <- NbClust(x, min.nc = 2, max.nc = 15, method = "kmeans")


#최적 군집개수 정하기
#군집내 제곱합(withinss)를 이용하는 방법
results<-matrix(nrow=14, ncol=2, 
                dimnames=list(2:15,
                              c("cluster s",
                              "within gr. Sum of squares")))
for(i in 2:15){
  fit<-kmeans(x,i)
  results[i-1,1]<-i
  results[i-1,2]<-fit$tot.withinss
  }
plot(results, type='b')
results



#==> kmedoids군집분석
library(cluster)
a<-c(1,2,1,2,1,3,2,2,3)
b<-c(10,11,10,12,4,5,6,5,6)
x<-data.frame(a,b)
x
result<-pam(x,2,FALSE,'euclidean')
summary(result)
plot(result$data,col=result$clustering,pch=16)




#==> Hierarchical군집분석
#거리계산
c<-c(0,2,3,6,7)
dist(c)

dat<-matrix(rnorm(100),nrow=10, ncol=10)
dat
hc<-hclust(dist(dat))
hc
plot(hc)




#==>밀도추정 방법에 의한 군집분석
d<-density(iris$Sepal.Length)
d
#대부분의 데이터가 5~7에 위치함
#Sepal.Length의 평균은 대략 6에 해당함
plot(d)







#======================================
##==> 박스플롯으로 Anomaly Detection
x<-rnorm(100)
summary(x)()
boxplot.stats(x)$out
boxplot(x)



##==>산점도로 Anomaly Detection
x <- rnorm(1000)
y <- rnorm(1000)
f <- data.frame(x,y)
a <- boxplot.stats(x)$out
b <- boxplot.stats(y)$out
list <- union(a,b)
plot(f)
px <- f[f$x  %in% a,]
py <- f[f$y  %in% b,]
p <- rbind(px,py)
par(new=TRUE)
plot(p$x, p$y,cex=2,col=2)



##==>사용자 함수로 Anomaly Detection
outliers<-function(data, low, high){
  outs<-subset(data, data[,1] < low | data[,1] > high)
  return(outs)
}
outliers(iris, 4.5, 7.5)



##==>DMwR사용하여 Anomaly Detection
library(DMwR)
nospecies <- iris[,1:4]
# k : outlier계산을 위한 이웃 갯수
scores <- lofactor(nospecies, k=5)
# 밀도그래프를 그려보고 이상치 기준을 판단한다.
plot(density(scores), main='iris데이터 이상치')
# score값을 내림차순으로 정렬하여 보고
# 이상치 기준을 판단한다.
sort(scores, decreasing=T)[1:10]
# socre 1.9를 기준이상인 경우 outlier처리
outlier<-order(scores,decreasing=T)[1:3]
outlier



##==>PCA사용하여 Anomaly Detection
labels<-1:nrow(nospecies)
labels[-outlier]<-"."
biplot(prcomp(nospecies),cex=0.8, xlabs=labels)



##==>산점도 사용하여 Anomaly Detection
pch="."
pch[outlier]<-"@"
col<-rep("black",nrow(nospecies))
col[outlier]<-"red"
pairs(nospecies,pch="@",col=col)




#======================================
##==>연관성 분석(Apriori)
library(arules)
filenm="c:/temp/groceries.csv"
data<-read.csv(filenm)
rules<-apriori(data)
rules
inspect(rules)
# apriori의 parameter를 수정하고자할 경우
rules <- apriori(data, parameter = list(supp = 0.001, conf = 0.8))
inspect(rules[1:10])



#==> 연관성 분석(Apriori)
library(arules)
#데이터 읽어오기
tr<-read.transactions('http://fimi.ua.ac.be/data/retail.dat', format='basket')
#읽어온 데이터 요약정보 보기
summayr(tr)
#Item빈도 그래프 그려보기
itemFrequencyPlot(tr, support=0.1)
#연관규칙 찾아내기(apriori사용)
#지지도 0.5, 신뢰도 0.5
rules<-apriori(tr,parameter=list(supp=0.5, conf=0.5))
#룰정보 요약해서 보기
summary(rules)
#룰 목록을 보기
inspect(rules)
#연관규칙에 대한 지지도, 신뢰도 , 향상도등의 정보 보기
interestMeasure(rules,  c('support','chiSquare','confidence','conviction','cosine','leverage','lift','oddsRatio'),tr)



#==> 연관성 분석(Eclat)
#미국 성인 인구조사 데이터에 대한 분석
data("Adult")
dim(Adult)[1]
summary(Adult)

#eclat을 이용한 연관성 분석
itemsets<-eclat(Adult)
itemsets.sorted<-sort(itemsets)
itemsets.sorted[1:5]
inspect(itemsets.sorted[1:5])
inspect(itemsets.sorted[100:100])

#길이가 9이고 빈도값이 가장 큰 Rule 도출
data("Adult")
itemsets <- eclat(Adult, parameter=list(minlen=9))
inspect(itemsets)
showMethods("items")






#======================================
#==> Sequence Patterns 마이닝
library('TraMineR')
data(mvad)
summary(mvad)

# 17~86번째 열까지 데이터를 대상으로 분석
myseq<-seqdef(mvad, 17:86)
#index plot 그리기(가장 빈도가 많은 순서로)
#개별 데이터에 대한 Sequence를 그리기
seqiplot(myseq, idx=1:10)
#Sequence단위별 빈도분석을 하여 Frequencyplot그리기
seqfplot(myseq)

#분포 Distribution Plot 그리기
seqdplot(myseq)
#Entropy chart 그리기
seqHtplot(myseq)

#개별 state sequence data에 대해
#몇개의 sub-sequence data로 구분가능한지를 계산한 지표임
myturbulence<-seqST(myseq)
hist(myturbulence)


data(famform)
seq<-seqdef(famform)

#시퀀스 3 : S-U-M-MC
#시퀀스 4 : S-U-M-MC-SC
#시퀀스 3과 4를 비교 : 동일한 최대길이(4)
#Longest Common Prefix
seqLLCP(seq[3,], seq[4,])


#시퀀스 1 : S-U
#시퀀스 2 : S-U-M
#Longest Common Subsequence
#시퀀스 1과 2를 비교 : 일치하는 sub-seq중 가장 긴 길이
seqLLCS(seq[1,], seq[2,])

cost<-seqsubm(seq, method='CONSTANT', cval=2)
cost


LCS.ex<-c("S-U-M-S-SC-U-SC-U", 
          "S-1-M-1-SC-1-1-U")
LCS.ex<-seqdef(LCS.ex)
#Longest Common Prefix
#시퀀스 1과 2를 비교한 결과 n개의 prefix가 일치함
seqLLCP(LCS.ex[1,], LCS.ex[2,])

#Longest Common Subsequence
#일치하는 Sub-Seq길이중에서 가장 긴 길이
seqLLCS(LCS.ex[1,], LCS.ex[2,])




#======================================
## DT
filenm="c:/temp/Heart.csv"
df<-read.csv(filenm,sep=",")
str(df)
head(df)

library(caret)
set.seed(1000)  #재현성을 위한 random seed설정
intrain<-createDataPartition(y=df$AHD, p=0.7, list=FALSE)
train<-df[intrain,]
test<-df[-intrain,]


#==> tree 패키지 사용
library(tree)
treemod<-tree(AHD~. , data=train)
plot(treemod)
text(treemod)



#DT결과에 대한 Pruning 작업
#Tree패키지의 cv.tree를 사용하여 k-fold crossvalidation을 수행한다.
cv.trees<-cv.tree(treemod, FUN=prune.misclass )
plot(cv.trees)

#가지치기를 위해서 prune.trees를 사용한다.
prune.trees <- prune.misclass(treemod, best=7)
plot(prune.trees)
text(prune.trees, pretty=0)

#tree기반 의사결정 모델링의 정확도 계산
treepred <- predict(prune.trees, test, type='class')
confusionMatrix(treepred, test$AHD)


#==> rpart 패키지 사용
library(rpart)
library(e1071)
rpartmod<-rpart(AHD~.,  data=train, method='class')
plot(rpartmod)
text(rpartmod)

#Pruning
printcp(rpartmod)
plotcp(rpartmod)

which.min(rpartmod$cptable[,"xerror"])
ptree<-prune(rpartmod, cp=rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)

#rpart기반 의사결정 모델링의 정확도 계산
rpartpred<-predict(ptree, test, type='class')
confusionMatrix(rpartpred, test$AHD)


#==> party 패키지 사용
library(party)
partymod<-ctree(AHD~., data=train)
plot(partymod)

#party기반 의사결정 모델링의 정확도 계산
partypred<-predict(partymod, test)
confusionMatrix(partypred, test$AHD)





#===========================================
##==> 회귀분석
require(datasets); require(ggplot2)
data(swiss)
str(swiss)
summary(swiss)

hist(swiss$Infant.Mortality)
qqnorm(swiss$Infant.Mortality)
qqline(swiss$Infant.Mortality)


model<-lm(Infant.Mortality~. ,data=swiss)
summary(model)


#Fertility만을 입력변수로 고려한 회귀예측 모델  생성
model_simple<-lm(Infant.Mortality~Fertility ,data=swiss)
#기존 회귀예측 모델과의 유의차 검증
anova(model, model_simple)


new_Fertility<-rnorm(10, mean=mean(swiss$Fertility), sd=sd(swiss$Fertility))
new_Fertility<-as.data.frame(new_Fertility)
colnames(new_Fertility)<-c("Fertility")
predict(model_simple, new_Fertility, interval="prediction")



##==> knn
library(kknn)
## iris 데이터셋을 불러오기
data(iris)
## 데이터를 훈련+테스트셋으로 구분하기  
idxs <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))  
trainIris <- iris[idxs,]
testIris <- iris[-idxs,]
## knn모델링(k=3, 정규화 적용하지 않음)  
nn3 <- kknn(Species ~ .,trainIris,testIris,k=1)  ## 혼동행렬(confusion matrix)생성  
table(testIris[,'Species'],nn3$fitted.values)

## knn모델링(k=5, 정규화 적용함)
nn5 <- kknn(Species ~ .,trainIris,testIris,k=5)  
## 혼동행렬(confusion matrix)생성  
table(testIris[,'Species'],nn5$fitted.values)



##==>ANN
# nnet 패키지를 불러온다.
library(nnet)
# 입력데이터를 설정한다.
input<-matrix(c(0,0,1,1,0,1,0,1),ncol=2)
input

#출력데이터를 설정한다.
#XOR(Minsky가 업급했듯이 XOR은 Single Layer ANN으로 분류불가능)
#output<-matrix(c(0,1,1,0))


#OR
output<-matrix(c(0,1,1,1))
output

# 신경망 모델링을 생성한다.
# 최대 interation회수는 100으로 설정한다.
ann<-nnet(input,output,maxit=100, size=2, decay=0.001)  # weights: 9

# 생성된 신경망의 구조를 본다.
ann
#생성된 모델링에 input값을 입력하여 예측한 결과를 본다.
result<-predict(ann, input)
result



#==========================================
#==> SVM
library(caret)
library(kernlab)
data('spam')
summary(spam)
table(spam$type)
index<-1:nrow(spam)
testindex<-sample(index,trunc(length(index)/3))
testset<-spam[testindex,]
trainingset<-spam[-testindex,]

model<-ksvm(type~.,data=trainingset, kernel='rbfdot', C=1, nu=0.1)
model
typeval<-predict(model, testset)
confusionMatrix(typeval, testset$type)

