ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#벡터 생성하기
x<-c(10, 20, 30, 40) 
y<-c(10, 20, 30, 40)

#리스트 생성하기
my.list<-list("Streets",
              2000,
              "Parcels",
              5000,
              TRUE,
              FALSE)
#리스트 참조(1개)
my.list[[1]]
#리스트 참조(복수개)
my.list[c(1, 2)]

#매트릭스 생성하기
matrix<-matrix(c(2,4,3,1,5,7), nrow=2, ncol=3, byrow=TRUE)
matrix
colnames(matrix)<-c("pop2000", "pop2005", "pop2010")
matrix
matrix[2,3]
matrix[2,]
matrix[,"pop2005"]

colMeans(matrix)
colSums(matrix)
rowMeans(matrix)
rowSums(matrix)

#사용가능한 전체 데이터셋을 보기
data()

view(iris)

for (i in 1:nrow(iris))
{
  if(i==10){
    print(i+2) 
  }
  else{
    print(i)
  }
}

myfunction<-function(a,b){
  print(a+b)
}

myfunction(2,3)

view(iris)
library(dplyr)
names(iris)
iris1<-select(iris, "Sepal.Length", "Species")
view(iris1)
iris2<-filter(iris, Species=="setosa")
view(iris2)
levels(iris2$Species)

iris3=group_by(iris, Species)
iris4=summarise(iris3, n=n(), mean=mean(iris3$Sepal.Length))
head(iris4)


ggplot(data=iris4)+geom_col(mapping=aes(x=Species, y=n), fill="red")

library(readr)
setwd("f:/IntroR/Data")
dfCrime=read_csv("Crime_Data.csv",col_names=TRUE)
names(dfCrime)
dfCrime=select(dfCrime, "CrimeDate"="Reported Date",
               "Category"="Crime Subcategory",
               "Description"="Primary Offense Description",
               "Precinct","Sector","Beat","Neighborhood")
head(dfCrime)
dfCrime2=filter(dfCrime, Neighborhood=="QUEEN ANNE",
               Category=="BURGLARY-RESIDENTIAL")
head(dfCrime2)
names(dfCrime2)
dfCrime3=mutate(dfCrime2, 
                YEAR=year(as.Date(dfCrime2$CrimeDate,
                                  format='%m/%d/%Y')))
dfCrime4=group_by(dfCrime3, YEAR)
dfCrime4=summarise(dfCrime4, n=n())
view(dfCrime4)
ggplot(data=dfCrime4)+
  geom_col(mapping=aes(x=YEAR,y=n),
           fill="red")
dfCrime3=mutate(dfCrime2, 
                MONTH=month(as.Date(dfCrime2$CrimeDate,
                                    format="%m/%d/%Y")))
dfCrime4=group_by(dfCrime3, MONTH)
dfCrime4=summarise(dfCrime4, n=n())
view(dfCrime4)
ggplot(data=dfCrime4)+
  geom_col(mapping=aes(x=MONTH, y=n),
           fill="blue")
