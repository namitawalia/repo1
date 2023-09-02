# Revenue Forecasting for B2B clients using Markov process

#Matrix for October-November data

Oct_Nov= matrix(c(2005,rep(0,4),400,1002,602,0,0,294,0,793,903,0,913,0,0,597,486,
                  rep(0,4),548), byrow=TRUE, nrow=5)

Oct_Nov<- rbind(Oct_Nov, colSums(Oct_Nov)) #rbind is to show the sum in the row(below horizontally) and rowSums will sum the row
Oct_Nov<- cbind(Oct_Nov, rowSums(Oct_Nov))

rownames(Oct_Nov)<-c("Lost","Stage A","Stage B","Stage C","Won","col_total")
colnames(Oct_Nov)<-c("Lost","Stage A","Stage B","Stage C","Won","row_total")
Oct_Nov

#November_December Data
Nov_Dec=matrix(c(1607,rep(0,4),450,1106,570,0,0,279,0,808,974,0,
                 871,0,0,601,597,rep(0,4),486),byrow=TRUE,nrow=5)

Nov_Dec<-rbind(Nov_Dec,colSums(Nov_Dec))
Nov_Dec<-cbind(Nov_Dec,rowSums(Nov_Dec))

rownames(Nov_Dec)<-c("Lost","Stage A","Stage B","Stage C","Won","col_total")
colnames(Nov_Dec)<-c("Lost","Stage A","Stage B","Stage C","Won","row_total")
Nov_Dec

#December_January data
Dec_Jan=matrix(c(1600,rep(0,4),521,1248,670,0,0,243,0,545,808,0,793,0,0,514,601,rep(0,4),597)
               ,byrow=TRUE,ncol=5)
Dec_Jan<-rbind(Dec_Jan,colSums(Dec_Jan))
Dec_Jan<-cbind(Dec_Jan,rowSums(Dec_Jan))

rownames(Dec_Jan)<-c("Lost","Stage A","Stage B","Stage C","Won","col_total")
colnames(Dec_Jan)<-c("Lost","Stage A","Stage B","Stage C","Won","row_total")
Dec_Jan


#Aggregate data (USD Million): Finding Average

avg=(Oct_Nov+Nov_Dec+Dec_Jan)/3
avg=apply(avg,2,round) #to round off

avg=apply(avg,2,round)[-6,] #display all except 6 row or minus 6 row

#Finding Transition Probabilities

avg=avg[,-6]/avg[,6]   
avg


avg[,-6] # To understand the division with array index
avg[,6]


#Define a function to round off probabilities

rnd<-function(x)
{
  round(x,2)
}

prob=apply(avg,2,rnd)
prob

# prob1=apply(avg,2,round(avg,2)) #Error in match.fun(FUN) : 
# 'round(avg, 2)' is not a function, character or symbol
# prob1

#Reorder by column index and row index

prob=prob[c(1,5,2,3,4),]
prob=prob[,c(1,5,2,3,4)]

I=diag(3)  #default value of x=1

#Transition probability between non absorbing state

Q=prob[3:5,3:5]

#F=inverse(I-Q) matrix - Fundamental Matrix

IQ=I-Q
IQ

# F matrix represents the amount of time an opportunity in a particular state will
# take, to reach an absorbing state(Won or Lost or Withdrawn)

# F-matrix

F=apply(solve(IQ),2,rnd)
F

F1<-cbind(F,rowSums(F))
F1

colnames(F1)<-c("Stage A","Stage B","Stage C","Time taken")

F1[,4] #Time taken by each state to reach an absorbing state(Won/Lost)


#Find probability of Won and Lost
#R Probability of being absorbed in next transition

R=prob[3:5,1:2]
R

FR=F %*% R
FR

#Define a function to convert values in percentage
percent<-function(x)
{
  paste0(x*100,"%")
}

FR=apply(FR,2,percent)
FR=noquote(FR)                      #To print without quote
FR
 
FR=noquote(apply(FR,2,percent))     #Integrated functions
FR

#FR gives us the probability of being absorbed in next transition
