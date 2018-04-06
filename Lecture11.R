

my_func<-function(arg1, arg2){
  #body of the function
}






my_add<-function(x=1,y=1){
  x+y
}

my_add1<-function(x,y){
  x+y
}



mode(my_add)
my_add()
my_add(2,3)




my.abs<-function(x=-2){
  if (x<0){
    -x
  }else{
    x
  }
}



my.mean<-function(x){
  n=length(x)
  S=0
  for(i in 1:n){
    S=S+x[i]
  }
  S/n
}


my.factorial<-function(n){
  S=1
  for (i in 1:n){
    S=S*i
  }
  S
}

my.factorial2<-function(n){
  s=1
  for(i in 1:n){
    if(i%%2==1){
      s=s*i
    }
  }
s
}


my.factorial3<-function(n){
  S=1;
  count=1
  while(count<=n){
    if(count%%2==1){
      S=S*count
    }
    count=count+1
  }
  S
}

matrix.sum=function(x){
  n=nrow(x)
  m=ncol(x)
  S=0
  for (i in 1:n){
    for(j in 1:m){
      S=S+x[i,j]
    }
  }
  S
}

matrix.sum1=function(x){
  n=nrow(x)
  m=ncol(x)
  S=0
  for (i in 1:n){
    for(j in 1:m){
      if(x[i,j]%%2==0) {
       S=S+x[i,j] 
      }
    }
  }
  S
}

Joe.sum<-function(){
  S=0
  for (i in 2:3){
    for(j in 5:12){
      S=S+(2*i-3)/j^2
    }
  }
  S
}


  my.abs()
  my.abs(-4)
  
  
  
  
  
  

#Scoping with R
x<-10
x
f<-function(){
  x<-1
  x<-x+1
  y<-3
  c(x,y)
}

f()  
x



f1<-function(){
  y<-4
  c(x,y)
}




f1()
x

rm(x)


f2<-function(){
  y<-4
  c(x,y)
}
f2()








#Find all the prime numbers less than n
install.packages("matlab")
isprime(2)
isprime(3)
isprime(4)
isprime(5)
isprime(6)

my.prime<-function(n=10){
  for(i in 1:n){
    if(isprime(i)==1)
      print(i)
  }
}
my.prime()
my.prime(20)


my.is.prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

my.is.prime(2)
my.is.prime(3)
my.is.prime(6)

#Q1: Write a program to find the sum : 1+2+...+n for any n
#Q2: Write a program to find n! for any positive number n
#Q3: Write a program to find the summ of all entries in the matrix for any matrix



s=0
i=1
while(s<20){
  s=s+(1/i)
  i=i+1
}



#> setwd("/Users/roman/Desktop/DATA220")

#> dir()




