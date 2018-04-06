x1=runif(1,0,10)
if(x1>3){
  y=10
} else{
  y=0
}
y

#Find the absolulate value of a random number
x2=rnorm(1,70,3)
if(x2<0) {
  print(-x2)
} else {
  print(x2)
}


x3=runif(1,0,10)
y=if(x2>3){
  10
}else{
  0
}

x3=1:100
#print all the even numbers from 1 to 100



for (i in 1:100 ){
  if ( x3[i]%%2 == 0) {
    print(x3[i])
  }
}
  
#print all the odd numbers from 1 to 100

for (i in 1:100){
  if ( i%%2==1){
    print(x[i])
  }
}
#print all the multiple of 5

for(i in 1:100){
  if(i%%5==0){
    print(x[i])
  }
}



#Find the sum : 1+2+3+...+100

S=0;
for (i in 1:100){
  S=S+i
}
S

#Find the product: 1*3*5*..*17*19
S1=1
for (i in 1:20) {
  if(i%%2==1){
  S1=S1*i
  }
}
S1






