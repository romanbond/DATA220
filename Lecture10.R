x <- c("a", "b", "c", "d")
for(i in 1:4) {
   #Print out each element of 'x'
     print(x[i])
}









x <- c("a", "b", "c", "d")
 for(i in seq_along(x)) {
   print(x[i])
   }










x <- c("a", "b", "c", "d")
 for(letter in x) {
   print(letter)
   }





#Nested for LOOPs


x <- matrix(1:6, 2, 3)


for (i in 1:nrow(x)){
  for (j in 1:ncol(x)){
    print(x[i,j])
  }
}

x <- matrix(1:9, 3,3)
for ( i in seq_len(nrow(x))) {
  for ( j in seq_len(ncol(x))) {
    if(x[i,j]%%2==1){
      print(x[i,j])
    }
  }
}


s=0
for( i in seq_len(nrow(x))){
  for(j in seq_len(ncol(x))){
    if(x[i,j]%%2==0){
      s=s+x[i,j]
    }
  }
}
s


s=0
for(i in x){
  if (i%%2==0){
    s=s+i
  }
}
s






for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }
}

#Q1: print all the even numbers of matrix x
#Q2: Print all the odd numbers of matrix x
#Q3: Find the sum of all numbers in matrix x


#while LOOPS


 count <- 0
while(count < 10) {
   print(count)
   count <- count + 1
   }


#random walk using while LOOP
 z <- 5
 set.seed(1)
count=1;
 while(z >= 3 && z <= 10) {
     coin <- rbinom(1, 1, 0.5)
     count=count+1
    
       if(coin == 1) { ## random walk
         z <- z + 1
        } else {
           z <- z - 1
           }
     }
print(z)
print(count)


#random walk using while LOOP
z <- 5
set.seed(1)
count=1;
while( count<=10) {
  coin <- rbinom(1, 1, 0.5)
  count=count+1
  
  if(coin == 1) { ## random walk
    z <- z + 1
  } else {
    z <- z - 1
  }
}
print(z)
print(count)

s=0
x=1
while(x<=100){
  s=s+x
  x=x+1
}
s

s=1
count=2
while(count<=100){
  if(count%%2==1){
    s=s+count
    count=count+1
  }
  else{
    count=count+1
  }
}
s




#next, break

for(i in 1:100) {
  if(i <= 90) {
    ## Skip the first 20 iterations
    next
  }
  ## Do something here
  print(i)
}


#break


for(i in 1:100) {
  print(i)
  if(i > 20) {
    ## Stop loop after 20 iterations
    break
  }
}