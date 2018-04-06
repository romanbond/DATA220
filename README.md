# DATA_220L_112_18S
Data Analysis 220 working files and notes along with completed labs and exams.


## File Directory

19. 4/5/18 - Lecture20_LogisticRegression_Titanic_s18.R
    1. Using training and test data to predict outcomes and measuring their accuracy.
20. 4/6/18 - Lecture20_LogisticRegression_Titanic_s18_Updated.R
    1. Using training and test data to predict outcomes in a large data set.

## Lecture Code
<details><summary>Lecture 20</summary>
<p>

#### Lecture 20 

```r
require("Hmisc")
require("rms")
#install.packages("ggplot2")
library(ggplot2)
require(ggplot2)

getHdata(titanic3)
head(titanic3)

#replacing na in age with mean
#find na values in age
index=which(is.na(titanic3$age),arr.ind=TRUE)
titanic3$age[index]=mean(titanic3$age,na.rm=TRUE)

#changing embarked to numerical values
##southampton == 1
##cherbourg == 2
##queenstown == 3

titanic3$embarked1= NA
titanic3$embarked1[titanic3$embarked %in% "Southampton"] = 1
titanic3$embarked1[titanic3$embarked %in% "Cherbourg"] = 2
titanic3$embarked1[titanic3$embarked %in% "Queenstown"] = 3
titanic3$embarked1=as.factor(titanic3$embarked1)

#remove rows with na from embarked, there are only 2
index1=which(is.na(titanic3$embarked1),arr.ind=TRUE)
titanic<-titanic3[-c(index1),]
attach(titanic)
```

</p>
</details>



Directory Template
~~~
L#. x/x/x  Lecture#_File_Name_Here_s18.R
    1. Discription about lecture and file here.
~~~
