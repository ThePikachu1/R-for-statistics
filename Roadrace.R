library(pacman)

pacman::p_load(rio)


roadrace <- import("/Users/fathimafaridamohammad/Downloads/roadrace.csv")

head(roadrace)

barplot(roadrace$Maine) # doesnt work by itself 

#We create a table with count of the column named Maine
MaineOrNot <- table(roadrace$Maine)
head(MaineOrNot)
prop.table(MaineOrNot)

# We create a barplot of the table Maine and lable it using the function text
bp<- barplot(MaineOrNot, xlab ="RunnerOrigin", ylab="Number of Runners")
text(bp, MaineOrNot, labels = MaineOrNot, pos=1)

#For solving b we install the package dplyr 
pacman::p_load(dplyr)

#Histogram for times from Away
h<- hist(roadrace$`Time (minutes)` [roadrace$Maine =="Maine"] , 
         breaks = 7, main ="Histogram for runners’ times from Maine" , 
         xlab = "" , col = "pink" )
# text function helps me label the histogram
text(h$mids, h$counts, labels = h$counts, pos = 3)

# Histogram for times from Maine
h<- hist(roadrace$`Time (minutes)` [roadrace$Maine =="Away"] , 
         breaks = 7, main ="Histogram for runners’ times from Away" , 
         xlab = "" , col = "yellow" )
# text function helps me label the histogram
text(h$mids, h$counts, labels = h$counts, pos = 3)

# We filter for runners from Maine using filter function. 
Maine_Runners <- filter(roadrace, Maine== "Maine")
#Summary() helps us generate summary
summary(Maine_Runners$`Time (minutes)`)
# sd() and range() help calculate standard deviation and range.
sd(Maine_Runners$`Time (minutes)`)
range(Maine_Runners$`Time (minutes)`)

# We filter for runners from Away using filter() function 
Away_Runners <- filter(roadrace, Maine=="Away")
# summary() helps us generate summary
summary(Away_Runners$`Time (minutes)`)
# sd() and range() help calculate standard deviation and range.
sd(Away_Runners$`Time (minutes)`)
range(Away_Runners$`Time (minutes)`)


#For sidebyside boxplot we dont need to split the dataframes into 2 
# Boxplot for runners' time in minutes 
boxplot(`Time (minutes)` ~ Maine,
        data = roadrace,
        col = c("lightblue", "lightgreen"),
        main = "Race Time Comparison",
        ylab = "Time (minutes)")

# Boxplot for ages 
boxplot(`Age` ~ Sex,
        data = roadrace,
        col = c("pink", "yellow"),
        main = "Age Comparison",
        ylab = "Age")


