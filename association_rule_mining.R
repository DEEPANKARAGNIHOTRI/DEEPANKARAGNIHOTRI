install.packages("arules",dependencies = TRUE)
install.packages("arulesViz",dependencies = TRUE)

library(arules)
library(arulesViz)

setwd("C:/R")

list.files()

##read the file Transaction.csv
transData<-read.csv("Transaction.csv")
transData


class(transData) ##this is a dataframe

?apriori

#transData<-discretize(transData) #run only if you have problems with the next line

myTransaction<-as(transData,"transactions") #convert the dataframe into a transaction object as the apriori 
                                            # algorithm will only accept transaction objects

class(myTransaction) ## transaction object from the arules package

myTransaction@data ## displays the data in sparse format. It is a transpose of the way we have our data in the 
                  ## Transaction.csv file. Therefore it displays the data as 13 rows and 2928 columns

myTransaction@itemInfo ## displays the column headers in our data

myTransaction@itemInfo[,1]


class(myTransaction@data) ##ngCMatrix
class(myTransaction@itemInfo) ## Data frame

myTransaction@data[,1] ##Display the first column which is actually the first row in our Excel file

myTransaction@data[1,] ## Displays the first row which is actually the first column in our Excel file

myTransaction@itemInfo 

myTransaction@itemInfo[,"labels"] # Displays the labels

myTransaction@itemInfo[myTransaction@data[,1],"labels"] #We now look at the data in the first column from
                                                        #myTransaction@data and the labels column of myTransaction@itemInfo

#write a loop to print the first 20 columns of the matrix
for (i in 1:20){
  print(myTransaction@itemInfo[myTransaction@data[,i],"labels"])
}
paste(myTransaction@itemInfo[myTransaction@data[,i],"labels"],collapse = ",")
#################################Home-work##################################
#after the class
#change the above loop with apply function
#replace the space between the items by a comma

#############################################################################


#apriori algo. Look at the itemsets of only item, min suport is 0.002
itemSets<-apriori(myTransaction, parameter = list(minlen = 1,
                                                  maxlen = 1,
                                                  support = 0.002,
                                                  target = "frequent itemsets"))


inspect(sort(itemSets,by="support")) #Use the inspect function to look at itemSets after sorting by support

#displays itemsets with one item and two items
itemSets<-apriori(myTransaction, parameter = list(minlen = 1,
                                                  maxlen = 2,
                                                  support = 0.002,
                                                  target = "frequent itemsets"))
inspect(sort(itemSets,by="support"))


#rules. look at rules for at least 1 item and at most 3 item itemsets
#with min. support 0.002 and confidence 0.1

myRules<-apriori(myTransaction,parameter = list(minlen = 1,
                                                maxlen = 3,
                                                support = 0.002,
                                                confidence = 0.1,
                                                target = "rules"))

inspect(head(sort(myRules,by="lift"),10)) ##sort the rules by lift



##Load the Groceries data
data("Groceries")

Groceries

class(Groceries)

Groceries@data
Groceries@itemInfo

Groceries@itemInfo[Groceries@data[,1],"labels"]

paste(Groceries@itemInfo[Groceries@data[,1],"labels"],collapse=",")


for (i in 1:20){
  print(paste(Groceries@itemInfo[Groceries@data[,i],"labels"],collapse=","))
}

apply(Groceries@data[,1:20],2,function(r) paste(
                            Groceries@itemInfo[r,"labels"],collapse=","))

#Write a function called aprioriFunction which will do the above

aprioriFunction<-function(r){
  paste(Groceries@itemInfo[r,"labels"],collapse=",")
}

apply(Groceries@data[,1:20],2,aprioriFunction)

###########Item Frequency Plot################
itemFrequencyPlot(Groceries,topN=20,type = "absolute")
itemFrequencyPlot(Groceries,topN=20,type = "relative")


######Check support of itemsets min 1 item, max 2 items
itemSets<-apriori(Groceries,parameter = list(minlen = 1,
                                             maxlen = 2,
                                             support = 0.002,
                                             target = "frequent itemsets"))

inspect(head(sort(itemSets,by="support"),10))


######Check support of itemsets of itemsets with exactly 3 items
itemSets<-apriori(Groceries,parameter = list(minlen = 3,
                                             maxlen = 3,
                                             support = 0.002,
                                             target = "frequent itemsets"))

inspect(head(sort(itemSets,by="support"),10))

#############Check support for itemsets with at least 2 itema
itemSets<-apriori(Groceries,parameter = list(minlen = 2,
                                             support = 0.002,
                                             target = "frequent itemsets"))

inspect(head(sort(itemSets,by="support"),10))


#Create some rules
#Determine rules, with support = 0.002, confidence = 0.2
#Display the rules in decreasing order of lift

myRules<-apriori(Groceries,parameter = list(support = 0.002,
                                            confidence = 0.2,
                                            target = "rules"))

inspect(head(sort(myRules,by="lift"),10)) 



#Determine rules, with support = 0.002, confidence = 0.6
#Display the rules in decreasing order of lift
myRules<-apriori(Groceries,parameter = list(support = 0.002,
                                            confidence = 0.6,
                                            target = "rules"))

inspect(head(sort(myRules,by="lift"),10)) 


myRules<-apriori(Groceries,parameter = list(support = 0.002,
                                            confidence = 0.6,
                                            maxlen = 3,
                                            target = "rules"))

inspect(head(sort(myRules,by="lift"),10)) 

#What are customers likely to buy before buying whole milk?
#What are customers likely to buy after buying whole milk?

myRules<-apriori(Groceries,parameter = list(support = 0.002,
                                            confidence = 0.6,
                                            target = "rules"),
                 appearance = list(default="lhs",rhs="whole milk"))

inspect(head(sort(myRules,by="lift"),10))

myRules<-apriori(Groceries,parameter = list(support = 0.002,
                                            confidence = 0.2,
                                            target = "rules"),
                 appearance = list(default="rhs",lhs="whole milk"))

inspect(head(sort(myRules,by="lift"),10))

#Plot the rules
myRules<-apriori(Groceries,parameter = list(support = 0.001,
                                            confidence = 0.6,
                                            maxlen = 4,
                                            target = "rules"))

#please load arulesViz - library(arulesViz) before this
plot(myRules) 

plot(myRules@quality)


highLiftRules<-head(sort(myRules,by="lift"),5) #create a set of top 5 high lift rules
inspect(highLiftRules)

plot(highLiftRules,method="graph") #depict the rules as a graph.

