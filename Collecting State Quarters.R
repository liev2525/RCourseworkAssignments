#Assignment:Module 6 Assignment
#Student:Elizabeth Elizondo
#Class:MIS 5330 780
#Date: 5/4/2021

#(1)Write a function using the sample() function to simulate the 
#purchase of 100.
#Answer:
usstates<-c(state.name)
usstates
set.seed(100)
state.quarter=sample(usstates,size = 100,replace = TRUE)
state.quarter

#(2) Using the replicate() function, repeat this process for 
#1000 purchases. Construct a table of the number of unique quarters
#you obtain in these 1000 simulations. Use this table to estimate the
#probability that you obtain at least 45 unique quarters.
#Answer: 
collector=function(n,m){
  state.quarter = sample(1:n, size=m, replace=TRUE)
  ifelse(length(unique(state.quarter)) == n, "yes", "no")
}
collector(45, 100)
table(replicate(1000, collector(45, 100)))
#Answer Result:
#no yes 
#996   4 

#(3)Use the output from 2) to find the expected number of unique quarters.
#Answer
unique(table(replicate(1000, collector(45, 100))))
#[1] 998   2


#(4)Suppose you are able to complete your quarter set by purchasing 
#state quarters from a coin shop for $2 for each quarter. Revise your 
#function to compute the total (random) cost of completing the quarter
#set. Using the replicate() function, repeat the quarter-purchasing 
#process 1000 times and compute the expected cost of completing your set.
collect2 = function(n.purchased){
  cost.rquarter = 2.00
  cost.nquarter = 2.00
  samp.quarter = sample(1:100, size=n.purchased, replace=TRUE)
  n.quarter = length(unique(samp.quarter))
  n.missed = 100 - n.quarter
  n.purchased * cost.rquarter + n.missed * cost.nquarter
}
costs = replicate(1000, collect2(100))
summary(costs)
