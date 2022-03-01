#Module 5: Checking Hats Excercise
#Date: 5/1/2021
#Student: Elizabeth Elizondo
#Class: MIS 5330 780

#Modify the function scramble.hats() to compute the number of 
#correct matches in this setting. (The only change is 
#the definition of the vector hats; if one represents
# a black hat and a grey hat using a 1 and 2, respectively, 
#then hats consist of ten 1's and ten 2's.

#Answer:
scramble.hats = function(n){
  blk_hats = sample(c(1),size=10,replace = TRUE)
  grey_hats = sample(c(2),size=10,replace = TRUE)
  mixed.hats = blk_hats+grey_hats
}
sum(blk_hats&grey_hats == mixed.hats)

#Using the function replicate(), repeat this simulation for 
#1000 trials. Store the number of matches for the 1000
#experiments in the vector matches.

#Answer:
matches = replicate(1000, scramble.hats(20))
scramble.hats(30)

#From the simulated values, approximate the probability that 
#10 or more men receive the correct hats. Also, find the 
#expected number of correct matches. 
#Answer:
correct = sum(hats == mixed.hats)
correct

#Then, plot a histogram 
#of the non-parametric distribution of correct matches

png(filename = "Hat Matches.png")
hist(correct,xlab = "Hats",col = "green", border = "blue",xlim = c(0,25),ylim = c(0,10),breaks = 5)

