library(tidyverse)
library(emmeans)
library(multcomp)

#read data
read_delim("wordle-oct23-2023b.csv") -> wordle
wordle %>% mutate(result=as.numeric(result)) -> wordle

# Identify initial guesses with frequency > 6
wordle %>% filter(!is.na(botresult)) %>% group_by(guess1) %>% 
summarize(mean(result),mean(botresult),count=n()) %>% 
filter(count>6) %>% arrange(-count) %>% select(1) -> topwords

wordle %>% filter(!is.na(botresult),guess1 %in% pull(topwords)) -> wordle.top


#...which are ...
wordle.top %>% select(guess1) %>% table 
wordle.top %>% select(guess1) %>% table %>% prop.table -> wordle.tab1
wordle.tab1[order(wordle.tab1,decreasing=TRUE)]

#fit model with two covariates
lm(result ~ guess1 + avg + botresult,data=wordle.top) -> wordle.fit3

#anova table
wordle.fit3 %>% anova

#summary
wordle.fit3 %>% summary

#means adjusted to avgs of botresult and (nyt)avg
wordle.fit3 %>% emmeans(~guess1) %>% as.data.frame %>% arrange(emmean)

#pairwise diff between "aisle" and "least"
diffLeast_Aisle <- matrix(c(0,0,-1,0,0,0,1,0,0,0,0,0,0),1)
glht(wordle.fit3,diffLeast_Aisle) -> diff3.out
diff3.out %>% summary

#raw means:
wordle.top %>% group_by(guess1) %>% 
summarize(mymean=mean(result), std=sd(result),mean(avg),mean(botresult),n=n()) %>% 
arrange(mymean,decreasing=TRUE)
