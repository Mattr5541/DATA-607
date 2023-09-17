library(tibble)
library(knitr)
library(tidyverse)
library(dplyr)
library(ggplot2)


congage <- read.csv("data_aging_congress.csv")

congage_short <- congage %>% select(congress, chamber, start_date,
                                    age_years, birthday, generation,
                                    state_abbrev, cmltv_cong)


congage_short <- congage_short %>% rename(cumulative_congress_terms = 
                                            cmltv_cong)



###Idea: Test ages of the current Congress by state to determine which states elect the oldest representatives; I can use generation to make this easier, possibly###

congcurr <- congage_short %>% subset(congress == 118)

congcurr_count <- congcurr %>% group_by(state_abbrev, generation, age_years) %>% summarize(freq = n())

##congcurr_mean <- congcurr %>% summarize(gen_by_age = median(age_years)) 

ggplot(congcurr_count, aes(x = state_abbrev, y = freq, fill = generation)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Generation by State", x = "State", y = "Frequency")
sum(congcurr_count$freq)

save(congcurr, file = "congcurr.RData")
save(congage, file = "congage.RDATA")
save(congage_short, file = "congage_short.RDATA")
save(congcurr_count, file = "congcurr_count.RDATA")

############################################################################IGNORE THIS FOR NOW###########################################################
congage_short_mod <- congage_short %>% group_by(generation, state_abbrev) %>% summarize(age_by_gen = median(age_years))

congage_short_mean <- congage_short %>% group_by(state_abbrev, generation) %>% summarize(age_total = sum(age_years))

ggplot(congage_short_mod, aes(x = state_abbrev, y = age_by_gen, fill = generation)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Age by State", x = "State", y = "Age") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(congcurr_count, aes(x = state_abbrev, y = age_years, color = generation, group=generation)) +
  geom_line() +
  theme_minimal()

congage_short %>% summarize(min = min(age_years), max  = max(age_years))
#########################################################################################################################################################