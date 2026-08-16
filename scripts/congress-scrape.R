# Ages of members of the 119th Congress (2025-2026)
# The U.S. Constitution sets the minimum age requirements to serve in the U.S. Senate at 30 years of age and in the U.S. House of Representatives at 25 years of age. However, it does not set a maximum age limit to serve in either chamber.

# As of Jan. 3, 2025:
# The median age of a Democratic U.S. senator was 64, and a Republican U.S. senator was 64. The median age of an independent U.S. senator was 81.5.
# The median age of a Democratic U.S. representative was 57, and a Republican U.S. representative was 57.
# A plurality of U.S. senators — 33.3% — were in their sixties, and a plurality of U.S. representatives — 26.5% — were in their fifties.
# A plurality of U.S. senators — 60.6% — were baby boomers, and a plurality U.S. representatives — 41.7% — were part of Generation X. 



library(httr)
library(yaml)
library(tidyverse)

# Pull current legislators from unitedstates/congress-legislators (community-maintained)
resp <- GET("https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-current.yaml")
stop_for_status(resp)

legislators_raw <- yaml.load(content(resp, as = "text", encoding = "UTF-8"))

congress_ages <- map_dfr(legislators_raw, function(x) {
  last_term <- tail(x$terms, 1)[[1]]
  tibble(
    first_name = x$name$first,
    last_name  = x$name$last,
    birthday   = x$bio$birthday,
    gender     = x$bio$gender,
    party      = last_term$party,
    chamber    = last_term$type,
    state      = last_term$state
  )
}) |>
  mutate(
    birthday = as.Date(birthday),
    age      = as.integer(Sys.Date() - birthday) %/% 365L,
    chamber  = if_else(chamber == "sen", "Senate", "House")
  )



congress_ages = congress_ages |>
  mutate(
    birthyear = year(birthday),
    generation = case_when(
      birthyear >= 1928 & birthyear <= 1945 ~ "Silent Generation",
      birthyear >= 1946 & birthyear <= 1964 ~ "Baby Boomer",
      birthyear >= 1965 & birthyear <= 1980 ~ "Generation X",
      birthyear >= 1981 & birthyear <= 1996 ~ "Millennial",
      birthyear >= 1997 & birthyear <= 2009 ~ "Generation Z",
      birthyear >= 2010  ~ "Generation Alpha",
    )
  ) |>
  print(n = Inf)



# Silent Generation: Individuals born from 1928 to 1945.
# Baby Boomers: Individuals born from 1946 to 1964.
# Generation X: Individuals born from 1965 to 1980.
# Millennials: Individuals born from 1981 to 1996.
# Generation Z: Individuals born from 1997 to 2012.


congress_ages |>
  write_csv("data/congress.csv")



#####################################
# Check
#####################################

census = read_csv("data/census-age-2025.csv")
congress = read_csv("data/congress.csv")

congress2 = read_csv("data/congress-by-generation.csv")


# Count and percent
congress |>
  group_by(chamber, generation) |>
  summarize(
    N = n()
  ) |>
  mutate(
    Perc = if_else(chamber == "House", N/436, N/100)
  )



ggplot(data = census, aes(x = age, fill = generation)) +
  geom_bar(aes(y = percent), stat = "identity") +
  # geom_col(aes(y = count)) +
  geom_bar(data = congress_ages, aes(y = after_stat(count / sum(count)*100)), fill = "yellow")

ggplot(data = congress_ages, aes(x = age)) +
  geom_histogram() 

ggplot(data = congress_ages, aes(x = generation)) +
  geom_bar() 



ggplot(data = census, aes(x = age, y = sum(count_millions), fill = generation)) +
  geom_bar(stat = "identity", position = "stack") 


census_gen = census |>
  group_by(generation) |>
  summarize(
    count_millions = sum(count_millions)
  )

ggplot(data = census_gen, aes(x = count_millions, y = generation,  fill = generation)) +
  geom_bar(stat = "identity", position = "stack") 



ggplot(data = congress, aes(x = age, fill = party)) +
  geom_histogram(color= "black") +
  facet_wrap(~party, ncol = 1) 

# census-age-2025.csv# census-age-2025.countcsv
# Total N = 341784857


congress2$generation = factor(congress2$generation, levels = c("Generation Alpha", "Generation Z", "Millennial", "Generation X", "Baby Boomer", "Silent Generation"))

ggplot(data = congress2, aes(x = percent, y = group, fill = fct_rev(generation))) +
  geom_bar(stat = "identity", position = "stack")

congress$generation = factor(congress$generation, levels = c("Generation Alpha", "Generation Z", "Millennial", "Generation X", "Baby Boomer", "Silent Generation"))
ggplot(data = congress, aes(y = party, fill = generation)) +
  geom_bar(position = position_fill(reverse = TRUE))

ggplot(data = congress, aes(y = chamber, fill = generation)) +
  geom_bar(position = position_fill(reverse = TRUE))



ggplot(data = congress, aes(x = age)) +
  geom_histogram(color= "black") +
  facet_grid(party ~ chamber) 


ggplot(data = congress, aes(x = age, fill = party)) +
  geom_histogram(color= "black") +
  facet_wrap(~ chamber) 


congress |>
  group_by(chamber, party) |>
  summarize(
    M = mean(age)
  )


