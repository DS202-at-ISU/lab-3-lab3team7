
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(tidyr)
```

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

``` r
# Transform Death columns to long form
deaths <- av %>%
  pivot_longer(cols = starts_with("Death"), 
               names_to = "Death_Time", 
               values_to = "Death") %>%
  mutate(Time = parse_number(Death_Time),
         Death = tolower(Death)) %>%
  select(-Death_Time)

# Transform Return columns to long form
returns <- av %>%
  pivot_longer(cols = starts_with("Return"), 
               names_to = "Return_Time", 
               values_to = "Return") %>%
  mutate(Time = parse_number(Return_Time),
         Return = tolower(Return)) %>%
  select(-Return_Time)
```

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

Similarly, deal with the returns of characters.

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
# Calculate the average number of deaths per Avenger
# We understand that the code below only considers the avengers who have at least 1 death. We provide the second code that consider all avengers right below this code.
average_deaths <- deaths %>%
  filter(Death == "yes") %>%
  group_by(Name.Alias) %>%
  summarize(total_deaths = n()) %>%
  summarize(avg_deaths = mean(total_deaths, na.rm = TRUE))
```

``` r
# Calculate the total number of deaths for each Avenger, including those with zero deaths
average_deaths <- deaths %>%
  mutate(Death = ifelse(Death == "yes", 1, 0)) %>%  # Convert "yes" to 1 and everything else to 0
  group_by(Name.Alias) %>%
  summarize(total_deaths = sum(Death, na.rm = TRUE)) %>%
  summarize(avg_deaths = mean(total_deaths, na.rm = TRUE))

# Output the result
average_deaths
```

    ## # A tibble: 1 × 1
    ##   avg_deaths
    ##        <dbl>
    ## 1      0.546

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### Nhat Le

#### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check. My statement: “If
> a character is killed but then secretly hidden away in a stasis tube,
> they died.”

#### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

``` r
# Check for characters who have both a death and a return, implying a "hidden away" scenario
hidden_deaths <- deaths %>%
  filter(Death == "yes") %>%
  inner_join(returns %>% filter(Return == "yes"), by = c("Name.Alias", "Time")) %>%
  select(Name.Alias, Time, Death, Return)
```

    ## Warning in inner_join(., returns %>% filter(Return == "yes"), by = c("Name.Alias", : Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 48 of `x` matches multiple rows in `y`.
    ## ℹ Row 40 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Display the first few rows to see examples of "hidden away" deaths
head(hidden_deaths)
```

    ## # A tibble: 6 × 4
    ##   Name.Alias                       Time Death Return
    ##   <chr>                           <dbl> <chr> <chr> 
    ## 1 "Janet van Dyne"                    1 yes   yes   
    ## 2 "Anthony Edward \"Tony\" Stark"     1 yes   yes   
    ## 3 "Robert Bruce Banner"               1 yes   yes   
    ## 4 "Thor Odinson"                      1 yes   yes   
    ## 5 "Steven Rogers"                     1 yes   yes   
    ## 6 "Clinton Francis Barton"            1 yes   yes

``` r
# Count the number of such scenarios
num_hidden_deaths <- hidden_deaths %>%
  nrow()
num_hidden_deaths
```

    ## [1] 67

#### Include your answer

For this statement, we’ll interpret “killed” as a record of “yes” in the
Death column in our dataset. However, since the concept of “hidden away
in a stasis tube” implies they returned without a true resurrection,
we’ll look for cases where a character has a recorded death followed by
a recorded return in the Return column, regardless of how that return is
defined.

According to the dataset, there are 67 instances where a character is
recorded as having died and later returned. This supports the notion
that even if a character was “hidden away” or returned through other
means, their initial recorded “death” is considered legitimate in the
dataset. This aligns with the statement from FiveThirtyEight that such
characters are indeed counted as having “died,” even if they were later
revived or returned from stasis.

### Manas Mathur

#### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

#### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

#### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

### Anaya Ramji

#### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

#### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

#### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

### Pranava Sai Maganti

#### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

#### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

#### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

## TEAM SUMMARY

Upload your changes to the repository. Discuss and refine answers as a
team. – HELP ME SUMMARIZE THIS OR IF YOU GUYS FINISH. JUST LET ME KNOW
AND I’LL JUST SUMMARIZE IT (NHAT LE)–
