
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

### Pranava Sai Maganti

#### FiveThirtyEight Statement

The statement I am analyzing is:

> If Iron Man fakes his death to chill out for a while but all his
> friends (and the reader) know he’s just kicking it somewhere, he
> didn’t die.

#### Include the code

``` r
# Define Iron Man's aliases
alias1 <- "Anthony Edward \"Tony\" Stark"
alias2 <- "Anthony Edward Stark"

# Filter for each alias separately
iron_man_data_alias1 <- av %>% filter(Name.Alias == alias1)
iron_man_data_alias2 <- av %>% filter(Name.Alias == alias2)

# Select death and return columns for each alias
iron_man_deaths_alias1 <- iron_man_data_alias1 %>% select(starts_with("Death"))
iron_man_returns_alias1 <- iron_man_data_alias1 %>% select(starts_with("Return"))

iron_man_deaths_alias2 <- iron_man_data_alias2 %>% select(starts_with("Death"))
iron_man_returns_alias2 <- iron_man_data_alias2 %>% select(starts_with("Return"))

# Count the number of deaths and returns for each alias
iron_man_death_count_alias1 <- sum(iron_man_deaths_alias1 == "YES", na.rm = TRUE)
iron_man_return_count_alias1 <- sum(iron_man_returns_alias1 == "YES", na.rm = TRUE)
iron_man_no_return_count_alias1 <- sum(
  (iron_man_deaths_alias1$Death1 == "YES" & iron_man_returns_alias1$Return1 == "NO") |
  (iron_man_deaths_alias1$Death2 == "YES" & iron_man_returns_alias1$Return2 == "NO") |
  (iron_man_deaths_alias1$Death3 == "YES" & iron_man_returns_alias1$Return3 == "NO") |
  (iron_man_deaths_alias1$Death4 == "YES" & iron_man_returns_alias1$Return4 == "NO") |
  (iron_man_deaths_alias1$Death5 == "YES" & iron_man_returns_alias1$Return5 == "NO"),
  na.rm = TRUE
)

iron_man_death_count_alias2 <- sum(iron_man_deaths_alias2 == "YES", na.rm = TRUE)
iron_man_return_count_alias2 <- sum(iron_man_returns_alias2 == "YES", na.rm = TRUE)
iron_man_no_return_count_alias2 <- sum(
  (iron_man_deaths_alias2$Death1 == "YES" & iron_man_returns_alias2$Return1 == "NO") |
  (iron_man_deaths_alias2$Death2 == "YES" & iron_man_returns_alias2$Return2 == "NO") |
  (iron_man_deaths_alias2$Death3 == "YES" & iron_man_returns_alias2$Return3 == "NO") |
  (iron_man_deaths_alias2$Death4 == "YES" & iron_man_returns_alias2$Return4 == "NO") |
  (iron_man_deaths_alias2$Death5 == "YES" & iron_man_returns_alias2$Return5 == "NO"),
  na.rm = TRUE
)

# Display the counts and detailed death/return records for each alias
results <- list(
  "Alias 1 - Anthony Edward 'Tony' Stark" = list(
    "Death Count" = iron_man_death_count_alias1,
    "Return Count (Returned After Death)" = iron_man_return_count_alias1,
    "No Return Count (Died Without Returning)" = iron_man_no_return_count_alias1,
    "Death Records" = iron_man_deaths_alias1,
    "Return Records" = iron_man_returns_alias1
  ),
  "Alias 2 - Anthony Edward Stark" = list(
    "Death Count" = iron_man_death_count_alias2,
    "Return Count (Returned After Death)" = iron_man_return_count_alias2,
    "No Return Count (Died Without Returning)" = iron_man_no_return_count_alias2,
    "Death Records" = iron_man_deaths_alias2,
    "Return Records" = iron_man_returns_alias2
  )
)

results
```

    ## $`Alias 1 - Anthony Edward 'Tony' Stark`
    ## $`Alias 1 - Anthony Edward 'Tony' Stark`$`Death Count`
    ## [1] 1
    ## 
    ## $`Alias 1 - Anthony Edward 'Tony' Stark`$`Return Count (Returned After Death)`
    ## [1] 1
    ## 
    ## $`Alias 1 - Anthony Edward 'Tony' Stark`$`No Return Count (Died Without Returning)`
    ## [1] 0
    ## 
    ## $`Alias 1 - Anthony Edward 'Tony' Stark`$`Death Records`
    ##   Death1 Death2 Death3 Death4 Death5
    ## 1    YES                            
    ## 
    ## $`Alias 1 - Anthony Edward 'Tony' Stark`$`Return Records`
    ##   Return1 Return2 Return3 Return4 Return5
    ## 1     YES                                
    ## 
    ## 
    ## $`Alias 2 - Anthony Edward Stark`
    ## $`Alias 2 - Anthony Edward Stark`$`Death Count`
    ## [1] 1
    ## 
    ## $`Alias 2 - Anthony Edward Stark`$`Return Count (Returned After Death)`
    ## [1] 0
    ## 
    ## $`Alias 2 - Anthony Edward Stark`$`No Return Count (Died Without Returning)`
    ## [1] 1
    ## 
    ## $`Alias 2 - Anthony Edward Stark`$`Death Records`
    ##   Death1 Death2 Death3 Death4 Death5
    ## 1    YES                            
    ## 
    ## $`Alias 2 - Anthony Edward Stark`$`Return Records`
    ##   Return1 Return2 Return3 Return4 Return5
    ## 1      NO

#### Include your answer

##### Analysis and Results

Based on the results, the analysis of Iron Man’s death and return
records across both aliases, “Anthony Edward ‘Tony’ Stark” and “Anthony
Edward Stark,” reveals distinct outcomes:

##### For “Anthony Edward ‘Tony’ Stark,” we observe:

- **Death Count**: 1 recorded death.
- **Return Count**: 1 recorded return, indicating that Iron Man returned
  after his recorded death.
- **No Return Count**: 0, meaning there were no cases where he died
  without a subsequent return.

##### For “Anthony Edward Stark,” we find:

- **Death Count**: 1 recorded death.
- **Return Count**: 0 recorded returns, signifying that he did not
  return after this death.
- **No Return Count**: 1, which confirms that there is a case where Iron
  Man died without a return.

##### Conclusion

These results support the statement that if Iron Man fakes his death but
everyone knows he’s still alive or he later returns, it’s not considered
a permanent death. For the alias “Anthony Edward ‘Tony’ Stark,” Iron Man
returned after his recorded death, aligning with this interpretation.
However, for the alias “Anthony Edward Stark,” he has one recorded death
without a return, indicating that in this case, his death was not staged
or temporary and could be seen as more definitive in the dataset’s
context.

### Manas Mathur

#### FiveThirtyEight Statement

> The quote I’m analyzing is: “Out of 173 listed Avengers, my analysis
> found that 69 had died at least one time after they joined the team.”

#### Include the code

``` r
av %>% 
  filter(Death1 == "YES") %>% 
  count()
```

    ##    n
    ## 1 69

#### Include your answer

I was able to conclude that the statement given that 69 avengers had
died at least one time is true. This was done by checking the count of
the value “Yes” in the ‘Death1’ column of av.

### Ananya Ramji

``` r
deaths
```

    ## # A tibble: 865 × 18
    ##    URL                Name.Alias Appearances Current. Gender Probationary.Introl
    ##    <chr>              <chr>            <int> <chr>    <chr>  <chr>              
    ##  1 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  2 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  3 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  4 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  5 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  6 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ##  7 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ##  8 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ##  9 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ## 10 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ## # ℹ 855 more rows
    ## # ℹ 12 more variables: Full.Reserve.Avengers.Intro <chr>, Year <int>,
    ## #   Years.since.joining <int>, Honorary <chr>, Return1 <chr>, Return2 <chr>,
    ## #   Return3 <chr>, Return4 <chr>, Return5 <chr>, Notes <chr>, Death <chr>,
    ## #   Time <dbl>

``` r
returns
```

    ## # A tibble: 865 × 18
    ##    URL                Name.Alias Appearances Current. Gender Probationary.Introl
    ##    <chr>              <chr>            <int> <chr>    <chr>  <chr>              
    ##  1 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  2 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  3 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  4 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  5 http://marvel.wik… "Henry Jo…        1269 YES      MALE   ""                 
    ##  6 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ##  7 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ##  8 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ##  9 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ## 10 http://marvel.wik… "Janet va…        1165 YES      FEMALE ""                 
    ## # ℹ 855 more rows
    ## # ℹ 12 more variables: Full.Reserve.Avengers.Intro <chr>, Year <int>,
    ## #   Years.since.joining <int>, Honorary <chr>, Death1 <chr>, Death2 <chr>,
    ## #   Death3 <chr>, Death4 <chr>, Death5 <chr>, Notes <chr>, Return <chr>,
    ## #   Time <dbl>

#### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check. There’s a 2-in-3
> chance that a member of the Avengers returned from their first stint
> in the afterlife, but only a 50 percent chance they recovered from a
> second or third death.

#### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

``` r
first_return <- deaths %>%
  filter(Death == "yes", Return1 == "YES", Time == 1)
first_did_not_return <- deaths %>%
  filter(Death == "yes", Return1 == "NO", Time == 1)

second_return <- deaths %>%
  filter(Death == "yes", Return2 == "YES", Time == 2)
second_did_not_return <- deaths %>%
  filter(Death == "yes", Return2 == "NO", Time == 2)

third_return <- deaths %>%
  filter(Death == "yes", Return3 == "YES", Time == 3)
third_did_not_return <- deaths %>%
  filter(Death == "yes", Return3 == "NO", Time == 3)

nrow(first_return)/(nrow(first_did_not_return)+nrow(first_return))
```

    ## [1] 0.6666667

``` r
nrow(second_return)/(nrow(second_did_not_return)+nrow(second_return))
```

    ## [1] 0.5

``` r
nrow(third_return)/(nrow(third_did_not_return)+nrow(third_return))
```

    ## [1] 0.5

#### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

After completing the analysis and calculating the percentages of
Avengers who returned after their first, second, and third death events,
I found the following results:

The percentage of Avengers who returned after their first death was 66%
or two-thirds. The percentage of Avengers who returned after their
second death was 50% or half. The percentage of Avengers who returned
after their third death was 50% or half.

## TEAM SUMMARY

Upload your changes to the repository. Discuss and refine answers as a
team. – HELP ME SUMMARIZE THIS OR IF YOU GUYS FINISH. JUST LET ME KNOW
AND I’LL JUST SUMMARIZE IT (NHAT LE)–
