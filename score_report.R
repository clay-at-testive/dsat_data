source('~/Testive/db_testive/basic_tools.R')

rw_floor = function(integer) {
  if (integer == 0) {
    floor = 200
  } else if (integer > 15) {          #HighScoring
    floor = 10 * round(17.5774 + integer * 1.4229)
  } else                       #LowScoring
    floor = 10 * round(27.7817 + integer * 0.6419)
  return(floor)
}

rw_ceiling = function(integer) {
  if (integer == 27) {          #HighScoring
    ceiling = 800
  } else if (integer > 15) {
    ceiling = 10 * round(17.5774 + integer * 1.4229 + 27 * 0.7887)
  } else                        #LowScoring
    ceiling = 10 * round(27.7817 + integer * 0.6419 + 27 * 0.6727)
  return(ceiling)
}

math_floor = function(integer) {
  if (integer == 0) {
    floor = 200
  } else if (integer > 11) {          #HighScoring
    floor = 10 * round(29.312 + integer * 1.1438)
  } else                              #LowScoring
    floor = 10 * round(22.2509 + integer * 0.7549)
  return(floor)
}

math_ceiling = function(integer) {
  if (integer == 22) {          
    ceiling = 800
  } else if (integer > 11) {          #HighScoring 
    ceiling = 10 *  round(29.312 + integer * 1.1438 + 22 * 1.1865)
  } else                        #LowScoring
    ceiling = 10 * round(22.2509 + integer * 0.7549 + 22 * 0.9324)
  return(ceiling)
}

test = read.csv('~/Desktop/sample_test.csv') %>%
  mutate(isCorrect = case_when(isCorrect == 'true' ~ '1',
                               TRUE ~ '0'))
test$isCorrect = as.numeric(test$isCorrect)

section_scores = test %>%
  filter(nonEmpty(score)) %>%
  group_by(section) %>%
  summarize(score = max(score))

rw_mod_1 = test %>% 
  filter(index == 1)
rw_mod_1_correct = sum(rw_mod_1$isCorrect)
rw_mod_2 = test %>%
  filter(index == 2)
math_mod_1 = test %>%
  filter(index == 4)
math_mod_1_correct = sum(math_mod_1$isCorrect)
math_mod_2 = test %>%
  filter(index == 5)

rw_bucket = rw_mod_1$scoringCategory[1]
math_bucket = math_mod_1$scoringCategory[1]

subsection_questions = test %>%
  group_by(name) %>%
  summarize(n = n())

subsection_scores = test %>%
  group_by(name) %>%
  summarize(isCorrect = sum(isCorrect))

subsection_scores = full_join(subsection_scores, subsection_questions) %>%
  mutate(Incorrect = n - isCorrect) %>%
  rename(Correct = isCorrect) %>%
  select(-n)
