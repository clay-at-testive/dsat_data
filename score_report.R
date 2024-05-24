source('~/Testive/db_testive/basic_tools.R')
library(glue)

rw_floor = function(integer) {
  if (integer == 0) {
    floor = 200
  } else if (integer > 15) {    #HighScoring
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
  } else                              #LowScoring
    ceiling = 10 * round(22.2509 + integer * 0.7549 + 22 * 0.9324)
  return(ceiling)
}

question_info = read.csv('~/Desktop/question_info.csv') %>%
  rename(questionId = id, tag = name)

test = read.csv('~/Desktop/sample_test_2.csv') %>%
  mutate(isCorrect = case_when(isCorrect == 'true' ~ '1',
                               TRUE ~ '0'))
test$isCorrect = as.numeric(test$isCorrect)

test = left_join(test, question_info, by = 'questionId')

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

rw_low = rw_floor(rw_mod_1_correct)
rw_high = rw_ceiling(rw_mod_1_correct)
math_low = math_floor(math_mod_1_correct)
math_high = math_ceiling(math_mod_1_correct)

rw_bucket_range = data.frame(position =
                               c('bottom', 'range', 'top'),
                             value = c(rw_low,
                                       (rw_high - rw_low),
                                       (800 - rw_high)),
                             x = c(0.5, 0.5, 0.5))

math_bucket_range = data.frame(position =
                               c('bottom', 'range', 'top'),
                             value = c(math_low,
                                       (math_high - math_low),
                                       (800 - math_high)),
                             x = c(0.5, 0.5, 0.5))

rw_subsection = test %>%
  filter(section == 'ReadAndWrite') %>%
  group_by(name) %>%
  summarize(Correct = sum(isCorrect), Total = n()) %>%
  mutate(PercentCorrect = percent(Correct/Total))

math_subsection = test %>%
  filter(section == 'Math') %>%
  group_by(name) %>%
  summarize(Correct = sum(isCorrect), Total = n()) %>%
  mutate(PercentCorrect = percent(Correct/Total))

## Create graphics ##

rw_score = section_scores %>%
  filter(section == 'ReadAndWrite')
rw_score_data = data.frame(category = c('score', 'max_score'),
                             value = c(rw_score$score, 800))
rw_score_data$ymax = rw_score_data$value/800
rw_score_data$ymin = c(0, head(rw_score_data$ymax, n = -1))

rw_circle = ggplot(rw_score_data,
                     aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3,
                     fill = category)) +
  geom_rect() + coord_polar(theta = 'y') +
  xlim(c(2, 4)) +
  theme_void() +
  scale_fill_manual(values = c('grey', '#b9770e')) +
  annotate('text', x = 2, y = 0, label = rw_score$score, size = 25) +
  theme(legend.position = 'none')

math_score = section_scores %>%
  filter(section == 'Math')
math_score_data = data.frame(category = c('score', 'max_score'),
                             value = c(math_score$score, 800))
math_score_data$ymax = math_score_data$value/800
math_score_data$ymin = c(0, head(math_score_data$ymax, n = -1))

math_circle = ggplot(math_score_data,
                     aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3,
                     fill = category)) +
  geom_rect() + coord_polar(theta = 'y') +
  xlim(c(2, 4)) +
  theme_void() +
  scale_fill_manual(values = c('grey', '#076fa2')) +
  annotate('text', x = 2, y = 0, label = math_score$score, size = 25) +
  theme(legend.position = 'none')

composite_score = data.frame(section = 'Composite',
                             score = sum(section_scores$score))
composite_score_data = data.frame(category = c('score', 'max_score'),
                             value = c(composite_score$score, 1600))
composite_score_data$ymax = composite_score_data$value/1600
composite_score_data$ymin = c(0, head(composite_score_data$ymax, n = -1))

composite_circle = ggplot(composite_score_data,
                     aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3,
                     fill = category)) +
  geom_rect() + coord_polar(theta = 'y') +
  xlim(c(2, 4)) +
  theme_void() +
  scale_fill_manual(values = c('grey', 'green4')) +
  annotate('text', x = 2, y = 0, label = composite_score$score, size = 25) +
  theme(legend.position = 'none')

rw_subsection_graph = ggplot(rw_subsection) +
  geom_col(aes(name, PercentCorrect), fill = '#b9770e', width = 0.6) +
  coord_flip()

math_subsection_graph = ggplot(math_subsection) +
  geom_col(aes(name, PercentCorrect), fill = '#076fa2', width = 0.6) +
  coord_flip()

rw_line_data <- data.frame(
    x = 0.45, 
    xend = 0.55, 
    y = rw_score$score,
    yend = rw_score$score
)

math_line_data <- data.frame(
    x = 0.45, 
    xend = 0.55, 
    y = math_score$score,
    yend = math_score$score
)

rw_score_range = ggplot(rw_bucket_range, aes(fill = position, y = value,
                                                    x = x)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity',
           width = 0.1) +
  scale_fill_manual(values = c('gray', '#b9770e', 'gray')) +
  geom_segment(data = rw_line_data, aes(x =  x, xend = xend, y = y,
                                        yend = yend),
                   color = "black", linewidth = 1, inherit.aes = FALSE) +
  coord_cartesian(ylim = c(200, 800)) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(legend.position = 'none') +
  annotate('text', x = 0.32, y = (rw_high - 30), size = 5,
           label = glue('Your range: {rw_low}-{rw_high}')) +
  annotate('text', x = 0.76, y = rw_score$score, size = 5,
           label = glue('Your final Reading/Writing score: {rw_score$score}'))

math_score_range = ggplot(math_bucket_range, aes(fill = position, y = value,
                                             x = x)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity',
           width = 0.1) +
  scale_fill_manual(values = c('gray', '#076fa2', 'gray')) +
  geom_segment(data = math_line_data, aes(x = x, xend = xend, y = y,
                                          yend = yend),
               color = 'black', linewidth = 1, inherit.aes = FALSE) +
  coord_cartesian(ylim = c(200, 800)) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(legend.position = 'none') +
  annotate('text', x = 0.32, y = (math_high - 30), size = 5,
           label = glue('Your range: {math_low}-{math_high}')) +
  annotate('text', x = 0.7, y = math_score$score, size = 5,
           label = glue('Your final Math score: {math_score$score}'))

# Need to create correct_color and tag_color columns
# Probably need to hack geom_bar into geom_rect to get correct width of bar

ggplot(mod1, aes(y = order, fill = tag_color)) +
  geom_bar(position = 'identity', width = 1)
+ scale_y_reverse() + coord_cartesian(xlim = c(0.25, 0.75)) + scale_x_discrete()

# Create matix for math section showing technique questions and
# correct/incorrect. See geom_tile