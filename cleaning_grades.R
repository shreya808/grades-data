# Shreya Sinha
# 11/01/2024


# Make separate table with course code, division, department, and full course
# name

rm(list = ls())
library(tidyverse)
library(janitor)

course_grades <- read_csv("courseGrades.csv")


# Clean data and categorize by academic year, department, whether it is
# upper division/lower division, course number, college and division
# GPA, percentage of A's (plus and minuses), by year, department

cleaned_course_grades <- course_grades %>% mutate(
  #create column for school year based on quarter + year
  academic_year = case_when( 
    year == "2009" & quarter == "Fall" |
      year == "2010" & quarter == "Winter" |
      year == "2010" & quarter == "Spring" |
      year == "2010" & quarter == "Summer" ~ "2009-2010",
    year == "2010" & quarter == "Fall" |
      year == "2011" & quarter == "Winter" |
      year == "2011" & quarter == "Spring" |
      year == "2011" & quarter == "Summer" ~ "2010-2011",
    year == "2011" & quarter == "Fall" |
      year == "2012" & quarter == "Winter" |
      year == "2012" & quarter == "Spring" |
      year == "2012" & quarter == "Summer" ~ "2011-2012",
    year == "2012" & quarter == "Fall" |
      year == "2013" & quarter == "Winter" |
      year == "2013" & quarter == "Spring" |
      year == "2013" & quarter == "Summer" ~ "2012-2013",
    year == "2013" & quarter == "Fall" |
      year == "2014" & quarter == "Winter" |
      year == "2014" & quarter == "Spring" |
      year == "2014" & quarter == "Summer" ~ "2013-2014",
    year == "2014" & quarter == "Fall" |
      year == "2015" & quarter == "Winter" |
      year == "2015" & quarter == "Spring" |
      year == "2015" & quarter == "Summer" ~ "2014-2015",
    year == "2015" & quarter == "Fall" |
      year == "2016" & quarter == "Winter" |
      year == "2016" & quarter == "Spring" |
      year == "2016" & quarter == "Summer" ~ "2015-2016",
    year == "2016" & quarter == "Fall" |
      year == "2017" & quarter == "Winter" |
      year == "2017" & quarter == "Spring" |
      year == "2017" & quarter == "Summer" ~ "2016-2017",
    year == "2017" & quarter == "Fall" |
      year == "2018" & quarter == "Winter" |
      year == "2018" & quarter == "Spring" |
      year == "2018" & quarter == "Summer" ~ "2017-2018",
    year == "2018" & quarter == "Fall" |
      year == "2019" & quarter == "Winter" |
      year == "2019" & quarter == "Spring" |
      year == "2019" & quarter == "Summer" ~ "2018-2019",
    year == "2019" & quarter == "Fall" |
      year == "2020" & quarter == "Winter" |
      year == "2020" & quarter == "Spring" |
      year == "2020" & quarter == "Summer" ~ "2019-2020",
    year == "2020" & quarter == "Fall" |
      year == "2021" & quarter == "Winter" |
      year == "2021" & quarter == "Spring" |
      year == "2021" & quarter == "Summer" ~ "2020-2021",
    year == "2021" & quarter == "Fall" |
      year == "2022" & quarter == "Winter" |
      year == "2022" & quarter == "Spring" |
      year == "2022" & quarter == "Summer" ~ "2021-2022",
    year == "2022" & quarter == "Fall" |
      year == "2023" & quarter == "Winter" |
      year == "2023" & quarter == "Spring" |
      year == "2023" & quarter == "Summer" ~ "2022-2023",
    year == "2023" & quarter == "Fall" |
      year == "2024" & quarter == "Winter" |
      year == "2024" & quarter == "Spring" |
      year == "2024" & quarter == "Summer" ~ "2023-2024")) %>% 
  rename("A_plus" = "Ap",
         "A_minus" = "Am",
         "B_plus" = "Bp",
         "B_minus" = "Bm",
         "C_plus" = "Cp",
         "C_minus" = "Cm",
         "D_plus" = "Dp",
         "D_minus" = "Dm") %>% 
  mutate(total_As = A_plus + A_minus + A,
         total_Bs = B_plus + B_minus + B,
         total_Cs = C_plus + C_minus + C,
         total_Ds = D_plus + D_minus + D,
         total_Fs = F) %>% 
  select(-A_plus, -A_minus, -A, -B_plus, -B_minus, -B, -C_plus, -C_minus, -C, -D_plus, -D_minus, -D, -F, -year) %>% 
  #create column for course number
  mutate(course_number = parse_number(course)) %>% 
  relocate(course_number, .after = "course") %>% 
  # remove graduate classes (i.e. course number >= 200)
  filter(course_number < 200) %>% 
  # categorize division (i.e. if course is < 100 it is lower division, if course is >= 100 it is upper division)
  mutate(division = case_when(
    course_number < 100 ~ "lower",
    course_number >= 100 ~ "upper"))

# course grade percentage by instructor and quarter
grade_percentage_instructor_quarter <- cleaned_course_grades %>% 
  summarize(percentage_A = total_As / nLetterStudents,
            .by = c("academic_year", "course", "quarter", "instructor", "dept", "avgGPA", "division"))

# course grade percentage by academic year
grade_percentage_academic_year <- cleaned_course_grades %>% 
  summarize(percentage_A = sum(total_As, na.rm = T) / sum(nLetterStudents, na.rm = T),
            avg_GPA = mean(avgGPA, na.rm = T),
            .by = c("academic_year", "course","dept", "division"))
