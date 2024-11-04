# Shreya Sinha
# 11/01/2024

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
  # column for quarter and year
  mutate(quarter_year = paste(quarter, year, sep = " ")) %>% 
# remove plus and minus grade columns and year column
  select(-A_plus, -A_minus, -A, -B_plus, -B_minus, -B, -C_plus, -C_minus, -C, -D_plus, -D_minus, -D, -F, -year) %>% 
  #create column for course number
  mutate(course_number = parse_number(course)) %>% 
  relocate(course_number, .after = "course") %>% 
  # remove graduate classes (i.e. course number >= 200)
  filter(course_number < 200) %>% 
  # categorize division (i.e. if course is < 100 it is lower division, if course is >= 100 it is upper division)
  mutate(course_level = case_when(
    course_number < 100 ~ "lower division",
    course_number >= 100 ~ "upper division")) %>% 
  # remove extra spaces in course code
  mutate(course = str_squish(course)) %>% 
  # rename department codes for two word codes
  # regex finds matches from beginning of string in `course` column that
  # have 2 words separated by spaces like "[word] [word]"
  mutate(dept = if_else(str_detect(course, "^\\w+\\s+\\w+\\s"), 
                        str_extract(course, "^\\w+\\s+\\w+\\s"),
                        dept)) %>% 
  # if dept has "W" at the end of the string, it is designated as an online course
  # unique(course_grades$course[str_detect(course_grades$course, "\\s*\\w*W\\s*\\d+$")==TRUE])
  mutate(online = if_else(str_detect(dept, "\\s*\\w*W\\s*$"), 1, 0)) %>%
  # remove "W" from end of dept
  mutate(dept = str_remove(dept, "W\\s*$")) %>% 
  mutate(dept = str_squish(dept))
           
        

# course grade percentage by instructor and quarter
grade_percentage_instructor_quarter <- cleaned_course_grades %>% 
  summarize(percentage_A = sum(total_As, na.rm = T) / sum(nLetterStudents, na.rm = T), 
            .by = c("quarter_year", "academic_year", "course", "instructor", "dept", "avgGPA", "course_level", "quarter"))

# course grade percentage by academic year
grade_percentage_academic_year <- cleaned_course_grades %>% 
  summarize(percentage_A = sum(total_As, na.rm = T) / sum(nLetterStudents, na.rm = T),
            avg_GPA = mean(avgGPA, na.rm = T),
            .by = c("academic_year", "course","dept", "course_level"))

# Make separate table with course code, division, department, and full course name

course_info <- cleaned_course_grades %>%
  select(course, dept, course_level) %>% 
  distinct() %>% 
  mutate(division = case_when(
    # humanities and fine arts departments from https://www.hfa.ucsb.edu/academics/#majors-section
    dept %in% c("ART", "ARTST", "CLASS", "C LIT", "EACS", "KOR", "JAPAN", "CHIN",
                "ENGL", "FAMST","FLMST", "ITAL", "FR", "GER", "SLAV", "RUSS", "HIST",
                "ARTHI", "LAIS", "LING", "LATIN","GREEK", "MUS", "MUS A", "PHIL", "RG ST", "HEB", "MES", "PORT", 
                "SPAN", "DANCE", "THTR", "WRIT", "ME ST") ~ "Humanities and Fine Arts",
    #social sciences departments from https://www.socialsciences.ucsb.edu/units
    dept %in% c("ANTH", "AS AM", "BL ST", "CH ST", "COMM", "ECON", "ES", "ESS",
                "FEMST", "GPS", "GLOBL", "MS", "POL S", "SOC", "LAWSO", "POL SH") ~ "Social Sciences",
    # science departments from https://www.science.ucsb.edu/programs
    dept %in% c("CHEM", "EARTH", "GEOL", "EEMB", "GEOG", "MATH", "MCDB", "ASTRO", "PHYS",
                "PSY", "PSTAT", "ENV S", "SHS", "BMSE") ~ "Mathematical, Life, and Physical Sciences",
    # engineering departments from https://engineering.ucsb.edu/departments
    # there are no divisions in the college of engineering but for consistency
    # the division for engineering courses will be "Engineering"
    dept %in% c("CMPSC", "CH E", "BIOE", "ECE", "ENGR", "MATRL", "ME", "TMP") ~ "Engineering",
    # division for "INT" classes will be "interdisciplinary"
    dept %in% c("INT", "MAT") ~ "Interdisciplinary",
    # division for "CS" classes will be "Creative Studies" 
    # str_detect is finding dept where there is a space before "CS"; or dept is CMPSCCS or CMPTGCS or W&L (writing and literature)
    str_detect(dept, "\\sCS") | dept == "CMPSCCS" | dept == "CMPTGCS" | dept == "W&L" ~ "Creative Studies",
    # division for education or CNSCP will be "Graduate" because it is in the Gervitz Graduate School of Education
    dept %in% c("ED", "CNCSP") ~ "Graduate")) %>% 
  mutate(college = case_when(
    division == "Humanities and Fine Arts" ~ "College of Letters & Science",
    division == "Social Sciences" ~ "College of Letters & Science",
    division == "Mathematical, Life, and Physical Sciences" ~ "College of Letters & Science",
    division == "Engineering" ~ "College of Engineering",
    division == "Creative Studies" ~ "College of Creative Studies",
    division == "Graduate" ~ "Gervitz Graduate School of Education"))

