# Shreya Sinha
# 11/13/2024

rm(list = ls())
library(tidyverse)
library(janitor)
library(lubridate)

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
  mutate(dept = str_squish(dept)) %>% 
  mutate(total_students = nLetterStudents + nPNPStudents)
           

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

# join course_info with cleaned_course_grades
course_division <- cleaned_course_grades %>%
  left_join(course_info, by = c("course", "dept", "course_level")) 
 

# course average grades weighted by number of students by academic year

# average grade by college by year
college_avg_grade <- course_division %>%
  # find average GPA. percentage of A's, and total number of students enrolled for a course offered
  # by multiple instructors/quarters in an academic year
  summarize(course_avgGPA = weighted.mean(avgGPA, w = total_students, na.rm = T),
            yearly_course_enrollment = sum(total_students, na.rm = T),
            percentage_A = sum(total_As, na.rm = T) / sum(total_students, na.rm = T),
            .by = c(academic_year, course, dept, division, college))  %>%
  # find average GPA for each college per year
  summarize(college_GPA = weighted.mean(course_avgGPA, w = yearly_course_enrollment, na.rm = T),
            college_percentage_A = weighted.mean(percentage_A, w = yearly_course_enrollment, na.rm = T),
            .by = c(academic_year, college))

college_avg_plot <- college_avg_grade %>%
  # filtering out interdisciplinary/interdepartmental courses
  # filtering out college of creative studies because they do not have letter grades
  filter(!is.na(college), college != "College of Creative Studies") %>%
  ggplot(aes(x = academic_year, y = college_GPA, group = college)) +
  geom_point(aes(colour = college))+
  geom_line(aes(colour = college))+
  labs(title = "Average GPA by Academic Year for UCSB Colleges",
       x = "Academic Year",
       y = "Grade Point Average",
       colour = "College") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

print(college_avg_plot)

ggsave('college_avg_plot.pdf')
  
            
college_A_plot <- college_avg_grade %>%
  # filtering out interdisciplinary/interdepartmental courses
  # filtering out college of creative studies because they do not have letter grades
  filter(!is.na(college), college != "College of Creative Studies") %>%
  ggplot(aes(x = academic_year, y = college_percentage_A, group = college)) +
  geom_point(aes(colour = college))+
  geom_line(aes(colour = college))+
  labs(title = "Proportion of A's by Academic Year for UCSB Colleges",
       x = "Academic Year",
       y = "Proportion of A's",
       colour = "College") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))


print(college_A_plot)
ggsave('college_A_plot.pdf')


# average grade by division by year 

division_avg_grade <- course_division %>%
  # find average GPA and total number of students enrolled for a course offered
  # by multiple instructors/quarters in an academic year
  summarize(course_avgGPA = weighted.mean(avgGPA, w = total_students, na.rm = T),
            percentage_A = sum(total_As, na.rm = T) / sum(total_students, na.rm = T),
            yearly_course_enrollment = sum(total_students, na.rm = T),
            .by = c(academic_year, course, dept, division))  %>% 
  # find average GPA for each division per year
  summarize(division_GPA = weighted.mean(course_avgGPA, w = yearly_course_enrollment, na.rm = T),
            division_percentage_A = weighted.mean(percentage_A, w = yearly_course_enrollment, na.rm = T),
            .by = c(academic_year, division)) 
  
division_avg_plot <- division_avg_grade %>%
  # filtering to not include Creative Studies and Engineering because they are not part of Letters & Science
  filter(division != "Creative Studies", division != "Engineering") %>% 
  ggplot(aes(x = academic_year, y = division_GPA, group = division)) +
  geom_point(aes(colour = division))+
  geom_line(aes(colour = division))+
  labs(title = "Average GPA by Academic Year for Divisions of UCSB Letters & Science",
       x = "Academic Year",
       y = "Grade Point Average",
       colour = "Letters & Science Division") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))
  

print(division_avg_plot)
ggsave('division_avg_plot.pdf')

division_A_plot <- division_avg_grade %>%
  # filtering to not include Creative Studies and Engineering because they are not part of Letters & Science
  filter(division != "Creative Studies" & division != "Engineering") %>%
  ggplot(aes(x = academic_year, y = division_percentage_A, group = division)) +
  geom_point(aes(colour = division))+
  geom_line(aes(colour = division))+
  labs(title = "Proportion of A's by Academic Year for Divisions of UCSB Letters & Science",
       x = "Academic Year",
       y = "Proportion of A's",
       colour = "Letters & Science Division")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

  
print(division_A_plot)
ggsave('division_A_plot.pdf')
  

# average grade each year for econ department weighted by class size
econ_avg_grade <- cleaned_course_grades %>%
  filter(dept == "ECON") %>%
  # find average GPA, proportion of As, and total number of students enrolled for a course offered
  # by multiple instructors/quarters in an academic year
  summarize(course_avgGPA = weighted.mean(avgGPA, w = total_students, na.rm = T),
            percentage_A = sum(total_As, na.rm = T) / sum(total_students, na.rm = T),
         yearly_course_enrollment = sum(total_students, na.rm = T),
         .by = c(academic_year, course, course_level)) %>% 
  # find average GPA and percentage As for Econ department per year
  summarize(dept_avgGPA = weighted.mean(course_avgGPA, w = yearly_course_enrollment, na.rm = T),
            dept_percentage_A = weighted.mean(percentage_A, w = yearly_course_enrollment, na.rm = T),
        .by = c(academic_year, course_level))

econ_avg_plot <- econ_avg_grade %>%
  ggplot(aes(x = academic_year, y = dept_avgGPA, group = course_level)) +
  geom_point(aes(colour = course_level))+
  geom_line(aes(colour = course_level))+
  labs(title = "Average Course GPA by Academic Year for UCSB Economics Department",
       x = "Academic Year",
       y = "Course GPA",
       colour = "Course Level") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

print(econ_avg_plot)
ggsave('econ_avg_plot.pdf')

econ_A_plot <- econ_avg_grade %>%
  ggplot(aes(x = academic_year, y = dept_percentage_A, group = course_level)) +
  geom_point(aes(colour = course_level))+
  geom_line(aes(colour = course_level))+
  labs(title = "Proportion of A's by Academic Year for UCSB Economics Department",
       x = "Academic Year",
       y = "Proportion of A's",
       colour = "Course Level") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

print(econ_A_plot)
ggsave('econ_A_plot.pdf')



# average grade by year for online vs in-person class


# course grade percentage by academic year

  # summarize(percentage_A = sum(total_As, na.rm = T) / sum(nLetterStudents, na.rm = T),
  #           avg_GPA = mean(avgGPA, na.rm = T),
  #           .by = c("academic_year", "course","dept", "course_level")) %>%
  # summarize(percentage_A = mean(percentage_A, na.rm = T),
  #           avg_GPA = mean(avg_GPA, na.rm = T),
  #           .by = c("academic_year", "course_level"))

