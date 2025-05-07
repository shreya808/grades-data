# Shreya Sinha
# 12/3/2024

rm(list = ls())
library(tidyverse)
library(janitor)
library(lubridate)
library(gt)
library(grid)
library(ggthemes)
library(data.table)

course_grades <- read_csv("courseGrades.csv")


# Clean data and categorize by academic year, department, whether it is
# upper division/lower division, course number, college and division
# GPA, percent of A's (plus and minuses), by year, department

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
      year == "2024" & quarter == "Summer" ~ "2023-2024",
    year == "2024" & quarter == "Fall" |
      year == "2025" & quarter == "Winter" |
      year == "2025" & quarter == "Spring" |
      year == "2025" & quarter == "Summer" ~ "2024-2025")) %>% 
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
  select(-A_plus, -A_minus, -A, -B_plus, -B_minus, -B, -C_plus, -C_minus, -C, -D_plus, -D_minus, -D, -`F`, -year) %>% 
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
  # if dept has "W" at the end of the string, it is designated as an online course
  # unique(course_grades$course[str_detect(course_grades$course, "\\s*\\w*W\\s*\\d+$")==TRUE])
  mutate(online = if_else(str_detect(course, "\\s*\\w*W\\s*\\d+"), 1, 0)) %>%
  # rename department codes for two word codes
  # regex finds matches from beginning of string in `course` column that
  # have 2 words separated by spaces like "[word] [word]"
  mutate(dept = if_else(str_detect(course, "^\\w+\\s+\\w+\\s"), 
                        str_extract(course, "^\\w+\\s+\\w+\\s"),
                        dept)) %>% 
  # remove "W" from end of dept
  mutate(dept = str_remove(dept, "W\\s*$")) %>% 
  mutate(dept = str_squish(dept),
         # astro classes are in physics department
         dept = case_match(
           dept, 
           c("ASTRO", "PHYS") ~ "PHYS",
           c("JAPAN", "KOR", "CHIN") ~ "EACS",
           c("HEB", "MES", "RG ST") ~ "RG ST",
           c("FR", "ITAL") ~ "FRIT",
           c("LATIN", "GREEK", "CLASS") ~ "CLASS",
           c("MUS", "MUS A") ~ "MUS",
           c("POL S", "POL SH") ~ "POL S",
           c("PORT", "SPAN") ~ "SPAN PORT",
           c("DANCE", "THTR") ~ "THTR DANCE",
           c("GER", "SLAV", "RUSS") ~ "GER", 
           .default = dept)) %>% 
  mutate(total_students = nLetterStudents + nPNPStudents) %>% 
  # remove satisfactory and unsatisfactory grade columns
  select(-S, -su)
           

# Make separate table with course code, division, department, and full course name

course_info <- cleaned_course_grades %>%
  select(course, dept, course_level) %>% 
  distinct() %>% 
  mutate(division = case_when(
    # humanities & fine arts departments from https://www.hfa.ucsb.edu/academics/#majors-section
    dept %in% c("ART", "ARTST", "CLASS", "C LIT", "EACS", "KOR", "JAPAN", "CHIN",
                "ENGL", "FAMST","FLMST", "ITAL", "FR", "FRIT", "GER", "SLAV", "RUSS", "HIST",
                "ARTHI", "LAIS", "LING", "LATIN","GREEK", "MUS", "MUS A", "PHIL", "RG ST", "HEB", "MES", "PORT", 
                "SPAN", "SPAN PORT", "DANCE", "THTR", "THTR DANCE", "WRIT", "ME ST") ~ "Humanities & Fine Arts",
    # social sciences departments from https://www.socialsciences.ucsb.edu/units
    dept %in% c("ANTH", "AS AM", "BL ST", "CH ST", "COMM", "ECON", "ES", "ESS",
                "FEMST", "GPS", "GLOBL", "MS", "POL S", "SOC", "LAWSO", "POL SH", "CNCSP") ~ "Social Sciences",
    # science departments from https://www.science.ucsb.edu/programs
    dept %in% c("CHEM", "EARTH", "GEOL", "EEMB", "GEOG", "MATH", "MCDB", "ASTRO", "PHYS",
                "PSY", "PSTAT", "ENV S", "SHS", "BMSE") ~ "Mathematical, Life, & Physical Sciences",
    # engineering departments from https://engineering.ucsb.edu/departments
    # there are no divisions in the college of engineering but for consistency
    # the division for engineering courses will be "Engineering"
    dept %in% c("CMPSC", "CH E", "BIOE", "ECE", "ENGR", "MATRL", "ME", "TMP") ~ "College of Engineering",
    # division for "INT" classes will be "interdisciplinary"
    dept %in% c("INT", "MAT") ~ "Interdisciplinary",
    # undergraduate classes in Gervitz School
    dept %in% c("ED", "CNCSP") ~ "Gervitz School of Education",
    # division for "CS" classes will be "Creative Studies" 
    # str_detect is finding dept where there is a space before "CS"; or dept is CMPSCCS or CMPTGCS or W&L (writing and literature)
    str_detect(dept, "\\w*\\s*CS$") |
      dept == "CMPSCCS" |
      dept == "CMPTGCS" |
      dept == "W&L" ~ "College of Creative Studies")) %>%
  mutate(
    college = case_match(
      division,
      "Gervitz School of Education" ~ "Gervitz School of Education",
      c("Humanities & Fine Arts",
        "Social Sciences",
        "Mathematical, Life, & Physical Sciences") ~ "College of Letters & Science",
      "College of Engineering" ~ "College of Engineering",
      "College of Creative Studies" ~ "College of Creative Studies",
      "Interdisciplinary" ~ "Interdisciplinary"
    )
  )

# join course_info with cleaned_course_grades
course_grade_info <- cleaned_course_grades %>%
  left_join(course_info, by = c("course", "dept", "course_level")) 
 

# course average grades weighted by number of students by academic year

# average grade by college by year
college_avg_grade <- course_grade_info %>%
  # find average GPA, percent of A's, and total number of students enrolled for a course offered
  # by multiple instructors/quarters in an academic year
  summarize(course_avgGPA = weighted.mean(avgGPA, w = total_students, na.rm = T),
            yearly_course_enrollment = sum(total_students, na.rm = T),
            course_As = sum(total_As, na.rm = T),
            .by = c(academic_year, course, dept, division, college)) %>% 
  # find weighted average GPA and percent of A's for each college per year
  summarize(college_GPA = weighted.mean(course_avgGPA, w = yearly_course_enrollment, na.rm = T),
            # sum of course enrollment for each year by college 
            college_course_enrollment = sum(yearly_course_enrollment, na.rm = T),
            college_percent_A = 100*sum(course_As, na.rm = T) / college_course_enrollment,
            .by = c(academic_year, college))


# average grade by division by year 

division_avg_grade <- course_grade_info %>%
  # find average GPA and total number of students enrolled for a course offered
  # by multiple instructors/quarters in an academic year
  summarize(course_avgGPA = weighted.mean(avgGPA, w = total_students, na.rm = T),
            yearly_course_enrollment = sum(total_students, na.rm = T),
            course_As = sum(total_As, na.rm = T),
            .by = c(academic_year, course, dept, division, college)) %>% 
  # find average GPA for each division per year
  summarize(division_GPA = weighted.mean(course_avgGPA, w = yearly_course_enrollment, na.rm = T),
            division_course_enrollment = sum(yearly_course_enrollment, na.rm = T),
            division_percent_A = 100 * sum(course_As, na.rm = T) / division_course_enrollment,
            .by = c(academic_year, division)) 

#joining division and college data
college_division_statistics <- division_avg_grade %>% 
  left_join(y = course_info %>% select(division, college) %>% distinct(),
            by = c("division"),
            relationship = "many-to-one") %>% 
  left_join(college_avg_grade,
            by = c("college", "academic_year"))
 

college_division_gpa_plot <- college_division_statistics %>% 
  # filtering out interdisciplinary/interdepartmental courses
  filter(!is.na(college) & !is.na(division),
         division != "College of Creative Studies",
         division != "Interdisciplinary") %>% 
  ggplot(aes(x = academic_year, group = division))+
  geom_line(aes(y = college_GPA, colour = college), linewidth = .8)+
  geom_point(aes(y = college_GPA, colour = college), size = 1)+
  geom_line(aes(y = division_GPA, colour = division), linewidth = .8)+
  geom_point(aes(y = division_GPA, colour = division), size = 1)+
  labs(title = "Average GPA Over Time for UCSB Undergraduate Colleges and Divisions",
       x = "Academic Year",
       y = "Grade Point Average (GPA)",
       colour = "College/Division")+
  theme_minimal()+
  theme(axis.text=element_text(size=12), #change font size of axis text
        axis.title=element_text(size=15, face = "bold"), #change font size of axis titles
        plot.title=element_text(size=17, face = "bold"), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=15, face = "bold"),#change font size of legend title   
        axis.text.x = element_text(angle = 60))+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))


print(college_division_gpa_plot)
ggsave('college_division_gpa_plot.png')

college_division_A_plot <- college_division_statistics %>% 
  filter(!is.na(college) & !is.na(division),
         division != "College of Creative Studies",
         division != "Interdisciplinary") %>% 
  ggplot(aes(x = academic_year, group = division))+
  geom_line(aes(y = college_percent_A, colour = college), linewidth = .8)+
  geom_point(aes(y = college_percent_A, colour = college), size = 1)+
  geom_line(aes(y = division_percent_A, colour = division), linewidth = .8)+
  geom_point(aes(y = division_percent_A, colour = division), size = 1)+
  labs(title = "Percentage of A's Over Time for UCSB Colleges and Divisions",
       x = "Academic Year",
       y = "Percentage of A's (%)",
       colour = "College/Division") +
  theme_minimal()+
  theme(axis.text=element_text(size=13), #change font size of axis text
        axis.title=element_text(size=15, face = "bold"), #change font size of axis titles
        plot.title=element_text(size=20, face = "bold"), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=15, face = "bold"),#change font size of legend title   
        axis.text.x = element_text(angle = 60))+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
print(college_division_A_plot)

ggsave('college_division_A_plot.png')

#percent of A's for each department in the most recent year
percent_23_24 <- course_grade_info %>%
  filter(academic_year == "2023-2024" & division != "College Creative Studies"
         & avgGPA > 0) %>% 
  summarize(dept_enrollment = sum(total_students, na.rm = T),
            dept_percent_A = sum(total_As, na.rm = T) / dept_enrollment,
            .by = c(dept, college)) %>% 
  select(-dept_enrollment) %>%
  adorn_pct_formatting(digits = 2) %>% 
  mutate(dept = tolower(dept),
         # names from https://catalog.ucsb.edu/departments
         dept = case_match(dept,
           "anth" ~ "Anthropology",
           "art" ~ "Art",
           "as am" ~ "Asian American Studies",
           "arthi" ~ "History of Art & Architecture",
           "bioe" ~ "Bioengineering",
           "bl st" ~ "Black Studies",
           "c lit" ~ "Comparative Literature",
           "ch e" ~ "Chemical Engineering",
           "ch st" ~ "Chicana & Chicano Studies",
           "chem" ~ "Chemistry & Biochemistry",
           "class" ~ "Classics",
           "comm" ~ "Communication",
           "cmpsc" ~ "Computer Science",
           "cncsp" ~ "Counseling, Clinical & School Psychology",
           "thtr dance" ~ "Theater & Dance",
           "eacs" ~ "East Asian Cultural Studies",
           "econ" ~ "Economics",
           "earth" ~ "Earth Science",
           "ece" ~ "Electrical & Computer Engineering",
           "ed" ~ "Education",
           "eemb" ~ "Ecology, Evolution & Marine Biology",
           "engl" ~ "English",
           "engr" ~ "Engineering Sciences",
           "env s" ~ "Environmental Studies",
           "ess" ~ "Exercise & Sport Studies",
           "famst" ~ "Film & Media Studies",
           "femst" ~ "Feminist Studies",
           "frit" ~ "French & Italian",
           "geog" ~ "Geography",
           "ger" ~ "Germanic & Slavic Studies",
           "globl" ~ "Global Studies",
           "greek" ~ "Classics",
           "hist" ~ "History",
           "int" ~ "Interdisciplinary Studies",
           "lais" ~ "Latin American & Iberian Studies",
           "latin" ~ "Classics",
           "ling" ~ "Linguistics",
           "mat" ~ "Media Arts & Technology",
           "math" ~ "Mathematics",
           "matrl" ~ "Materials",
           "mcdb" ~ "Molecular, Cellular & Developmental Biology",
           "me" ~ "Mechanical Engineering",
           "rg st" ~ "Religious Studies",
           "ms" ~ "Military Science",
           "mus" ~ "Music",
           "phil" ~ "Philosophy",
           "phys" ~ "Physics",
           "pol s" ~ "Political Science",
           "span port" ~ "Spanish & Portuguese",
           "psy" ~ "Psychological & Brain Sciences",
           "pstat" ~ "Statistics & Applied Probability",
           "soc" ~ "Sociology",
           "tmp" ~ "Technology Management",
           "writ" ~ "Writing Program",
           .default = dept)) %>% 
  arrange(college, dept_percent_A) %>% 
  rename("Percentage of A's" = dept_percent_A,
         "Department" = dept,
         "College" = college)
 
print(percent_23_24)
# gtsave(percent_23_24, "percent_23_24")

percent_24_25 <- course_grade_info %>%
  filter(academic_year == "2024-2025" & division != "College Creative Studies"
         & avgGPA > 0) %>% 
  summarize(dept_enrollment = sum(total_students, na.rm = T),
            dept_percent_A = sum(total_As, na.rm = T) / dept_enrollment,
            .by = c(dept, college)) %>% 
  select(-dept_enrollment) %>%
  adorn_pct_formatting(digits = 2) %>% 
  mutate(dept = tolower(dept),
         # names from https://catalog.ucsb.edu/departments
         dept = case_match(dept,
                           "anth" ~ "Anthropology",
                           "art" ~ "Art",
                           "as am" ~ "Asian American Studies",
                           "arthi" ~ "History of Art & Architecture",
                           "bioe" ~ "Bioengineering",
                           "bl st" ~ "Black Studies",
                           "c lit" ~ "Comparative Literature",
                           "ch e" ~ "Chemical Engineering",
                           "ch st" ~ "Chicana & Chicano Studies",
                           "chem" ~ "Chemistry & Biochemistry",
                           "class" ~ "Classics",
                           "comm" ~ "Communication",
                           "cmpsc" ~ "Computer Science",
                           "cncsp" ~ "Counseling, Clinical & School Psychology",
                           "thtr dance" ~ "Theater & Dance",
                           "eacs" ~ "East Asian Cultural Studies",
                           "econ" ~ "Economics",
                           "earth" ~ "Earth Science",
                           "ece" ~ "Electrical & Computer Engineering",
                           "ed" ~ "Education",
                           "eemb" ~ "Ecology, Evolution & Marine Biology",
                           "engl" ~ "English",
                           "engr" ~ "Engineering Sciences",
                           "env s" ~ "Environmental Studies",
                           "ess" ~ "Exercise & Sport Studies",
                           "famst" ~ "Film & Media Studies",
                           "femst" ~ "Feminist Studies",
                           "frit" ~ "French & Italian",
                           "geog" ~ "Geography",
                           "ger" ~ "Germanic & Slavic Studies",
                           "globl" ~ "Global Studies",
                           "greek" ~ "Classics",
                           "hist" ~ "History",
                           "int" ~ "Interdisciplinary Studies",
                           "lais" ~ "Latin American & Iberian Studies",
                           "latin" ~ "Classics",
                           "ling" ~ "Linguistics",
                           "mat" ~ "Media Arts & Technology",
                           "math" ~ "Mathematics",
                           "matrl" ~ "Materials",
                           "mcdb" ~ "Molecular, Cellular & Developmental Biology",
                           "me" ~ "Mechanical Engineering",
                           "rg st" ~ "Religious Studies",
                           "ms" ~ "Military Science",
                           "mus" ~ "Music",
                           "phil" ~ "Philosophy",
                           "phys" ~ "Physics",
                           "pol s" ~ "Political Science",
                           "span port" ~ "Spanish & Portuguese",
                           "psy" ~ "Psychological & Brain Sciences",
                           "pstat" ~ "Statistics & Applied Probability",
                           "soc" ~ "Sociology",
                           "tmp" ~ "Technology Management",
                           "writ" ~ "Writing Program",
                           .default = dept)) %>% 
  arrange(college, dept_percent_A) %>% 
  rename("Percentage of A's" = dept_percent_A,
         "Department" = dept,
         "College" = college)

# Split data  and recombine -----------------------------------------------

# filtering for only letters and science
percent_ls <- percent_23_24 %>% 
  filter(College == "College of Letters & Science") |>
  select(Department, `Percentage of A's`) |>
  as.data.frame()


percent_ls1 <- percent_ls[1:20,]

percent_ls2 <- percent_ls[21:40,]


# Rename to remove duplicate names ----------------------------------------

percent_ls_full <- data.frame(percent_ls1, percent_ls2)
names(percent_ls_full) <- c("a", "b", "aa", "bb")

# Generate Table & Relabel----------------------------------------------------------

percent_ls_full_pretty <- percent_ls_full |>
  gt() |>
  tab_header(title = md("Percentage of A's in 2023-2024"),
             subtitle = md("Data from Fall 2023 to Spring 2024")) |>
  cols_label(a = "Department",
             b = "Percentage of A's",
             aa = "Department",
             bb = "Percentage of A's")|>
  tab_spanner(label = md("College of Letters & Science"), columns = c(a, b,aa,bb)) |>
  tab_options(heading.title.font.size = 17, heading.subtitle.font.size = 14, table.font.size = 14,
              table.border.top.color = "white")
percent_ls_full_pretty

percent_rest <- percent_23_24 |> 
  filter(College != "College of Letters & Science") |>
  as.data.frame()|>
  gt(groupname_col = "College", row_group_as_column = TRUE)|>
  opt_row_striping(row_striping = TRUE)|>
  tab_options(row.striping.include_stub = FALSE, table.font.size = 14)

  
  
percent_rest

percent_full <- gt_group(percent_ls_full_pretty, percent_rest)

percent_full

# average grade each year for econ department weighted by class size
econ_avg_grade <- cleaned_course_grades %>%
  filter(dept == "ECON") %>%
  # find average GPA, percent of As, and total number of students enrolled for a course offered
  # by multiple instructors/quarters in an academic year
  summarize(course_avgGPA = weighted.mean(avgGPA, w = total_students, na.rm = T),
            course_As = sum(total_As, na.rm = T),
            yearly_course_enrollment = sum(total_students, na.rm = T),
            .by = c(academic_year, course, course_level)) %>% 
  # find average GPA and percent As for Econ department per year
  summarize(dept_avgGPA = weighted.mean(course_avgGPA, w = yearly_course_enrollment, na.rm = T),
            dept_course_enrollment = sum(yearly_course_enrollment, na.rm = T),
            dept_percent_A = 100* sum(course_As, na.rm = T) / dept_course_enrollment,
        .by = c(academic_year, course_level))

econ_avg_plot <- econ_avg_grade %>%
  ggplot(aes(x = academic_year, y = dept_avgGPA, group = course_level)) +
  geom_point(aes(colour = course_level))+
  geom_line(aes(colour = course_level), linewidth = .8)+
  labs(title = "Average Course GPA by Academic Year for UCSB Economics Department",
       x = "Academic Year",
       y = "Course GPA",
       colour = "Course Level") +
  theme_minimal()+
  theme(axis.text=element_text(size=13), #change font size of axis text
        axis.title=element_text(size=15, face = "bold"), #change font size of axis titles
        plot.title=element_text(size=18, face = "bold"), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=13, face = "bold"),#change font size of legend title   
        axis.text.x = element_text(angle = 45))+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))

print(econ_avg_plot)
ggsave('econ_avg_plot.png')


# econ avg grade table by course
econ_avg_grade_table <- cleaned_course_grades %>%
  filter(dept == "ECON") %>%
  # find average GPA, percent of As, and total number of students enrolled for a course offered
  # by multiple instructors/quarters in an academic year
  summarize(course_avgGPA = weighted.mean(avgGPA, w = total_students, na.rm = T),
            course_As = sum(total_As, na.rm = T),
            yearly_course_enrollment = sum(total_students, na.rm = T),
            .by = c(academic_year, course, course_level)) %>% 
  # remove honors classes
  filter(!str_detect(course, "H$") & course_avgGPA != 0) %>% 
  # find average GPA and percent As for Econ department per year
  summarize(course_avgGPA = weighted.mean(course_avgGPA, w = yearly_course_enrollment, na.rm = T),
            course_percent_A = 100* sum(course_As, na.rm = T) / sum(yearly_course_enrollment, na.rm = T),
            .by = c(course)) 


econ_A_plot <- econ_avg_grade %>%
  ggplot(aes(x = academic_year, y = dept_percent_A, group = course_level)) +
  geom_point(aes(colour = course_level))+
  geom_line(aes(colour = course_level), linewidth = .8)+
  labs(title = "Percentage of A's by Academic Year for UCSB Economics Department",
       x = "Academic Year",
       y = "Percentage of A's (%)",
       colour = "Course Level") +
  theme_minimal()+
  theme(axis.text=element_text(size=13), #change font size of axis text
        axis.title=element_text(size=15, face = "bold"), #change font size of axis titles
        plot.title=element_text(size=18, face = "bold"), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=13, face = "bold"),#change font size of legend title   
        axis.text.x = element_text(angle = 45))+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))

      

print(econ_A_plot)
ggsave('econ_A_plot.png')


# write report on average gpa and percent of A’s for colleges and divisions
# 
# fix graduate classes for clinical psychology to be in social sciences
# 
# include two graphs (average gpa and percent of As)
# 
# and one table (percent of a’s for each department in the most recent year)
# 
# look at percent of A’s in econ 10a and econ 5 (prereqs for transfers) and see if lower division gpa requirement has changed over time
# 

# average grade by year for online vs in-person class

