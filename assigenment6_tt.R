library(dplyr)
library(tidyr)
library(readr)
#导入数据库

scientist <- read_csv("scientist.csv")
inst_wos_dict <- read_csv("inst_wos_dict.csv")
cddt_paper <- read_csv("cddt_paper.csv")
cite_all <- read_csv("cite.csv")
#读取数据


library(stringr)
scientist <- scientist %>% mutate(inst = str_to_lower(inst))
inst_wos_dict <- inst_wos_dict %>% mutate(inst = str_to_lower(inst))
cite_all <- cite_all %>% rename(citing_pid = `citing_pid`, cited_pid = `cited_pid`)
#数据预处理

inst_map <- function(cv, inst_dict, db = "wos") {
  if (db == "wos") {
    cv <- cv %>% inner_join(inst_dict %>% select(inst, wos) %>% distinct(), by = "inst") %>% 
      select(wos, startyear, endyear) %>% 
      distinct()
  } else if (db == "scopus") {
    cv <- cv %>% inner_join(inst_dict %>% select(inst, scopus) %>% distinct(), by = "inst") %>% 
      select(scopus, startyear, endyear) %>% 
      distinct()
  } else if (db == "openalex") {
    cv <- cv %>% inner_join(inst_dict %>% select(inst, openalex) %>% distinct(), by = "inst") %>% 
      select(openalex, startyear, endyear) %>% 
      distinct()
  } else {
    print("The database is not supported by now, please contact the authors on Github.")
    return(NULL)
  }
  
  colnames(cv) <- c("inst", "startyear", "endyear")
  return(cv)
}
#定义函数inst_map函数

cv_filter <- function(paper, cv, year_lag = 2) {
  paper <- paper %>% mutate(aff = str_to_lower(aff), pub_year = as.numeric(pub_year))
  cv <- cv %>% mutate(inst = str_to_lower(inst), startyear = as.numeric(startyear), endyear = as.numeric(endyear))
  
  result <- merge(paper %>% mutate(key = 1), cv %>% mutate(key = 1), by = "key") %>% 
    filter(pub_year >= startyear, pub_year <= endyear + year_lag, str_detect(aff, regex(inst, ignore_case = TRUE))) %>% 
    select(pid) %>% 
    distinct()
  
  return(result$pid)
}
#定义函数cv_filter

cite_glue <- function(pid, cite) {
  pid_1 <- unique(cite %>% filter(citing_pid %in% pid) %>% select(cited_pid) %>% pull())
  pid_2 <- unique(cite %>% filter(cited_pid %in% pid) %>% select(citing_pid) %>% pull())
  
  pid_add <- unique(c(pid_1, pid_2))
  pid_add <- setdiff(pid_add, pid)
  
  return(pid_add)
}
#定义cite_glue函数

cv_disam <- function(paper, cv, year_lag1 = 2, year_lag2 = 2, cite) {
  paper_1 <- filter(paper, initials == 0)
  paper_2 <- filter(paper, initials == 1)
  
  pid_stage1 <- cv_filter(paper_1, cv, year_lag1)
  
  cite <- filter(cite, citing_pid %in% pid_stage1 | cited_pid %in% pid_stage1 | 
                   citing_pid %in% paper_2$pid | cited_pid %in% paper_2$pid)
  
  pid_core <- pid_stage1
  
  repeat {
    pid_add <- cite_glue(pid_core, cite)
    if (length(pid_add) == 0) {
      break
    } else {
      pid_core <- unique(c(pid_core, pid_add))
    }
  }
  
  pid_stage2 <- setdiff(pid_core, pid_stage1)
  paper_3 <- filter(paper_2, pid %in% pid_stage2)
  pid_stage3 <- cv_filter(paper_3, cv, year_lag2)
  
  pid_disam <- unique(c(pid_stage1, pid_stage3))
  
  return(pid_disam)
}
#cv_disam函数转换

# 示例：读取数据
scientist <- read_csv("scientist.csv")
inst_wos_dict <- read_csv("inst_wos_dict.csv")
cddt_paper <- read_csv("cddt_paper.csv")
cite_all <- read_csv("cite.csv")

# 示例：调用函数并展示部分结果
# 注意：这里仅为示例，具体参数和函数调用需要根据实际情况调整
pid_disam_example <- cv_disam(paper, cv, 2, 2, cite)
print(head(pid_disam_example))
#数据展示


