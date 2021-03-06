#####################################
################RVEST################
#####################################
library(rvest)
page <- read_html('https://ege.hse.ru/rating/2019/81031971/all/?rlist=&ptype=0&vuz-abiturients-budget-order=ge&vuz-abiturients-budget-val=10')


#xpth = '/html/body/div/div/div[2]/div[2]/div/div/ul[1]/li[5]/ul/li[1]'

links <- page %>%
  html_nodes(xpath = '/html/body/div/div/div[2]/div[2]/div/div/ul[1]') %>%
  html_nodes('li') %>% 
  html_nodes('ul') %>% 
  html_nodes('li') %>%
  html_nodes('a') %>% 
  html_attr('href')


df <- page %>% html_table()
df <- as.data.frame(df[[2]])

l <- list()
ii <- 1
for (i in links[1:5]){
  if (!startsWith(i, 'https://')){
    url <- paste0('https://ege.hse.ru', i)
    page <- read_html(url)
    df <- page %>% html_table()
    l[[ii]] <- as.data.frame(df[[2]])
    ii <- ii + 1
  } else { 
    page <- read_html(i)
    df <- page %>% html_table()
    l[[ii]] <- as.data.frame(df[[2]])
    ii <- ii + 1
    }
}

fin <- dplyr::bind_rows(l)

names(fin)
str(fin)
dim(fin)
###################################
############SELENIUM###############
###################################


library("RSelenium")

driver<- rsDriver()
remDr <- driver[["client"]]

remDr$browserName <- 'firefox'
remDr$open()
remDr$navigate('https://www.dndbeyond.com/monsters?')

#a <- remDr$findElement(using="css selector", '.div.b-pagination:nth-child(1)>ul:nth-child(1)>li:nth-child(2)>a:nth-child(1)')

a <- remDr$findElement(using='xpath', '/html/body/div[1]/div/div[3]/div/section/div/div[3]/div/ul/li[2]/a')

a$clickElement()

a$getElementText()
