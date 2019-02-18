#### @@@@------------------------------------
#### 
#### Topic---------------------------------
#### *Function-PTT爬蟲
#### 
#### 
#### Version---------------------------------
#### *2017/05/20 10:15
#### First
#### *2017/05/09 11:05
#### 
#### 
#### Author----------------------------------
#### *黃瑋 (Huang, Wei)
#### Contact
#### *E-mail:f0958425316@gmail.com
#### 
#### @@@@------------------------------------



#### 控制列----##############################################

### 控制列: 目標設定----
# search_board <- "Key_Mou_Pad"
# search_range <- 2
# search_keyword <- c("南港區|南港", "信義區|信義")




####   抓取並直接下載至指定資料夾----
ptt_crawler_download <- function(search_board, 
                                 search_range = 10, 
                                 search_keyword = NA, 
                                 path, 
                                 time_break = c(10,  5), 
                                 showProgress = TRUE, 
                                 proxy
){
      
      #### 環境設置----##############################################
      
      
      #### 環境設置: library----
      # library(XML)
      # library(RCurl)
      # library(jsonlite)
      require(data.table)
      require(tidyverse)
      require(lubridate)
      require(stringr)
      require(rvest)
      require(httr)
      
      
      
      #### 路徑整理----##############################################
      path_board <- paste0(path, search_board, "/")
      dir.create(path_board, showWarnings = FALSE)
      dir.create(paste0(path_board, "KEYWORD/"), showWarnings = FALSE)
      for(key_word_time_i in search_keyword){
            dir.create(paste0(path_board, "KEYWORD/", 
                              str_split(key_word_time_i, "\\|")[[1]][1], "/"
            ), 
            showWarnings = FALSE)
      }
      
      
      
      #### 爬蟲主體----##############################################
      
      ### 爬蟲主體: 頁面&文章代碼----
      page_time_start <- Sys.time()
      
      url <- paste0("https://www.ptt.cc/bbs/", search_board, "/index%s.html")
      if(missing(proxy)){
            last_page <- 
                  html_session(sprintf(url,  ""), use_cookies)
      }else{
            last_page <- 
                  html_session(sprintf(url,  ""), use_cookies, 
                               proxy
                  )
      }
      last_page <- 
            last_page %>% 
            read_html()  %>% 
            html_nodes(xpath ="/html/body/div[@id='main-container']/div[@id='action-bar-container']/div[@class='action-bar']/div[@class='btn-group btn-group-paging']/a[@href][2]") %>%
            html_attrs() %>%
            unlist() %>%
            .[2] %>%
            as.character() %>%
            substr(regexec("/index", .)%>%.[[1]]%>%.[1]+6
                   , regexec(".html", .)%>%.[[1]]%>%.[1]-1) %>%
            as.numeric()+1
      
      Sys.sleep(abs(rnorm(1)*time_break[1]))
      
      first_page <- last_page-search_range+1
      
      if(showProgress==TRUE){
            cat(paste0("PTT-", search_board, "'s Crawler\n", 
                       "Page from ", first_page, " to ", last_page
            ), 
            "\n\n"
            );cat("Go!!\n\n")
      }
      
      
      
      
      ### 爬蟲主體: 頁面&文章代碼 - 紀錄文章代碼----
      
      total_article_title <- tbl_df(NULL)
      error_list <- list()
      for(page_i in last_page:first_page){
            page_time_1 <- Sys.time()
            
            error_list[[length(error_list)+1]] <- 
                  try({
                        if(missing(proxy)){
                              page_html <- 
                                    html_session(sprintf(url, page_i), use_cookies) %>% 
                                    read_html()
                        }else{
                              page_html <- 
                                    html_session(sprintf(url, page_i), use_cookies, 
                                                 proxy
                                    ) %>% 
                                    read_html()
                        }
                        
                        article_title <- 
                              data.frame(ARTICLE_ID=page_html%>% 
                                               html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='title']") %>%
                                               str_match(paste0("/bbs/", search_board, "/.*html")), 
                                         URL=page_html%>% 
                                               html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='title']") %>%
                                               str_match(paste0("/bbs/", search_board, "/.*html")), 
                                         TITLE=page_html%>% 
                                               html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='title']") %>%
                                               gsub("\n", "", .) %>%
                                               gsub("\t", "", .) %>%
                                               gsub("<div class=\"title\">|<a href=\"/bbs/.*html\">", "", .) %>% 
                                               gsub("<a.*>|<div.*>|</a.*>|</div.*>", "", .) %>% 
                                               gsub("&amp;", "&", .) %>% 
                                               gsub("<span.*email=.*</script>", "?", .), 
                                         stringsAsFactors=FALSE
                              ) %>% 
                              tbl_df() %>% 
                              mutate(ARTICLE_ID=ifelse(ARTICLE_ID==TITLE, NA, 
                                                       gsub(paste0("/bbs/", search_board, "/|.html"), "", ARTICLE_ID)), 
                                     URL=ifelse(URL==TITLE, NA, 
                                                paste0("https://www.ptt.cc", URL)))
                        
                        article_title$PUSH <-
                              page_html%>% 
                              html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='nrec']") %>%
                              html_text()
                        
                        article_title$DATE <-
                              page_html%>% 
                              html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='meta']/div[@class='date']") %>%
                              html_text() %>%
                              gsub(" ", "", .)
                        
                        article_title$AUTHOR <-
                              page_html%>% 
                              html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='meta']/div[@class='author']") %>%
                              html_text() %>%
                              gsub(" ", "", .)
                        
                        total_article_title <- rbind(article_title, total_article_title)
                        
                        
                        Sys.sleep(abs(rnorm(1)*time_break[1]))
                        
                        if(showProgress==TRUE){
                              cat("current page ", page_i, "(", last_page-page_i+1, "/", last_page-first_page+1, ") Done!     "); print(Sys.time()-page_time_1)
                        }
                  }, silent = TRUE
                  )
      }
      
      #標記文章代號-數字
      article_url_to_number <- function(url_c){
            url_c <- substr(url_c, (26+nchar(search_board)+1), (26+nchar(search_board)+10))
            url_c <- ifelse(is.na(url_c), lag(url_c), url_c)
            return(url_c)
      }
      
      
      ### 爬蟲主體: 頁面&文章代碼 - 標記置底文章----
      total_article_title <- 
            total_article_title %>% 
            mutate(LOWER_BOUND2=article_url_to_number(URL), 
                   LOWER_BOUND3=article_url_to_number(lag(URL))
            ) %>% 
            mutate(LOWER_BOUND=ifelse(LOWER_BOUND2>=LOWER_BOUND3, 1, 0))
      
      total_article_title <- 
            total_article_title %>% 
            head(min(c(which(total_article_title$LOWER_BOUND==0), nrow(total_article_title)+1))-1) %>% 
            select(-LOWER_BOUND, -LOWER_BOUND2, -LOWER_BOUND3) %>% 
            mutate(ORDER=row_number()) %>% 
            rbind(total_article_title %>% 
                        slice(min(c(which(total_article_title$LOWER_BOUND==0), 
                                    nrow(total_article_title)+1)
                        ):nrow(total_article_title)
                        ) %>% 
                        select(-LOWER_BOUND, -LOWER_BOUND2, -LOWER_BOUND3) %>% 
                        mutate(ORDER=-row_number())
            ) %>% 
            mutate(CRAWLING_TIME=Sys.time(), 
                   ARTICLE_ID=iconv(ARTICLE_ID, from="UTF-8"), 
                   URL=iconv(URL, from="UTF-8"), 
                   TITLE=iconv(TITLE, from="UTF-8"), 
                   PUSH=iconv(PUSH, from="UTF-8"), 
                   DATE=iconv(DATE, from="UTF-8"), 
                   AUTHOR=iconv(AUTHOR, from="UTF-8"), 
                   KEY_WORD=NA
            )
      
      if(file.exists(paste0(path_board, "article_list.csv"))){
            total_article_title_old <- fread(paste0(path_board, "article_list.csv")) %>% tbl_df()
            total_article_title <- 
                  rbind(total_article_title %>% filter(!is.na(ARTICLE_ID)), 
                        total_article_title_old %>% filter(!is.na(ARTICLE_ID))
                  )
      }else{
            total_article_title_old <- tbl_df(NULL) 
            total_article_title <- total_article_title %>% filter(!is.na(ARTICLE_ID))
      }
      
      
      total_article_title <- 
            total_article_title %>% 
            arrange(ARTICLE_ID, desc(CRAWLING_TIME)) %>% 
            distinct(ARTICLE_ID, .keep_all=TRUE) %>% 
            arrange(desc(ORDER/abs(ORDER)), ARTICLE_ID, desc(CRAWLING_TIME))
      
      total_article_title <- 
            rbind(total_article_title %>% 
                        filter(ORDER>0) %>% 
                        mutate(ORDER=row_number()), 
                  total_article_title %>% 
                        filter(ORDER<0) %>% 
                        mutate(ORDER=-row_number())
            )
      
      
      write.csv(total_article_title, paste0(path_board, "article_list.csv"), row.names=FALSE)
      
      page_time_end <- Sys.time()
      
      
      
      ### 爬蟲主體: 文章內文----
      article_time_start <- Sys.time()
      for(total_article_title_i in nrow(total_article_title):1){
            
            
            article_time_1 <- Sys.time()
            
            error_list[[length(error_list)+1]] <- 
                  try({
                        if(missing(proxy)){
                              page_html <- 
                                    html_session(total_article_title$URL[total_article_title_i], use_cookies)
                        }else{
                              page_html <- 
                                    html_session(total_article_title$URL[total_article_title_i], use_cookies, 
                                                 proxy
                                    )
                        }
                        
                        if(page_html$response$status_code==200){
                              page_html <- 
                                    page_html %>% 
                                    read_html()
                              
                              article_text <- 
                                    rbind(
                                          data.table(TEXT=search_board) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="BOARD"), 
                                          data.table(TEXT=total_article_title$ARTICLE_ID[total_article_title_i]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="ARTICLE_ID"), 
                                          data.table(TEXT=total_article_title$URL[total_article_title_i]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="URL"), 
                                          data.table(TEXT="---------") %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="BREAK_LINE"), 
                                          data.table(TEXT=page_html%>% 
                                                           html_nodes(xpath = "/html/body/div[@id='main-container']/div[@id='main-content']") %>%
                                                           html_children() %>% head(4) %>% html_text()
                                          ) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="TITLE") %>% 
                                                filter((substr(TEXT, 1, 2)=="作者"&
                                                              substr(TEXT, str_length(TEXT), str_length(TEXT))==")")|
                                                             (substr(TEXT, 1, 2)=="看板"&
                                                                    str_detect(TEXT, search_board))|
                                                             (substr(TEXT, 1, 2)=="標題")|
                                                             (substr(TEXT, 1, 2)=="時間"&
                                                                    str_count(TEXT, ":")==2&
                                                                    substr(TEXT, 7, 9)%in%month.abb
                                                             )
                                                ), 
                                          data.table(TEXT="---------") %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="BREAK_LINE"), 
                                          data.table(TEXT=page_html %>% 
                                                           html_nodes(xpath = "/html/body/div[@id='main-container']/div[@id='main-content']") %>%
                                                           html_text() %>% 
                                                           strsplit("\n") %>% .[[1]] %>% .[-1]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="TEXT")
                                    )
                              
                              article_text <- 
                                    article_text %>% 
                                    mutate(TYPE=ifelse(TYPE=="TEXT"&str_detect(TEXT, "※ 發信站|※ 文章網址|◆ From:"), 
                                                       "TEXT_BREAK", TYPE)
                                    )
                              
                              text_break <- max(c(which(article_text$TYPE=="TEXT_BREAK"), 0))
                              if(text_break>0){
                                    article_text <- 
                                          article_text %>% 
                                          mutate(., 
                                                 TYPE=ifelse(row_number()>text_break &
                                                                   TYPE=="TEXT", 
                                                             "TEXT_EDIT", TYPE)
                                          )
                              }
                              article_text <- 
                                    article_text %>% 
                                    mutate(., 
                                           TYPE=ifelse(row_number()>text_break&
                                                             TYPE%in%c("TEXT", "TEXT_EDIT")&
                                                             substr(TEXT, 1, 1)%in%c("推", "→", "噓")&
                                                             substr(TEXT, str_length(TEXT)-2, str_length(TEXT)-2)==":", 
                                                       "TEXT_PUSH", TYPE)
                                    )
                              
                              if(sum(!is.na(search_keyword))!=0){
                                    key_word_time <- sum(str_count(article_text$TEXT, paste0(search_keyword, collapse="|")))
                                    total_article_title$KEY_WORD[total_article_title_i] <- key_word_time
                                    
                                    if(key_word_time!=0){
                                          for(key_word_time_i in search_keyword){
                                                if(sum(str_count(article_text$TEXT, 
                                                                 paste0(str_split(key_word_time_i, "\\|")[[1]], collapse="|")))!=0
                                                ){
                                                      write.csv(article_text, 
                                                                paste0(path_board, "KEYWORD/", 
                                                                       str_split(key_word_time_i, "\\|")[[1]][1], "/", 
                                                                       total_article_title$ARTICLE_ID[total_article_title_i], "_", 
                                                                       format(as.Date(paste0("2017/", total_article_title$DATE[total_article_title_i])), "%m%d"), 
                                                                       ".csv"), 
                                                                row.names=FALSE)
                                                }
                                          }
                                          write.csv(total_article_title, paste0(path_board, "article_list.csv"), row.names=FALSE)
                                          write.csv(article_text, 
                                                    paste0(path_board, "KEYWORD/", 
                                                           total_article_title$ARTICLE_ID[total_article_title_i], "_", 
                                                           format(as.Date(paste0("2017/", total_article_title$DATE[total_article_title_i])), "%m%d"), 
                                                           ".csv"), 
                                                    row.names=FALSE)
                                    }
                                    
                              }else{
                                    total_article_title$KEY_WORD[total_article_title_i] <- 0
                              }
                              
                              write.csv(article_text, 
                                        paste0(path_board, 
                                               total_article_title$ARTICLE_ID[total_article_title_i], 
                                               ".csv"), 
                                        row.names=FALSE)
                              
                              saveRDS(article_text, 
                                      paste0(path_board, 
                                             total_article_title$ARTICLE_ID[total_article_title_i], 
                                             ".rds"))
                              
                              Sys.sleep(abs(rnorm(1)*time_break[2]))
                              
                              if(showProgress==TRUE){
                                    cat("current article ", total_article_title$ARTICLE_ID[total_article_title_i], 
                                        "(", nrow(total_article_title)-total_article_title_i+1, "/", nrow(total_article_title), 
                                        ") Done!     "
                                    ); print(Sys.time()-article_time_1)
                              }
                              
                        }else{
                              Sys.sleep(abs(rnorm(1)*time_break[2]))
                              
                              if(showProgress==TRUE){
                                    cat("Can't connect - ", total_article_title$ARTICLE_ID[total_article_title_i], 
                                        "(", nrow(total_article_title)-total_article_title_i+1, "/", nrow(total_article_title), 
                                        ")    "
                                    ); print(Sys.time()-article_time_1)
                              }
                              
                        }
                  }, silent = TRUE
                  )
      }
      
      write.csv(total_article_title, paste0(path_board, "article_list.csv"), row.names=FALSE)
      
      
      article_time_end <- Sys.time()
      
      
      
      ####   結束----
      
      if(showProgress==TRUE){
            cat("頁面&文章代碼  ");print(page_time_end-page_time_start)
            cat("文章內文       ");print(article_time_end-article_time_start)
      }
      
      print(error_list)
      
}

#測試
# ptt_crawler_download(search_board, search_range, search_keyword, path, c(10, 5), 
#                      showProgress=TRUE, 
#                      proxy=use_proxy("http://dsfsdfsf", port=9999))







####   抓取彙整為list----

ptt_crawler_list <- function(search_board, 
                             search_range = 10, 
                             search_keyword = NA, 
                             size_mb_limit = 1024, 
                             time_break=c(10, 5), 
                             showProgress=TRUE, 
                             proxy, 
                             use_cookies=set_cookies()){
      
      #### 環境設置----##############################################
      
      
      #### 環境設置: library----
      # library(XML)
      # library(RCurl)
      # library(jsonlite)
      require(data.table)
      require(tidyverse)
      require(lubridate)
      require(stringr)
      require(rvest)
      require(httr)
      
      
      
      #### 路徑整理----##############################################
      
      
      
      #### 爬蟲主體----##############################################
      
      ### 爬蟲主體: 頁面&文章代碼----
      page_time_start <- Sys.time()
      
      url <- paste0("https://www.ptt.cc/bbs/", search_board, "/index%s.html")
      
      if(missing(proxy)){
            last_page <- 
                  html_session(sprintf(url,  ""), use_cookies)
      }else{
            last_page <- 
                  html_session(sprintf(url,  ""), use_cookies, 
                               proxy
                  )
      }
      
      last_page <- 
            last_page %>% 
            read_html()  %>% 
            html_nodes(xpath ="/html/body/div[@id='main-container']/div[@id='action-bar-container']/div[@class='action-bar']/div[@class='btn-group btn-group-paging']/a[@href][2]") %>%
            html_attrs() %>%
            unlist() %>%
            .[2] %>%
            as.character() %>%
            substr(regexec("/index", .)%>%.[[1]]%>%.[1]+6
                   , regexec(".html", .)%>%.[[1]]%>%.[1]-1) %>%
            as.numeric()+1
      
      
      first_page <- last_page-search_range+1
      
      if(showProgress==TRUE){
            cat(paste0("PTT-", search_board, "'s Crawler\n", 
                       "Page from ", first_page, " to ", last_page
            ), 
            "\n\n"
            );cat("Go!!\n\n")
      }
      
      
      Sys.sleep(abs(rnorm(1)*time_break[1]))
      
      ### 爬蟲主體: 頁面&文章代碼 - 紀錄文章代碼----
      
      total_article_title <- tbl_df(NULL)
      error_list <- list()
      for(page_i in last_page:first_page){
            page_time_1 <- Sys.time()
            
            error_list[[length(error_list)+1]] <- 
                  try({
                        if(missing(proxy)){
                              page_html <- 
                                    html_session(sprintf(url, page_i), use_cookies) %>% 
                                    read_html()
                        }else{
                              page_html <- 
                                    html_session(sprintf(url, page_i), use_cookies, 
                                                 proxy
                                    ) %>% 
                                    read_html()
                        }
                        
                        article_title <- 
                              data.frame(ARTICLE_ID=page_html%>% 
                                               html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='title']") %>%
                                               str_match(paste0("/bbs/", search_board, "/.*html")), 
                                         URL=page_html%>% 
                                               html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='title']") %>%
                                               str_match(paste0("/bbs/", search_board, "/.*html")), 
                                         TITLE=page_html%>% 
                                               html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='title']") %>%
                                               gsub("\n", "", .) %>%
                                               gsub("\t", "", .) %>%
                                               gsub("<div class=\"title\">|<a href=\"/bbs/.*html\">", "", .) %>% 
                                               gsub("<a.*>|<div.*>|</a.*>|</div.*>", "", .) %>% 
                                               gsub("&amp;", "&", .) %>% 
                                               gsub("<span.*email=.*</script>", "?", .), 
                                         stringsAsFactors=FALSE
                              ) %>% 
                              tbl_df() %>% 
                              mutate(ARTICLE_ID=ifelse(ARTICLE_ID==TITLE, NA, 
                                                       gsub(paste0("/bbs/", search_board, "/|.html"), "", ARTICLE_ID)), 
                                     URL=ifelse(URL==TITLE, NA, 
                                                paste0("https://www.ptt.cc", URL)))
                        
                        article_title$PUSH <-
                              page_html%>% 
                              html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='nrec']") %>%
                              html_text()
                        
                        article_title$DATE <-
                              page_html%>% 
                              html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='meta']/div[@class='date']") %>%
                              html_text() %>%
                              gsub(" ", "", .)
                        
                        article_title$AUTHOR <-
                              page_html%>% 
                              html_nodes(xpath = "/html/body/div[@id='main-container']//div[@class='r-ent']/div[@class='meta']/div[@class='author']") %>%
                              html_text() %>%
                              gsub(" ", "", .)
                        
                        total_article_title <- rbind(article_title, total_article_title)
                        
                        Sys.sleep(abs(rnorm(1)*time_break[1]))
                        
                        if(showProgress==TRUE){
                              cat("current page ", page_i, "(", last_page-page_i+1, "/", last_page-first_page+1, ") Done!     "
                              ); print(Sys.time()-page_time_1)
                        }
                  }, silent = TRUE
                  )
      }
      
      #標記文章代號-數字
      article_url_to_number <- function(url_c){
            url_c <- substr(url_c, (26+nchar(search_board)+1), (26+nchar(search_board)+10))
            url_c <- ifelse(is.na(url_c), lag(url_c), url_c)
            return(url_c)
      }
      
      
      ### 爬蟲主體: 頁面&文章代碼 - 標記置底文章----
      total_article_title <- 
            total_article_title %>% 
            mutate(LOWER_BOUND2=article_url_to_number(URL), 
                   LOWER_BOUND3=article_url_to_number(lag(URL))
            ) %>% 
            mutate(LOWER_BOUND=ifelse(LOWER_BOUND2>=LOWER_BOUND3, 1, 0))
      
      total_article_title <- 
            total_article_title %>% 
            head(min(c(which(total_article_title$LOWER_BOUND==0), nrow(total_article_title)+1))-1) %>% 
            select(-LOWER_BOUND, -LOWER_BOUND2, -LOWER_BOUND3) %>% 
            mutate(ORDER=row_number()) %>% 
            rbind(total_article_title %>% 
                        slice(min(c(which(total_article_title$LOWER_BOUND==0), 
                                    nrow(total_article_title)+1)
                        ):nrow(total_article_title)
                        ) %>% 
                        select(-LOWER_BOUND, -LOWER_BOUND2, -LOWER_BOUND3) %>% 
                        mutate(ORDER=-row_number())
            ) %>% 
            mutate(ARTICLE_ID=iconv(ARTICLE_ID, from="UTF-8"), 
                   URL=iconv(URL, from="UTF-8"), 
                   TITLE=iconv(TITLE, from="UTF-8"), 
                   PUSH=iconv(PUSH, from="UTF-8"), 
                   DATE=iconv(DATE, from="UTF-8"), 
                   AUTHOR=iconv(AUTHOR, from="UTF-8"), 
                   KEY_WORD=0
            )
      
      
      article_title <- 
            list(CRAWLING_TIME=Sys.time(), 
                 SUMMARY=total_article_title
            )
      
      
      page_time_end <- Sys.time()
      
      
      
      ### 爬蟲主體: 文章內文----
      article_time_start <- Sys.time()
      
      list_i <- 3
      for(total_article_title_i in nrow(total_article_title):1){
            article_time_1 <- Sys.time()
            
            error_list[[length(error_list)+1]] <- 
                  try({
                        if(missing(proxy)){
                              page_html <- 
                                    html_session(total_article_title$URL[total_article_title_i], use_cookies)
                        }else{
                              page_html <- 
                                    html_session(total_article_title$URL[total_article_title_i], use_cookies, 
                                                 proxy
                                    )
                        }
                        #判斷是否抓取成功
                        if(page_html$response$status_code==200){
                              page_html <- 
                                    page_html %>% 
                                    read_html()
                              
                              #文章資訊整合
                              article_text <- 
                                    rbind(
                                          data.table(TEXT=search_board) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="BOARD"), 
                                          data.table(TEXT=total_article_title$AUTHOR[total_article_title_i]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="ARTICLE_AUTHOR"), 
                                          data.table(TEXT=total_article_title$TITLE[total_article_title_i]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="ARTICLE_TITLE"), 
                                          data.table(TEXT=total_article_title$DATE[total_article_title_i]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="ARTICLE_DATE"), 
                                          data.table(TEXT=total_article_title$ARTICLE_ID[total_article_title_i]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="ARTICLE_ID"), 
                                          data.table(TEXT=total_article_title$URL[total_article_title_i]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="URL"), 
                                          data.table(TEXT="---------") %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="BREAK_LINE"), 
                                          data.table(TEXT=page_html%>% 
                                                           html_nodes(xpath = "/html/body/div[@id='main-container']/div[@id='main-content']") %>%
                                                           html_children() %>% head(4) %>% html_text()
                                          ) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="TITLE") %>% 
                                                filter((substr(TEXT, 1, 2)=="作者"&
                                                              substr(TEXT, str_length(TEXT), str_length(TEXT))==")")|
                                                             (substr(TEXT, 1, 2)=="看板"&
                                                                    str_detect(TEXT, search_board))|
                                                             (substr(TEXT, 1, 2)=="標題")|
                                                             (substr(TEXT, 1, 2)=="時間"&
                                                                    str_count(TEXT, ":")==2&
                                                                    substr(TEXT, 7, 9)%in%month.abb
                                                             )
                                                ), 
                                          data.table(TEXT="---------") %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="BREAK_LINE"), 
                                          data.table(TEXT=page_html %>% 
                                                           html_nodes(xpath = "/html/body/div[@id='main-container']/div[@id='main-content']") %>%
                                                           html_text() %>% 
                                                           strsplit("\n") %>% .[[1]] %>% .[-1]) %>% 
                                                tbl_df() %>% 
                                                mutate(TYPE="TEXT")
                                    )
                              
                              # 各行類型搜尋
                              article_text <- 
                                    article_text %>% 
                                    mutate(TYPE=ifelse(TYPE=="TEXT"&str_detect(TEXT, "※ 發信站|※ 文章網址|◆ From:"), 
                                                       "TEXT_BREAK", TYPE)
                                    )
                              text_break <- max(c(which(article_text$TYPE=="TEXT_BREAK"), 0))
                              if(text_break>0){
                                    article_text <- 
                                          article_text %>% 
                                          mutate(., 
                                                 TYPE=ifelse(row_number()>text_break &
                                                                   TYPE=="TEXT", 
                                                             "TEXT_EDIT", TYPE)
                                          )
                              }
                              article_text <- 
                                    article_text %>% 
                                    mutate(., 
                                           TYPE=ifelse(row_number()>text_break&
                                                             TYPE%in%c("TEXT", "TEXT_EDIT")&
                                                             substr(TEXT, 1, 1)%in%c("推", "→", "噓")&
                                                             substr(TEXT, str_length(TEXT)-2, str_length(TEXT)-2)==":", 
                                                       "TEXT_PUSH", TYPE)
                                    )
                              
                              # KEYWORK搜尋
                              if(sum(!is.na(search_keyword))!=0){
                                    for(key_word_time_i in search_keyword){
                                          article_text <- 
                                                article_text %>% 
                                                mutate(KEY=ifelse(TYPE%in%c("TEXT", "TEXT_EDIT", "TEXT_PUSH"), 
                                                                  str_count(TEXT, paste0(key_word_time_i, collapse="|")), 
                                                                  0)) %>% 
                                                chname("KEY", str_split(key_word_time_i, "\\|")[[1]][1])
                                    }
                                    article_title$SUMMARY$KEY_WORD[total_article_title_i] <- 
                                          article_title$SUMMARY$KEY_WORD[total_article_title_i]+
                                          sum(article_text %>% select(-TEXT, -TYPE))
                              }
                              
                              # 文章內容彙整
                              eval(parse(text=paste0("article_title$", 
                                                     total_article_title$ARTICLE_ID[total_article_title_i], 
                                                     " <- article_text")))
                              
                              Sys.sleep(abs(rnorm(1)*time_break[2]))
                              
                              if(showProgress==TRUE){
                                    cat("current article ", total_article_title$ARTICLE_ID[total_article_title_i], 
                                        "(", nrow(total_article_title)-total_article_title_i+1, "/", nrow(total_article_title), 
                                        ") Done!     "
                                    ); print(Sys.time()-article_time_1)
                              }
                              
                        }else{
                              # if(!is.na(total_article_title$ARTICLE_ID[total_article_title_i])){
                              #   eval(parse(text=paste0("article_title$", 
                              #                          total_article_title$ARTICLE_ID[total_article_title_i], 
                              #                          " <- NA")))
                              # }
                              # article_title[[nrow(total_article_title)-total_article_title_i+1+2]] <- 
                              #   data.table(Status=page_html$response$status_code, 
                              #              ARTICLE_ID=total_article_title$ARTICLE_ID[total_article_title_i], 
                              #              URL=total_article_title$URL[total_article_title_i], 
                              #              TITLE=total_article_title$TITLE[total_article_title_i]
                              #   ) %>% 
                              #   tbl_df()
                              
                              Sys.sleep(abs(rnorm(1)*time_break[2]))
                              
                              if(showProgress==TRUE){
                                    cat("Can't connect - ", total_article_title$ARTICLE_ID[total_article_title_i], 
                                        "(", nrow(total_article_title)-total_article_title_i+1, "/", nrow(total_article_title), 
                                        ")    "
                                    ); print(Sys.time()-article_time_1)
                              }
                              
                        }
                        
                        if(as.numeric(gsub(" Mb", "", format(object.size(article_title), units="Mb")))>=
                           size_mb_limit){
                              warning("記憶體達設定安全值")
                              break
                        }
                  }, silent = TRUE
                  )
      }
      
      article_time_end <- Sys.time()
      
      ####   結束----
      
      if(showProgress==TRUE){
            cat("頁面&文章代碼  ");print(page_time_end-page_time_start)
            cat("文章內文       ");print(article_time_end-article_time_start)
      }
      
      
      #### 資料回傳----  
      return(list(article_title=article_title, 
                  error_list=error_list))
}

#測試
# result <-
#   ptt_crawler_list(search_board, search_range, search_keyword, 1024, c(10, 5), 
#                      showProgress=TRUE, 
#                      proxy=use_proxy("http://dsfsdfsf", port=9999))
# 
# result2 <-
#   ptt_crawler_list(search_board, search_range, search_keyword, 0.01, c(2, 2), 
#                      showProgress=TRUE, 
#                      proxy=use_proxy("http://dsfsdfsf", port=9999))






####   文章整併----

article_combined <- function(data, search_date_start, search_date_end){
      require(tidyverse)
      require(stringr)
      nato <- function(x, y){x[is.na(x)] <- y;return(x)}
      
      data <- 
            lapply(data, 
                   function(x){
                         Sys.setlocale("LC_TIME", "C")
                         x2 <- 
                               x %>% 
                               filter(TYPE=="TITLE", 
                                      substr(TEXT, 1, 2)=="時間"&
                                            str_count(TEXT, ":")==2&
                                            substr(TEXT, 7, 9)%in%month.abb
                               ) %>% 
                               .$TEXT %>% 
                               gsub("時間", "", .) %>% 
                               strptime("%a %b %d %H:%M:%S %Y") %>% 
                               format("%Y-%m-%d %H:%M:%S") %>% 
                               .[1]
                         xx <- x %>% filter(TYPE=="ARTICLE_ID") %>% .$TEXT
                         Sys.setlocale("LC_TIME")
                         result <- 
                               x %>% 
                               # filter(TYPE%in%c("ARTICLE_TITLE", "TEXT", "TEXT_PUSH", "TEXT_EDIT")) %>% 
                               filter(TYPE%in%c("ARTICLE_TITLE", "TEXT", "TEXT_PUSH")) %>% 
                               mutate(ARTICLE_TIME=x2) %>% 
                               mutate(PUSH=ifelse(TYPE=="TEXT_PUSH", substr(TEXT, 1, 1), ""), 
                                      PUSH_EDITOR=ifelse(TYPE=="TEXT_PUSH", 
                                                         substr(TEXT, 3, 
                                                                str_locate(TEXT, ":")-1), ""), 
                                      PUSH_TIME=ifelse(TYPE=="TEXT_PUSH", 
                                                       str_match(TEXT, 
                                                                 "[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}$")[, 1], 
                                                       "")
                               ) %>% 
                               mutate(REAL_TEXT=ifelse(TYPE=="TEXT_PUSH", 
                                                       substr(TEXT, 
                                                              str_locate(TEXT, ":")[, 1]+1, 
                                                              str_locate(TEXT, "[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}$")[, 1]-1), 
                                                       TEXT)
                               ) %>% 
                               mutate(TEXT=ifelse(str_length(TEXT)>300, 
                                                  paste0(substr(TEXT, 1, 50), "...字串長度異常"), 
                                                  TEXT), 
                                      REAL_TEXT=ifelse(str_length(REAL_TEXT)>300, 
                                                       paste0(substr(REAL_TEXT, 1, 50), "...字串長度異常"), 
                                                       REAL_TEXT)
                               ) %>% 
                               mutate(REAL_TEXT=nato(REAL_TEXT, "")) %>% 
                               mutate(PUSH_TIME=ifelse(TYPE=="TEXT_PUSH", 
                                                       gsub("/", "-", substr(PUSH_TIME, 1, 5)), 
                                                       substr(ARTICLE_TIME, 6, 10))
                               ) %>% 
                               mutate(PUSH_TIME=ifelse(is.na(PUSH_TIME),  #支援連續三次日期遺失
                                                       lag(PUSH_TIME), 
                                                       PUSH_TIME)
                               ) %>% 
                               mutate(PUSH_TIME=ifelse(is.na(PUSH_TIME), 
                                                       lag(lag(PUSH_TIME)), 
                                                       PUSH_TIME)
                               ) %>% 
                               mutate(PUSH_TIME=ifelse(is.na(PUSH_TIME), 
                                                       lag(lag(lag(PUSH_TIME))), 
                                                       PUSH_TIME)
                               ) %>% 
                               mutate(ORDER=as.numeric(PUSH_TIME>substr(ARTICLE_TIME, 6, 10))) %>% 
                               mutate(ORDER2=row_number()) %>% 
                               group_by(ORDER) %>% 
                               mutate(ORDER2=ifelse(ORDER==0, 
                                                    ORDER2-lag(ORDER2), 
                                                    NA) %>% nato(1)
                               ) %>% 
                               ungroup() %>% 
                               mutate(ORDER2=as.numeric(cut(row_number(), 
                                                            breaks=c(0, which(ORDER2>1)-1, 
                                                                     n())))-1
                               ) %>% 
                               mutate(PUSH_TIME=paste0(substr(as.Date(substr(ARTICLE_TIME, 1, 10))%m+%
                                                                    years(ORDER2), 1, 5), PUSH_TIME)
                               ) %>% 
                               select(-ORDER, -ORDER2) %>% 
                               mutate(PUSH_TIME=ifelse(str_detect(as.character(PUSH_TIME), "NA"), 
                                                       NA, 
                                                       PUSH_TIME)) %>% 
                               mutate(ARTICLE_ID=xx)
                         return(result)
                   }
            )
      data <- 
            lapply(data, 
                   function(x){
                         x <- 
                               x %>% 
                               mutate(REAL_TEXT=gsub('"', "'", REAL_TEXT)) %>% 
                               select(REAL_TEXT, PUSH_TIME, ARTICLE_ID) %>% t()
                         return(x)
                   }
            ) %>% 
            unlist() %>%
            matrix(ncol=3, byrow=T) %>% 
            data.frame() %>% 
            transmute(REAL_TEXT=gsub("(\\w)", "\\L\\1", X1, perl=TRUE), 
                      PUSH_TIME=X2, 
                      ARTICLE_ID=X3
            ) %>% 
            filter(is.na(PUSH_TIME)|
                         as.Date(PUSH_TIME)%in%seq.Date(as.Date(search_date_start), as.Date(search_date_end), "day")
            )
      return(data)
}

#測試
# article_combined(article_data, search_date_start, search_date_end)











# 段詞(鎖定關鍵字出現之文章)----

article_cut <- function(data, text_cut=worker(), select_keyword_article){
      require(tidyverse)
      require(jiebaR)
      
      if(missing(select_keyword_article)){
            text_data <- 
                  data %>% 
                  mutate(REAL_TEXT=gsub(paste0("[[:digit:]]|[[:punct:]]|", 
                                               paste0("～", 
                                                      gsub("", "|", "！＠＃＄％︿＆＊（）＿＋－＝｛｝［］【】？‵１２３４５６７８９"), 
                                                      "０")
                  ), 
                  " ", REAL_TEXT)
                  ) %>% 
                  group_by(PUSH_TIME) %>% 
                  summarise(REAL_TEXT=gsub(" {2, }", " ", paste0(REAL_TEXT, collapse=" "))) %>% 
                  ungroup()
            
            tt <- segment(text_data$REAL_TEXT, text_cut)
            names(tt) <- paste0("DATE", text_data$PUSH_TIME)
            text_data <- tt; rm(tt)
      }else{
            text_data <- 
                  data %>% 
                  mutate(REAL_TEXT=gsub(paste0("[[:digit:]]|[[:punct:]]|", 
                                               paste0("～", 
                                                      gsub("", "|", "！＠＃＄％︿＆＊（）＿＋－＝｛｝［］【】？‵１２３４５６７８９"), 
                                                      "０")
                  ), 
                  " ", REAL_TEXT)
                  ) %>% 
                  group_by(ARTICLE_ID) %>% 
                  summarise(REAL_TEXT=gsub(" {2, }", " ", paste0(REAL_TEXT, collapse=" "))) %>% 
                  ungroup()
            
            tt <- segment(text_data$REAL_TEXT, text_cut)
            names(tt) <- paste0(text_data$ARTICLE_ID)
            text_data <- tt
            
            tt <- as.character()
            for(i in names(text_data)){
                  if(sum(str_detect(text_data[i][[1]], paste0(select_keyword_article, collapse="|")))>0){
                        tt <- c(tt, i)
                  }
            }
            text_data <- 
                  data %>% 
                  filter(ARTICLE_ID%in%tt) %>% 
                  mutate(REAL_TEXT=gsub(paste0("[[:digit:]]|[[:punct:]]|", 
                                               paste0("～", 
                                                      gsub("", "|", "！＠＃＄％︿＆＊（）＿＋－＝｛｝［］【】？‵１２３４５６７８９"), 
                                                      "０")
                  ), 
                  " ", REAL_TEXT)
                  ) %>% 
                  group_by(PUSH_TIME) %>% 
                  summarise(REAL_TEXT=gsub(" {2, }", " ", paste0(REAL_TEXT, collapse=" "))) %>% 
                  ungroup()
            
            tt <- segment(text_data$REAL_TEXT, text_cut)
            names(tt) <- paste0("DATE", text_data$PUSH_TIME)
            text_data <- tt; rm(tt)
      }
      return(text_data)
}
# text_data <- article_cut(article_data, text_cut, select_keyword_article)












#計次、整合、關鍵字詞彙替換、重新計次----
freq_combined <- function(data, search_keyword){
      require(tidyverse)
      
      data <- lapply(data, freq)
      #整合
      text_freq <- tbl_df(NULL)
      for(i in names(data)){
            text_freq <- 
                  rbind(text_freq, 
                        data[i][[1]] %>% 
                              tbl_df() %>% 
                              mutate(PUSH_DATE=gsub("DATE", "", i)) %>% 
                              mutate(PUSH_DATE=as.Date(ifelse(PUSH_DATE=="NA", NA, PUSH_DATE))) %>% 
                              rename(CHAR=char, 
                                     FREQ=freq
                              )
                  )
      }
      #詞彙替換
      if(!missing(search_keyword)){
            for(i in search_keyword){
                  text_freq <- 
                        text_freq %>% 
                        mutate(CHAR=gsub(i, 
                                         str_split(i, "\\|", simplify = T)[1, 1], 
                                         CHAR))
            }
      }
      #重新計次
      text_freq <- 
            text_freq %>%
            group_by(CHAR, PUSH_DATE) %>% 
            summarise(FREQ=sum(FREQ)) %>% 
            ungroup() %>% 
            arrange(desc(FREQ))
      
      return(text_freq)
}
# text_freq <- freq_combined(text_data)














#額外移除字彙與指定字詞最低長度----
text_remove_word <- function(data, char_min, remove_word){
      require(tidyverse)
      require(stringr)
      
      if(missing(remove_word)){
            remove_word <- as.character()
      }
      data <- 
            data %>% 
            arrange(desc(FREQ)) %>% 
            filter(!CHAR%in%remove_word) %>%
            filter(str_length(CHAR)>=char_min)
      return(data)
}
# text_freq <- text_remove_word(text_freq, char_min, remove_word)









## 趨勢-文字雲----
text_freq_cloud <- function(data, top=300, size=1.5, minSize=0, shuffle=TRUE, rotateRatio=0, version=2){
      require(tidyverse)
      require(tidyverse)
      
      data <- 
            data %>% 
            group_by(CHAR) %>% 
            summarise(FREQ=sum(FREQ)) %>% 
            ungroup() %>% 
            arrange(desc(FREQ))
      if(version==1){
            p <- 
                  wordcloud(words=data$CHAR, 
                            freq=data$FREQ, 
                            scale=c(4, .5), 
                            min.freq=min(headf(data, top)$FREQ), 
                            max.words=top, 
                            colors=rainbow(length(data$FREQ))
                  )
      }
      if(version==2){
            p <- 
                  wordcloud2(headf(data, top), size=size, minSize=minSize, shuffle=shuffle, rotateRatio=rotateRatio)
      }
      return(p)
}
# text_freq_cloud(data)







## 趨勢-每月關鍵字----
text_freq_keyword_month_trend <- function(data, search_keyword, cum=FALSE){
      require(tidyverse)
      require(stringr)
      require(plotly)
      
      search_keyword_t <- str_split(search_keyword, "\\|", simplify=TRUE)[, 1]
      if(length(search_keyword_t)>8){
            color_set <- rainbow(length(search_keyword_t))
      }else{
            color_set <- RColorBrewer::brewer.pal(max(length(search_keyword_t), 3),  "Set2")
      }
      data <- 
            data %>% 
            filter(CHAR%in%search_keyword_t) %>% 
            mutate(MONTH=format(PUSH_DATE, "%Y%m")) %>% 
            group_by(CHAR, MONTH) %>% 
            summarise(FREQ=sum(FREQ)) %>% 
            ungroup() %>% 
            spread(CHAR, FREQ, fill = 0) %>% 
            gather(CHAR, FREQ, -MONTH)
      if (nrow(data) == 0) {
            stop("nrow(data) == 0")
      }
      if (cum==TRUE) {
            data <- 
                  data %>% 
                  arrange(CHAR, MONTH) %>% 
                  group_by(CHAR) %>% 
                  mutate(FREQ=cumsum(FREQ)) %>% 
                  ungroup()
      }
      p <- plot_ly(data, x=~MONTH, y=~FREQ, color=~CHAR, colors=color_set,
                   type = "scatter",
                   mode = "markers+lines"
      )
      return(p)
}
# text_freq_keyword_month_trend(text_freq, search_keyword)


## 趨勢-每周關鍵字----
text_freq_keyword_week_trend <- function(data, search_keyword, cum=FALSE){
      require(tidyverse)
      require(stringr)
      require(plotly)
      
      search_keyword_t <- str_split(search_keyword, "\\|", simplify=TRUE)[, 1]
      if(length(search_keyword_t)>8){
            color_set <- rainbow(length(search_keyword_t))
      }else{
            color_set <- RColorBrewer::brewer.pal(length(search_keyword_t),  "Set2")
      }
      data <- 
            data %>% 
            filter(CHAR%in%search_keyword_t) %>% 
            mutate(WEEK=format(PUSH_DATE, "%Y%W")) %>% 
            group_by(CHAR, WEEK) %>% 
            summarise(FREQ=sum(FREQ)) %>% 
            ungroup() %>% 
            spread(CHAR, FREQ, fill = 0) %>% 
            gather(CHAR, FREQ, -WEEK)
      if(cum==TRUE){
            data <- 
                  data %>% 
                  arrange(CHAR, WEEK) %>% 
                  group_by(CHAR) %>% 
                  mutate(FREQ=cumsum(FREQ)) %>% 
                  ungroup()
      }
      p <- 
            plot_ly(data, x=~WEEK, y=~FREQ, color=~CHAR
                    , colors=color_set,
                    type = "scatter",
                    mode = "markers+lines"
            ) 
      return(p)
}
# text_freq_keyword_week_trend(text_freq, search_keyword)

## 趨勢-每日關鍵字----
text_freq_keyword_day_trend <- function(data, search_keyword, cum=FALSE){
      require(tidyverse)
      require(stringr)
      require(plotly)
      
      search_keyword_t <- str_split(search_keyword, "\\|", simplify=TRUE)[, 1]
      if(length(search_keyword_t)>8){
            color_set <- rainbow(length(search_keyword_t))
      }else{
            color_set <- RColorBrewer::brewer.pal(length(search_keyword_t),  "Set2")
      }
      data <- 
            data %>% 
            filter(CHAR%in%search_keyword_t) %>% 
            group_by(CHAR, PUSH_DATE) %>% 
            summarise(FREQ=sum(FREQ)) %>% 
            ungroup() %>% 
            spread(CHAR, FREQ, fill = 0) %>% 
            gather(CHAR, FREQ, -PUSH_DATE)
      if(cum==TRUE){
            data <- 
                  data %>% 
                  arrange(CHAR, PUSH_DATE) %>% 
                  group_by(CHAR) %>% 
                  mutate(FREQ=cumsum(FREQ)) %>% 
                  ungroup()
      }
      p <- 
            plot_ly(data, x=~PUSH_DATE, y=~FREQ, color=~CHAR
                    , colors=~color_set,
                    type = "scatter",
                    mode = "markers+lines"
            )
      return(p)
}
# text_freq_keyword_day_trend(text_freq, search_keyword)



#文本調查(輸入查詢之指定日期的關鍵字次數)----
article_search1 <- function(data, search_keyword, search_date_start, search_date_end){
      require(tidyverse)
      search_date <- seq.Date(as.Date(search_date_start), as.Date(search_date_end), "day")
      
      data <- 
            data%>% 
            filter(CHAR%in%unlist(str_split(search_keyword, "\\|")), 
                   as.Date(PUSH_DATE)%in%search_date
            ) %>% 
            arrange(CHAR, PUSH_DATE)
      return(data)
}
# article_search1(text_freq, search_keyword, search_date_start, search_date_end)





#文本調查(輸入查詢之指定日期的隨機n篇文章)----
article_search2 <- function(data, search_keyword, search_date_start, search_date_end, headn=50, sample_number=1){
      require(tidyverse)
      headf <- function(data, n=5){
            if(!"data.frame"%in%class(data))stop("data must be data.frame or its extend")
            return(data.frame(head(data, n)))
      }
      search_date <- seq.Date(as.Date(search_date_start), as.Date(search_date_end), "day")
      
      data <- 
            data %>% 
            semi_join(data %>% 
                            filter(as.Date(PUSH_TIME)%in%search_date, 
                                   str_detect(REAL_TEXT, 
                                              paste0(search_keyword, collapse="|")
                                   )
                            ) %>% 
                            sample_n(sample_number), 
                      by="ARTICLE_ID"
            ) %>% 
            select(REAL_TEXT, PUSH_TIME) %>% 
            headf(headn)
      return(data)
}
# article_search2(article_data, search_keyword, search_date_start, search_date_end)

