# PTT_Crawler
Function for crawl the PTT

## Source
```r
source("./function_ptt_crawler.R", encoding = "UTF-8")
```
<br>

## Crawler
Use the function named ‘ptt_crawler_list’ that crawl data from the selected board of PTT. Or use ‘ptt_crawler_download’ to download th data.

For examle, crawl the data from board of keyboard and mouse.

```r
result <- ptt_crawler_list(search_board = "Key_Mou_Pad",
                           search_range = 10
                           )
```
```r
## PTT-Key_Mou_Pad's Crawler
## Page from 1804 to 1813 
## 
## Go!!
## 
## current page  1813 ( 1 / 10 ) Done!     Time difference of 12.70118 secs
## current page  1812 ( 2 / 10 ) Done!     Time difference of 7.225601 secs
## current page  1811 ( 3 / 10 ) Done!     Time difference of 0.05406213 secs
## current page  1810 ( 4 / 10 ) Done!     Time difference of 8.983113 secs
## current page  1809 ( 5 / 10 ) Done!     Time difference of 9.372365 secs
## current page  1808 ( 6 / 10 ) Done!     Time difference of 6.60418 secs
## current page  1807 ( 7 / 10 ) Done!     Time difference of 7.983021 secs
## current page  1806 ( 8 / 10 ) Done!     Time difference of 2.155539 secs
## current page  1805 ( 9 / 10 ) Done!     Time difference of 4.546267 secs
## current page  1804 ( 10 / 10 ) Done!     Time difference of 13.4549 secs
## current article  M.1524315117.A.42E ( 1 / 198 ) Done!     Time difference of 3.880823 secs
## current article  M.1514637092.A.344 ( 2 / 198 ) Done!     Time difference of 9.644326 secs
## current article  M.1479447545.A.DED ( 3 / 198 ) Done!     Time difference of 3.502784 secs
## ...
## ...
```

We can get the data.

```r
head(result$article_title, 4)
```
```r
## $CRAWLING_TIME
## [1] "2018-05-29 23:48:25 CST"
## 
## $SUMMARY
## # A tibble: 198 x 8
##    ARTICLE_ID         URL       TITLE    PUSH  DATE  AUTHOR ORDER KEY_WORD
##    <chr>              <chr>     <chr>    <chr> <chr> <chr>  <int>    <dbl>
##  1 M.1525099000.A.08A https://~ [鍵盤] 太豪~ 10    4/30  stand~     1        0
##  2 M.1525101431.A.E3D https://~ [鍵盤] 自組~ 53    4/30  h15475     2        0
##  3 M.1525137903.A.C6F https://~ [閒聊] 鍵帽~ 11    5/01  olite      3        0
##  4 M.1525148222.A.A6C https://~ [選購] 手小~ 20    5/01  blues~     4        0
##  5 M.1525163897.A.507 https://~ [版友] 自介~ 2     5/01  RYoYo~     5        0
##  6 M.1525166596.A.D4E https://~ [鍵盤] SP~ 12    5/01  angel~     6        0
##  7 M.1525167696.A.3D8 https://~ [鍵盤] GM~ 8     5/01  angel~     7        0
##  8 M.1525168834.A.A0D https://~ [鍵盤] G8~ 20    5/01  kira3~     8        0
##  9 M.1525173210.A.18B https://~ [鍵盤] Je~ 12    5/01  angel~     9        0
## 10 M.1525185021.A.901 https://~ [鍵盤] ZO~ 16    5/01  angel~    10        0
## # ... with 188 more rows
## 
## $M.1524315117.A.42E
## # A tibble: 165 x 2
##    TEXT                                                       TYPE        
##    <chr>                                                      <chr>       
##  1 Key_Mou_Pad                                                BOARD       
##  2 kira3628800                                                ARTICLE_AUT~
##  3 [團購] BSP Round 4台灣團                                   ARTICLE_TIT~
##  4 4/21                                                       ARTICLE_DATE
##  5 M.1524315117.A.42E                                         ARTICLE_ID  
##  6 https://www.ptt.cc/bbs/Key_Mou_Pad/M.1524315117.A.42E.html URL         
##  7 ---------                                                  BREAK_LINE  
##  8 作者kira3628800 (kira10!)                                  TITLE      
## ...
## ...
```

<br>

## Data tidy
We have the data that is ‘list’ for format. Now we must transform to ‘data.frame’. You can follow as below:

```r
# fixed articles at the bottom of list.
under_article <- 
      result$article_title$SUMMARY %>% 
      filter(ORDER <= 0) %>% 
      .$ARTICLE_ID

under_article
```
```r
## [1] "M.1391431710.A.EA2" "M.1455092545.A.6D6" "M.1479447545.A.DED"
## [4] "M.1514637092.A.344" "M.1524315117.A.42E"
```

```r
# Choose the main article and target period.
article_data <- result$article_title
article_data <- 
      article_data[!names(article_data) %in% 
                         c("CRAWLING_TIME", "SUMMARY", under_article)
                   ]
article_data <- 
      article_data %>% 
      article_combined(as.Date("2018-05-01"),
                       as.Date("2018-05-25")
                       )
head(article_data, 20)
```
```r
## # A tibble: 20 x 3
##    REAL_TEXT                                      PUSH_TIME ARTICLE_ID    
##    <chr>                                          <chr>     <chr>         
##  1 [滑鼠] 硬派g304/1290元                         2018-05-~ M.1527251705.~
##  2 https://i.imgur.com/aoqqopg.jpg                2018-05-~ M.1527251705.~
##  3 ""                                             2018-05-~ M.1527251705.~
##  4 台南買不到990的可以考慮看看                    2018-05-~ M.1527251705.~
##  5 ""                                             2018-05-~ M.1527251705.~
##  6 不過我看到重量就滅火了xd，滑鼠本身就重99克，加3號電池上去大概跟g502本體差不多~ 2018-05-~ M.1527251705.~
##  7 重量，打fps會hen累。                           2018-05-~ M.1527251705.~
##  8 ""                                             2018-05-~ M.1527251705.~
##  9 ""                                             2018-05-~ M.1527251705.~
## 10 ""                                             2018-05-~ M.1527251705.~
## 11 --                                             2018-05-~ M.1527251705.~
## 12 " 坐等990"                                     2018-05-~ M.1527251705.~
## 13 " 為什麼你會覺得不含電池是99克 lol"            2018-05-~ M.1527251705.~
## 14 " 含電池99吧。。。"                            2018-05-~ M.1527251705.~
## 15 " 加電池99g啦"                                 2018-05-~ M.1527251705.~
## 16 " 可能是指不含線材吧"                          2018-05-~ M.1527251705.~
## 17 " 囧 但這隻也沒雙模 可能筆誤惹"                2018-05-~ M.1527251705.~
## 18 " https://i.imgur.com/gtfvhg1.jpg 自己看吧"    2018-05-~ M.1527251705.~
## 19 " 加蓋子99g"                                   2018-05-~ M.1527251705.~
## 20 " 重重鼠我愛"                                  2018-05-~ M.1527251705.~
## ...
## ...
```

<br>

## Text mining
### Segment
We provide easy function to split the sentence. (Using jiebaR package)

```r
text_data <- 
      article_cut(article_data,
                  jiebaR::worker(bylines = TRUE),
                  "電競"
                  )
text_data[1]
```
```r
## $`DATE2018-05-15`
##  [1] "滑鼠"     "acer"     "pmw"      "的"       "評價"     "前"      
##  [7] "幾天"     "逛三創"   "看到"     "的"       "也"       "有"      
## [13] "現在"     "電競鼠"   "必備"     "的"       "閃閃"     "rgb"     
## [19] "用"       "的"       "是雙"     "歐姆龍"   "的"       "微動"    
## [25] "開關"     "還有"     "側鰭"     "可以"     "調整"     "握感"    
## [31] "價錢"     "還比"     "同級"     "的"       "蛇"       "便宜"    
## [37] "有點"     "想"       "買來"     "取代"     "手上"     "微動"    
## [43] "開關"     "快"       "爛掉"     "的"       "蛇"       "已經"    
## [49] "換過"     "一次"     "了"       "想"       "問問"     "版上"    
## [55] "有沒有"   "這"       "隻"       "鼠"       "心得"     "可以"    
## [61] "分享"     "一下"     "感謝"     "板上"     "搜"       "predator"
## [67] "cestus"   "有"       "一篇"     "k"        "價格"     "算還"    
## [73] "不錯"     "有人"     "cue"      "我"       "嗎"       "我"      
## [79] "有"       "寫"       "一篇"     "阿"       "握感"     "不錯"    
## [85] "好"       "滑鼠"     "不握"     "嗎"
```

### Frequency of word
```r
text_freq <- freq_combined(text_data)

head(text_freq)
```
```r
## # A tibble: 6 x 3
##   CHAR  PUSH_DATE   FREQ
##   <chr> <date>     <dbl>
## 1 的    2018-05-21   132
## 2 https 2018-05-21    91
## 3 com   2018-05-21    90
## 4 imgur 2018-05-21    82
## 5 i     2018-05-21    72
## 6 jpg   2018-05-21    72
```

You can input the ‘search_keyword’ to insteading some words.

```r
text_freq <- 
      freq_combined(text_data,
                    search_keyword = c("https|com", "jpg|imgur")
                    )
head(text_freq)
```
```r
## # A tibble: 6 x 3
##   CHAR  PUSH_DATE   FREQ
##   <chr> <date>     <dbl>
## 1 https 2018-05-21   181
## 2 jpg   2018-05-21   154
## 3 的    2018-05-21   132
## 4 i     2018-05-21    72
## 5 滑鼠  2018-05-21    33
## 6 在    2018-05-21    27
```

You can also limit the length of number of characters and remove selected word.

```r
text_freq <- 
      text_remove_word(text_freq,
                       char_min = 2,
                       remove_word = c("https")
                       )
head(text_freq)
```
```r
## # A tibble: 6 x 3
##   CHAR   PUSH_DATE   FREQ
##   <chr>  <date>     <dbl>
## 1 jpg    2018-05-21   154
## 2 滑鼠   2018-05-21    33
## 3 rgb    2018-05-21    22
## 4 hyperx 2018-05-21    21
## 5 alloy  2018-05-21    18
## 6 使用   2018-05-21    17
```

### Graphs
Finally, we provide some function to ploting. For example, wordcloud

```r
library(wordcloud2)

text_freq_cloud(text_freq,
                top = 300,
                size = 1.5,
                minSize = 0,
                shuffle = TRUE,
                rotateRatio = 0,
                version = 2
                )
```

Or daily trend for keyword

```r
text_freq_keyword_day_trend(text_freq,
                            search_keyword = c("鍵盤", "滑鼠"),
                            cum = FALSE
                            )
```



