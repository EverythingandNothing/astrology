
#import csv of all signs + mbtis 
elements <- read.csv("elements.csv", stringsAsFactors = FALSE)

popavg   <- read.csv("mbtipopavg.csv", stringsAsFactors = FALSE)

data_raw <- read.csv("signmbti.csv", stringsAsFactors = FALSE)

# create data frame of all participants signs and letters split out

data <- data_raw %>%
            mutate(
              sign         = tolower(sign),
              sign_order   = Sign.Order,
              name         = Your.name..optional.,
              mbtiinterior = substr(mbti,2,3),
              mbti1        = substr(mbti,1,1),
              mbti2        = substr(mbti,2,2),
              mbti3        = substr(mbti,3,3),
              mbti4        = substr(mbti,4,4)
            )%>%
              select(
                name, 
                sign, 
                sign_order, 
                mbti, 
                mbtiinterior, 
                mbti1, 
                mbti2, 
                mbti3, 
                mbti4) %>%
              merge(elements, by = "sign", all.x = TRUE) %>%
              arrange(sign_order)

# create a new data frame of single letters based on population averages

mbti_letter_avg <- data.frame("letter" = c("e","n","t","j","st","sf","nt","nf"), stringsAsFactors = FALSE)

for(i in 1:nrow(mbti_letter_avg)){
  mbti_letter_avg$popavg[i]  <- data %>% filter(str_detect(tolower(mbti),mbti_letter_avg$letter[i])) %>% nrow() / nrow(data)
 
}
                        

#create new data frame based on signs and fill in w/ data on horoscope/ mbti

signs   <- data.frame("sign" = unique(data$sign), "ordering" = unique(data$sign_order), stringsAsFactors = FALSE) %>%
          merge(data %>% count(sign), by = 'sign') %>% rename(count = n) %>% # adding total count of each sign
          merge(data %>% count(sign,mbti1) %>% filter(mbti1 =="E") %>% select(-mbti1), by = 'sign', all.x = TRUE) %>% rename(e_count = n) %>%
          merge(data %>% count(sign,mbti2) %>% filter(mbti2 =="N") %>% select(-mbti2), by = 'sign', all.x = TRUE) %>% rename(n_count = n) %>%
          merge(data %>% count(sign,mbti3) %>% filter(mbti3 =="T") %>% select(-mbti3), by = 'sign', all.x = TRUE) %>% rename(t_count = n) %>%
          merge(data %>% count(sign,mbti4) %>% filter(mbti4 =="J") %>% select(-mbti4), by = 'sign', all.x = TRUE) %>% rename(j_count = n) %>%
          merge(data %>% count(sign,mbtiinterior) %>% filter(mbtiinterior =="ST") %>% select(-mbtiinterior), by = 'sign', all.x = TRUE) %>% rename(st_count = n) %>%
          merge(data %>% count(sign,mbtiinterior) %>% filter(mbtiinterior =="SF") %>% select(-mbtiinterior), by = 'sign', all.x = TRUE) %>% rename(sf_count = n) %>%     
          merge(data %>% count(sign,mbtiinterior) %>% filter(mbtiinterior =="NT") %>% select(-mbtiinterior), by = 'sign', all.x = TRUE) %>% rename(nt_count = n) %>%
          merge(data %>% count(sign,mbtiinterior) %>% filter(mbtiinterior =="NF") %>% select(-mbtiinterior), by = 'sign', all.x = TRUE) %>% rename(nf_count = n) %>%
          merge(elements, by = "sign")


elements <- data.frame("element" = unique(data$element), stringsAsFactors = FALSE) %>%
          merge(data %>% count(element), by = 'element') %>% rename(count = n) %>% # adding total count of each element
          merge(data %>% count(element,mbti1) %>% filter(mbti1 =="E") %>% select(-mbti1), by = 'element', all.x = TRUE) %>% rename(e_count = n) %>%
          merge(data %>% count(element,mbti2) %>% filter(mbti2 =="N") %>% select(-mbti2), by = 'element', all.x = TRUE) %>% rename(n_count = n) %>%
          merge(data %>% count(element,mbti3) %>% filter(mbti3 =="T") %>% select(-mbti3), by = 'element', all.x = TRUE) %>% rename(t_count = n) %>%
          merge(data %>% count(element,mbti4) %>% filter(mbti4 =="J") %>% select(-mbti4), by = 'element', all.x = TRUE) %>% rename(j_count = n) %>%
          merge(data %>% count(element,mbtiinterior) %>% filter(mbtiinterior =="ST") %>% select(-mbtiinterior), by = 'element', all.x = TRUE) %>% rename(st_count = n) %>%
          merge(data %>% count(element,mbtiinterior) %>% filter(mbtiinterior =="SF") %>% select(-mbtiinterior), by = 'element', all.x = TRUE) %>% rename(sf_count = n) %>%     
          merge(data %>% count(element,mbtiinterior) %>% filter(mbtiinterior =="NT") %>% select(-mbtiinterior), by = 'element', all.x = TRUE) %>% rename(nt_count = n) %>%
          merge(data %>% count(element,mbtiinterior) %>% filter(mbtiinterior =="NF") %>% select(-mbtiinterior), by = 'element', all.x = TRUE) %>% rename(nf_count = n) 

#take the data frame, convert all NA to 0, and then add in population averages of mbti and zscores relative to population averages

signs[is.na(signs)] <- 0

signs <- signs %>%
         mutate(
           e_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "e"],
           n_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "n"],
           t_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "t"],
           j_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "j"],
           sf_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "sf"],
           st_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "st"],
           nt_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "nt"],
           nf_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "nf"]
         ) %>%
          mutate(
            e_z       = round(zscore(count, e_pop_avg, e_count),1),
            n_z       = round(zscore(count, n_pop_avg, n_count),1),
            t_z       = round(zscore(count, t_pop_avg, t_count),1),
            j_z       = round(zscore(count, j_pop_avg, j_count),1),
            st_z      = round(zscore(count, sf_pop_avg, sf_count),1),
            sf_z      = round(zscore(count, st_pop_avg, st_count),1),
            nt_z      = round(zscore(count, nt_pop_avg, nt_count),1),
            nf_z      = round(zscore(count, nf_pop_avg, nf_count),1)
          ) 

row.names(signs) <- signs$sign

heatmap_data     <- signs %>%
                select(
                  e_z,      
                  n_z,     
                  t_z,      
                  j_z,     
                  st_z,   
                  sf_z,   
                  nt_z,      
                  nf_z
                ) %>% 
                  rename(
                  e  = e_z,      
                  n  =  n_z,     
                  t  =  t_z,      
                  j  = j_z,     
                  st =  st_z,   
                  sf =  sf_z,   
                  nt =  nt_z,      
                  nf =  nf_z                    
                  ) %>%
                 data.matrix() 

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

col_breaks  <- c(seq(-4,-1.5,length=100),   # for red
               seq(-1.49,1.49,length=100),  # for yellow
               seq(1.5,4,length=100))     # for green


heatmap           <-  heatmap_data %>% 
                      heatmap.2(
                        cellnote     = heatmap_data,
                        main         = "Z score of Letters v Pop Avg",
                        notecol      = "black",
                        density.info = "none",
                        trace        = "none",
                        Colv         = FALSE,
                        Rowv         = FALSE,
                        breaks       = col_breaks,
                        col          = my_palette
                        )

        
write.csv(signs, file = "signs.csv") #save this down cuz it's useful
  
# doing same heatmap but cutting it by element now instead of sign

elements <- elements %>%
  mutate(
    e_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "e"],
    n_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "n"],
    t_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "t"],
    j_pop_avg  = mbti_letter_avg$popavg[mbti_letter_avg$letter == "j"],
    sf_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "sf"],
    st_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "st"],
    nt_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "nt"],
    nf_pop_avg = mbti_letter_avg$popavg[mbti_letter_avg$letter == "nf"]
  ) %>%
  mutate(
    e_z       = round(zscore(count, e_pop_avg, e_count),1),
    n_z       = round(zscore(count, n_pop_avg, n_count),1),
    t_z       = round(zscore(count, t_pop_avg, t_count),1),
    j_z       = round(zscore(count, j_pop_avg, j_count),1),
    st_z      = round(zscore(count, sf_pop_avg, sf_count),1),
    sf_z      = round(zscore(count, st_pop_avg, st_count),1),
    nt_z      = round(zscore(count, nt_pop_avg, nt_count),1),
    nf_z      = round(zscore(count, nf_pop_avg, nf_count),1)
  ) 

row.names(elements) <- elements$element

heatmap_data     <- elements %>%
                      select(
                              e_z,      
                              n_z,     
                              t_z,      
                              j_z,     
                              st_z,   
                              sf_z,   
                              nt_z,      
                              nf_z
                        ) %>% 
                       rename(
                              e  = e_z,      
                              n  =  n_z,     
                              t  =  t_z,      
                              j  = j_z,     
                              st =  st_z,   
                              sf =  sf_z,   
                              nt =  nt_z,      
                              nf =  nf_z                    
                        ) %>%
                        data.matrix() 

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

col_breaks  <- c(seq(-4,-1.5,length=100),   # for red
                 seq(-1.49,1.49,length=100),  # for yellow
                 seq(1.5,4,length=100))     # for green


heatmap           <-  heatmap_data %>% 
  heatmap.2(
    cellnote     = heatmap_data,
    main         = "Z score of Letters v Pop Avg",
    notecol      = "black",
    density.info = "none",
    trace        = "none",
    Rowv         = FALSE,
    Colv         = FALSE,
    breaks       = col_breaks,
    col          = my_palette
  )



# I want to graph the data in a heatmap, but above i cut it by sign which isn't good for heatmap funciton, so i'm going to change the shape then do it

# sign_vector   <- rep(signs$sign,length(mbti_letter_avg$letter))
# letter_vector <- rep(mbti_letter_avg$letter, each = length(signs$sign))
# heatmap_data  <- data.frame("sign" = sign_vector, "letter" = letter_vector, stringsAsFactors = FALSE)%>%
#                  merge(mbti_letter_avg, by = "letter", all.x = TRUE) 
# 
# data$mbti     <- tolower(data$mbti)
# 
# for(i in 1:nrow(heatmap_data)){
#   
#   heatmap_data$sign_count[i]   <- data %>% count(sign) %>% filter(sign == heatmap_data$sign[i]) %$% n %>% sum()
#   heatmap_data$letter_count[i] <- data %>% count(sign,mbti) %>% filter(sign == heatmap_data$sign[i] & str_detect(mbti, heatmap_data$letter[i])) %$% n %>% sum()
#   
#   
# }
# 
# 
# 
# heatmap_data <- heatmap_data %>% 
#            mutate(
#              z_score = zscore(sign_count, popavg,letter_count)
#            ) %>%
#            select(
#              sign,
#              letter,
#              z_score
#            ) %>%
#            heatmap(heatmap_data)
#           




#graph data
