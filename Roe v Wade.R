

#-------Roe v Wade Agenda Setting Analysis-------


#Enter bearer token
bearer_token = ''   #Enter Twitter Academic API bearer token here

#Load libraries
library(academictwitteR)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tm)
library(ggplot2)
library(rtweet)
library(stringr)
library(SnowballC)


#-------Collect data-------


news <- get_all_tweets(users = c("FoxNews", "CNN", "ABC"),
                       start_tweets = "2022-04-25T04:00:00Z", #When the leak happened 
                       end_tweets = "2022-07-2T04:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "~/Desktop/roe",
                       n = 1000000)
#save(news, file = "roe_raw.RData")
roe <- bind_tweets(data_path = "~/Desktop/roe")
news_data <- roe


#------creating useful features-------


#Create channel names column to identify the tweets easier
authors <- news_data$author_id # Assign an object to retrieve and store author ids
authors_profile <- get_user_profile(authors, bearer_token) # We can use the get_user_profile() function to retrieve the user profiles.
names(authors_profile)
authors_profile2 <- authors_profile %>% select(name)# Subset the name column and combine with original data as a column.
news_data <- cbind(news_data, authors_profile2)
table(news_data$name)# Check number of tweets in each channel

#Create a new column to filter Roe v Wade/abortion tweets
news_data$text_lower=tolower(news_data$text) #create column with all lower case text for the tweets.
abortionwords=c("abortion", "anti-abortion", "pro-life", "pro life", "prolife", "pro-choice", "pro choice", "prochoice", "reproduct", 
                " roe ","roe v. wade", "roe v wade","roe vs. wade","roe vs wade","contracept", "pregnan", "fetus", "unborn", "birth control", "embryo", "womb", 
                "miscarriage", "family planning", "vasectom", "trimester", "planned parenthood", "plan b ", "feticide", " dobbs ", 
                "v. jackson", "v jackson", "vs jackson", "vs. jackson", "in utero", " alito ", "infanticide", "intact dilation", 
                "baby killing", "killing bab", "ectopic pregnanc", "mifepristone", "misoprostol", "right to life", 
                "vacuum aspiration", "body autonomy", "bodily autonomy", "female autonomy", "forced birth", "woman's autonomy", 
                "women's autonomy", "womens autonomy", "gameprost", "heartbeat bill", "heartbeat law", "partial birth", 
                "partial-birth","supreme court leak","leaked supreme court","scotus leak","leaked scotus") #create object with abortion keywords
news_data$abortion_post=NA #create column to filter for abortion tweets
news_data$abortion_post=ifelse(str_detect(news_data$text_lower,paste(abortionwords,collapse="|")),"1","0")
table(news_data$abortion_post)

#create new date column with only date
news_data$Date=as.Date(news_data$created_at)

#Create new week column to aggregate tweets by week
weeks <- data.frame(names = c("Apr 25", "May 2", "May 9", "May 16", "May 23", "May 30", "June 6", "June 13", "June 20", "June 27", "July 4"),
                    num = seq(1:11)) #create dictionary to name weeks
weeks$num <- as.factor(weeks$num)
news_data <- distinct(news_data, id, .keep_all = TRUE) #Remove duplicate tweets from ids
channel_counts <- news_data %>% count(name, id) %>%  # Count tweets by channel
  tidyr::pivot_wider(names_from = name, values_from = n, values_fill = 0) # Aggregate to channel
news_data <- news_data %>% 
  mutate(week = cut.Date(Date, breaks = "week", labels = FALSE, start.on.monday = F)) %>% 
  arrange(Date)
news_data <- merge(news_data,channel_counts,by="id",all.x=T,all.y=F)


#------- Save/ Load file -------


#save(news_data, file = "roe.RData")
#load("roe.RData")


#------- Descriptive Analysis -------


#Plot frequency of tweets
ggplot(news_data, aes(x=Date)) +
  geom_line(stat = "count", size = 0.5, color = "blue") +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=550, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=550, 
            colour="black", angle=90, size=3) + 
  ylim (0, 600) +
  labs(
    x = "Date", y = "Number of tweets",
    title = "Total number of tweets across time",
    subtitle = "Tweets aggregated per day"
  )

#Plot frequency of tweets per channel
ggplot(news_data, aes(x=Date, colour = name, group = name)) +
  geom_line(stat = "count", size = 0.5) +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=300, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=300, 
            colour="black", angle=90, size=3) + 
  ylim (0, 320) +
  labs(x = "Date", y = "Number of tweets",
       title = "Total number of tweets per channel",
       subtitle = "Tweets aggregated per day",
       col = "Channels")

#Plot total vs abortion tweets
ggplot(news_data, aes(x=Date, colour = abortion_post, group = abortion_post)) +
  geom_line(stat = "count", size = 0.5) +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=470, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=470, 
            colour="black", angle=90, size=3) + 
  ylim (0, 500) +
  labs(x = "Date", y = "Number of tweets",
       title = "Total vs Abortion tweets across time",
       subtitle = "Tweets aggregated per day",
       col = "Abortion tweet") +
  scale_color_hue(labels=c("No","Yes"))

#Plot abortion
#Subset abortion only
abortion <- news_data %>%
  subset(news_data$abortion_post == "1") %>%
  select(Date, name, abortion_post)
ggplot(abortion, aes(x=Date)) +
  geom_line(stat = "count", size = 0.5, colour="red") +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=175, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=175, 
            colour="black", angle=90, size=3) + 
  ylim(0,200) +
  labs(x = "Date", y = "Number of abortion related tweets",
       title = "Total number of abortion tweets",
       subtitle = "Tweets aggregated per day")

#Plot separately for each channel
fox <- news_data %>%
  subset(news_data$name == "Fox News") %>%
  select(Date, text, abortion_post)
cnn <- news_data %>%
  subset(news_data$name == "CNN") %>%
  select(Date, text, abortion_post)
abc <- news_data %>%
  subset(news_data$name == "ABC News") %>%
  select(Date, text, abortion_post)

#Plot fox
ggplot(fox, aes(x=Date, colour = abortion_post, group = abortion_post)) +
  geom_line(stat = "count", size = 0.5) +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=250, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=250, 
            colour="black", angle=90, size=3) + 
  labs(x = "Date", y = "Number of Tweets", col = "Abortion",
       title = "Total number of tweets by Fox News",
       subtitle = "Tweets aggregated per day")+
  scale_color_hue(labels=c("No","Yes"))
#Plot cnn
ggplot(cnn, aes(x=Date, colour = abortion_post, group = abortion_post)) +
  geom_line(stat = "count", size = 0.5) +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=120, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=120, 
            colour="black", angle=90, size=3) +
  ylim (0,130) +
  labs(x = "Date", y = "Number of Tweets", col = "Abortion",
       title = "Total number of tweets by CNN",
       subtitle = "Tweets aggregated per day")+
  scale_color_hue(labels=c("No","Yes"))
#Plot abc
ggplot(abc, aes(x=Date, colour = abortion_post, group = abortion_post)) +
  geom_line(stat = "count", size = 0.5) +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=120, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=120, 
            colour="black", angle=90, size=3) +
  labs(x = "Date", y = "Number of Tweets", col = "Abortion",
       title = "Total number of tweets by ABC",
       subtitle = "Tweets aggregated per day")+
  scale_color_hue(labels=c("No","Yes"))

#Plot all on one graph
ggplot(abortion, aes(x=Date, colour = name, group = name)) +
  geom_line(stat = "count", size = 0.5) +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=105, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=105, 
            colour="black", angle=90, size=3) + 
  ylim(0,110) +
  labs(x = "Date", y = "Number of abortion related tweets",
       title = "Abortion related tweets per channel",
       subtitle = "Tweets aggregated per day",
       col = "Channels")

#Plot all on one graph
#Create numeric variables
news_data$name2=ifelse(news_data$name=="Fox News",1,
                       ifelse(news_data$name=="ABC News",2,3))
table(news_data$abortion_post)
table(news_data$name)
table(news_data$name2)

ggplot(news_data, aes(x=Date, colour = name, shape = abortion_post)) +
  geom_line(stat = "count", size = .5) +
  geom_point(stat = "count", size = 1) +
  geom_vline(xintercept = as.integer(as.Date("2022-05-02")), linetype='dotted') + 
  geom_vline(xintercept = as.integer(as.Date("2022-06-24")), linetype='dotted') + 
  geom_text(x=as.integer(as.Date("2022-05-02")), label="\nLeak", y=270, 
            colour="black", angle=90, size=3) +
  geom_text(x=as.integer(as.Date("2022-06-24")), label="\nDecision", y=270, 
            colour="black", angle=90, size=3) + 
  ylim(0,300) +
  labs(x = "Date", y = "Number of tweets",
       title = "Abortion related tweets per channel",
       subtitle = "Tweets aggregated per day",
       col = "Channels",
       shape = "Abortion tweets")+
  scale_shape(labels=c("No","Yes"))


#-------Frequencies-------


#Subset tweets only from May 2 to Jun 23.
roe2 <- news_data[news_data$Date >= "2022-5-2" & news_data$Date <= "2022-6-23", ]

#Create new week column to aggregate tweets by week for only selected weeks
roe2 <- roe2[1:(length(roe2)-4)] #Remove old columns
weeks <- data.frame(names = c("May 2", "May 9", "May 16", "May 23", "May 30", "June 6", "June 13", "June 20", "June 27"),
                    num = seq(1:9)) #create dictionary to name weeks
weeks$num <- as.factor(weeks$num)
channel_counts <- roe2 %>% count(name, id) %>%  # Count tweets by channel
  tidyr::pivot_wider(names_from = name, values_from = n, values_fill = 0) # Aggregate to channel
roe2 <- roe2 %>% 
  mutate(week = cut.Date(Date, breaks = "week", labels = FALSE, start.on.monday = F)) %>% 
  arrange(Date)
roe2 <- merge(roe2,channel_counts,by="id",all.x=T,all.y=F)

#Percentage of abortion posts to non-abortion posts
table(roe2$abortion_post)
total_ratio = 888 / (888+19256)

#Subset abortion posts for each channel
fox <- roe2 %>%
  subset(roe2$name == "Fox News") %>%
  select(Date, text, abortion_post)
cnn <- roe2 %>%
  subset(roe2$name == "CNN") %>%
  select(Date, text, abortion_post)
abc <- roe2 %>%
  subset(roe2$name == "ABC News") %>%
  select(Date, text, abortion_post)

table(fox$abortion_post)
fox_total = 543+10094
fox_ratio = 543 / (fox_total)

table(cnn$abortion_post)
cnn_total = 148+4293
cnn_ratio = 148 / (cnn_total)

table(abc$abortion_post)
abc_total = 197+4869
abc_ratio = 197 / (abc_total)

prop_data<-news_data


#-------Topic Modeling-------


library(quanteda)
library(stm)
#install.packages("stopwords")
library(stopwords)
library(Rtsne)
#install.packages("rsvd")
library(rsvd)
#install.packages("geometry")
library(geometry)


#Select data
news_data <- roe2

#Preprocessing
news_data$text <- gsub("(https|http|www)\\S+", "", news_data$text) #Remove hyperlinks
news_data$text <- gsub("(@)\\S+", "", news_data$text) #Remove mentions of twitter ids
#news_data <- distinct(news_data, text, .keep_all = TRUE) #Remove duplicate tweets if any
corp_quanteda <- corpus(news_data,
                        docid_field="id",
                        meta=c("week","name"),
                        text_field="text") 
tok <- quanteda::tokens(corp_quanteda, 
              remove_numbers=T, 
              remove_punct=T,
              remove_symbols=T) %>% 
  tokens_remove(c(stopwords::stopwords(source = "snowball"), "et", "pt", "p.m", "a.m", "rt", "amp", "cnn", "abc", "fox")) %>% 
  #tokens_wordstem() %>% 
  tokens_tolower() %>% 
  tokens_ngrams(1)
topic_dfm <- dfm(tok)
topic_dfm <- dfm_trim(topic_dfm, min_termfreq = .95, termfreq_type = "quantile", verbose = TRUE)
#topic_dfm <- dfm_trim(topic_dfm, max_termfreq = .95, termfreq_type = "quantile", verbose = TRUE)
topic_dfm_stm <- quanteda::convert(topic_dfm, to = "stm")
topfeatures(topic_dfm, n=50)

#Prepare documents for model fit
out <- prepDocuments(topic_dfm_stm$documents, topic_dfm_stm$vocab, topic_dfm_stm$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#Finding number of topics

#K=0 method
#fit_stm0 <- stm(documents = out$documents, 
#                vocab = out$vocab,
#                prevalence=~ name,
#                K = 0,
#                max.em.its = 20,
#                data = out$meta,
#                init.type = "Spectral", verbose = TRUE)
#labelTopics(fit_stm0)
#plot.STM(fit_stm0,labeltype="frex",topics=seq(1,49),n=10) #Model converged on k=49

#Compare models with different number of topics to select no. of topics
#K<-c(10,20,30,40,50) #Enter number of topics that you want to compare
#set.seed(02138)
#kresult <- searchK(out$documents, 
#                   out$vocab,
#                   prevalence=~ name,
#                   K, 
#                   data = out$meta,
#                   max.em.its=10, 
#                   init.type = "Spectral")
#plot(kresult)
# HOL and coherence should be high. Residuals should be low.
# In short, HOL and Residuals tell us about model fit, SC and exclusivity tell us about quality of topics.

#K1<-c(15,16,17,18,19,20) #Enter number of topics that you want to compare
#set.seed(02138)
#kresult1 <- searchK(out$documents, 
#                   out$vocab,
#                   prevalence=~ name,
#                   K1, 
#                   data = out$meta,
#                   max.em.its=10, 
#                   init.type = "Spectral")
#plot(kresult1)

#Fit model with k topics
#fit_stm17 <- stm(documents = out$documents, 
#                 vocab = out$vocab,
#                 K = 17,
#                 max.em.its = 50,
#                 prevalence =~ name,
#                 data = out$meta,
#                 init.type = "Spectral",
#                 verbose = TRUE)
#labelTopics(fit_stm17, n=10)
#plot.STM(fit_stm17,labeltype="frex",topics=seq(1,17),n=10,main= "Topic Proportions")
#exclusivity(fit_stm17)

#fit_stm18 <- stm(documents = out$documents, 
#                 vocab = out$vocab,
#                 K = 18,
#                 max.em.its = 50,
#                 prevalence =~ name,
#                 data = out$meta,
#                 init.type = "Spectral",
#                 verbose = TRUE)
#labelTopics(fit_stm18, n=10)
#plot.STM(fit_stm18,labeltype="frex",topics=seq(1,18),n=10,main= "Topic Proportions")
#exclusivity(fit_stm18)

fit_stm19 <- stm(documents = out$documents, 
                 vocab = out$vocab,
                 K = 19,
                 max.em.its = 50,
                 prevalence =~ name,
                 data = out$meta,
                 init.type = "Spectral",
                 verbose = TRUE)
labelTopics(fit_stm19, n=10)
plot.STM(fit_stm19,labeltype="frex",topics=seq(1,19),n=10,main= "Topic Proportions")
exclusivity(fit_stm19)
fit_stm19$eta
proportions_table <- make.dt(fit_stm19)
summary <- summarize_all(proportions_table, mean)
data.frame(summary)

#fit_stm20 <- stm(documents = out$documents, 
#                 vocab = out$vocab,
#                 K = 20,
#                 max.em.its = 50,
#                 prevalence =~ name,
#                 data = out$meta,
#                 init.type = "Spectral",
#                 verbose = TRUE)
#labelTopics(fit_stm20, n=10)
#plot.STM(fit_stm20,labeltype="frex",topics=seq(1,20),n=10,main= "Topic Proportions")
#exclusivity(fit_stm20)

#Plot coherence and exclusivity to select best model
library(ggplot2)
library(plotly)
#Exc17<-as.data.frame(cbind(c(1:17),exclusivity(fit_stm17), semanticCoherence(model=fit_stm17, docs), "Mod17"))
#Exc18<-as.data.frame(cbind(c(1:18),exclusivity(fit_stm18), semanticCoherence(model=fit_stm18, docs), "Mod18"))
#Exc19<-as.data.frame(cbind(c(1:19),exclusivity(fit_stm19), semanticCoherence(model=fit_stm19, docs), "Mod19"))
#Exc20<-as.data.frame(cbind(c(1:20),exclusivity(fit_stm20), semanticCoherence(model=fit_stm20, docs), "Mod20"))
#Exc<-rbind(Exc17,Exc18,Exc19,Exc20)
#colnames(Exc)<-c("K","Exclusivity", "SemanticCoherence", "Model")
#Exc$Exclusivity<-as.numeric(as.character(Exc$Exclusivity))
#Exc$SemanticCoherence<-as.numeric(as.character(Exc$SemanticCoherence))
#options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
#plotexc<-ggplot(Exc, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
#  geom_text(aes(label=K), nudge_x=.01, nudge_y=.01)+
#  labs(x = "Semantic coherence",
#       y = "Exclusivity",
#       title = "Comparing exclusivity and semantic coherence")
#plotexc
#dev.off() #run this if the plot above doesn't work

#Find documents associated with the topics
findThoughts(fit_stm19, texts = meta$text, n=10)

#Visualize find thoughts
thoughtsRoe <- findThoughts(fit_stm19, texts = meta$text, n=3, topics = 12, thresh = .7)$docs[[1]]
thoughtsRoe
par(mfrow = c(1, 1),mar = c(2, 2, 2, 2))
plotQuote(thoughtsRoe, width = 60, main = "Tweets in Topic: Roe v Wade")

#Wordcloud for one specific topic
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
cloud(fit_stm19, topic=12,colors=brewer.pal(8, "Dark2"))

#Testing differences accross channels
prep<-estimateEffect(1:19~ name, fit_stm19, meta=out$meta, uncertainty="Global", nsims=100)
sumprep<-summary(prep)
sumprep
par(mar = c(4, 10, 4, 4))
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(12), nsims = 100, ci.level=.95, 
                    xlab="Abortion Topic Proportion", main="Proportion of abortion topic across channels",
                    labeltype = "custom", custom.labels = c("CNN", "ABC News", "Fox News"))
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(2), nsims = 100, ci.level=.95, xlab="Topic Proportion", main="Group differences accross Topic 2: Election" )
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(4), nsims = 100, ci.level=.95, xlab="Topic Proportion", main="Group differences accross Topic 4: School shooting" )
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(5), nsims = 100, ci.level=.95, xlab="Topic Proportion", main="Group differences accross Topic 5: Inflation" )
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(6), nsims = 100, ci.level=.95, xlab="Topic Proportion", main="Group differences accross Topic 6: Crime" )
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(11), nsims = 100, ci.level=.95, xlab="Topic Proportion", main="Group differences accross Topic 11: Health" )
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(15), nsims = 100, ci.level=.95, xlab="Topic Proportion", main="Group differences accross Topic 15: Jan 6th" )
plot.estimateEffect(prep, model=fit_stm19, covariate="name", topics=c(18), nsims = 100, ci.level=.95, xlab="Topic Proportion", main="Group differences accross Topic 18: Gun control" )

plot(prep, covariate = "name", topics = c(12,2,4,5,6,11,15,18), model = fit_stm19, method = "difference", 
     cov.value1 = "Fox News", cov.value2 = "CNN", main = "Difference in topic proportions between Fox News and CNN",
     xlim = c(-0.1, 0.1),labeltype = "custom", ci.level=.95, custom.labels = c("Abortion", "Election", "School shooting","Inflation","Crime","Health","Jan 6th","Gun control"),
     xlab = "Higher proportion for CNN ............................. Higher proportion for Fox News",)

plot(prep, covariate = "name", topics = c(12,2,4,5,6,11,15,18), model = fit_stm19, method = "difference", 
     cov.value1 = "Fox News", cov.value2 = "ABC News", main = "Difference in topic proportions between Fox News and ABC News",
     xlim = c(-0.1, 0.1),labeltype = "custom", ci.level=.95, custom.labels = c("Abortion", "Election", "School shooting","Inflation","Crime","Health","Jan 6th","Gun control"),
     xlab = "Higher proportion for ABC News ........................... Higher proportion for Fox News",)

plot(prep, covariate = "name", topics = c(12,2,4,5,6,11,15,18), model = fit_stm19, method = "difference", 
     cov.value1 = "ABC News", cov.value2 = "CNN", main = "Difference in topic proportions between ABC News and CNN",
     xlim = c(-0.1, 0.1),labeltype = "custom", ci.level=.95, custom.labels = c("Abortion", "Election", "School shooting","Inflation","Crime","Health","Jan 6th","Gun control"),
     xlab = "Higher proportion for CNN ............................. Higher proportion for ABC News",)


#-------Visualization of topic proportions with time-------


library(stminsights)
stmeffect<-estimateEffect(formula=1:19~name*s(week),
                           stmobj=fit_stm19,metadata=topic_dfm_stm$meta) #smooth the effect of week
sumstmeffect<-summary(stmeffect)
sumstmeffect
### Plot Roe v Wade Topic over time by news channel
par(mfrow = c(1, 1),mar = c(4, 2, 4, 2))
plot.estimateEffect(stmeffect,  #Topic proportions in Fox tweets
                    covariate="week",
                    model=fit_stm19,
                    topics=stmeffect$topics[12],
                    method="continuous",
                    xlab="Week",
                    ylab="Topic Proportions",
                    main="Abortion topic proportion over time",
                    moderator="name",
                    moderator.value="Fox News",
                    ylim=c(0,.15),xlim=c(1,8),
                    linecol="red",
                    printlegend=F)
plot.estimateEffect(stmeffect,  #Topic proportions in ABC tweets
                    covariate="week",
                    model=fit_stm19,
                    topics=stmeffect$topics[12],
                    method="continuous",
                    xlab="Week",
                    ylab="Topic Proportions",
                    main="Abortion topic proportion over time",
                    moderator="name",
                    moderator.value="ABC News",
                    ylim=c(0,.15),xlim=c(1,8),
                    linecol="green",
                    printlegend=F, add = T)
plot.estimateEffect(stmeffect,  #Topic proportions in CNN
                    covariate="week",
                    model=fit_stm19,
                    topics=stmeffect$topics[12],
                    method="continuous",
                    xlab="Week",
                    ylab="Topic Proportions",
                    main="Abortion topic proportion over time",
                    moderator="name",
                    moderator.value="CNN",
                    ylim=c(0,.15),xlim=c(1,8),
                    linecol="blue",
                    printlegend=F, add = T)
legend("topright",legend=c("Fox News","ABC News","CNN"),col=c("red","green","blue"),
       lty=1)

#Plot all topics over time by news channels
#Select topics
stmeffect1<-estimateEffect(formula=c(2,3,4,5,6,11,12,15,18)~name*s(week),
                          stmobj=fit_stm19,metadata=topic_dfm_stm$meta) #smooth the effect of week

#Loop function to get dataframe of effects by newssoure and by week
effect <- lapply(c(1:8), function(i) {
  get_effects(stmeffect1,
              variable = "name",
              type = "pointestimate",
              ci = 0.95,
              moderator = "week",
              modval = i,
              cov_val1 = NULL,
              cov_val2 = NULL)
})
effect <- do.call("rbind", effect)

#Create dictionary to name topics
topics <- data.frame(names = c("Elections","Ukraine","School Shootings","Inflation", 
                               "Crime","Health","Jan 6","Gun control","Abortion"),
                     num = c(2,3,4,5,6,11,15,18,12))

#Change topics to those we specified in the dictionary
#install.packages("matchmaker")
library(matchmaker)
effect$topic <- 
  match_vec(
    x = effect$topic,
    dictionary = topics,
    from = "num",
    to = "names",
    quiet = FALSE,
    warn_default = TRUE,
    anchor_regex = TRUE
  )

#All news sources faceted
effect$value = factor(effect$value, levels=c('CNN', 'ABC News', 'Fox News'))
effect %>%
  ggplot(aes(x=moderator, y=proportion, fill=topic)) + 
  geom_bar(stat = "identity") + ylab("Topic proportions") +
  #scale_x_continuous(breaks=seq(1,9,1)) +
  xlab("Week") +
  labs(title = "Proportion of topics accross channels", fill="Topics") +
  facet_grid(~value)

#Change weeks to those we specified in the dictionary
effect$moderator <- as.factor(effect$moderator)
effect$moderator <- 
  match_vec(
    x = effect$moderator,
    dictionary = data.frame(weeks),
    from = "num",
    to = "names",
    quiet = FALSE,
    warn_default = TRUE,
    anchor_regex = TRUE
  )
#Make week a factor variable in the correct order
effect$moderator <- factor(effect$moderator, levels=c("May 2", "May 9", "May 16", "May 23", "May 30", "June 6", "June 13", "June 20", "June 27"))
#Plot
effect %>%
  ggplot(aes(x=moderator, y=proportion, fill=topic)) + 
  geom_bar(stat = "identity") + ylab("Proportion of topics") +
  # scale_x_continuous(breaks=seq(1,9,1)) +
  xlab("Week") +
  labs(title = "Proportions of topics across channels and time", fill="Topics") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~value)


#----------- Frames --------------


fit_stm19b <- stm(documents = out$documents, 
                  vocab = out$vocab,
                  K = 19,
                  max.em.its = 50,
                  prevalence =~ name,
                  content =~ name,
                  data = out$meta,
                  init.type = "Spectral",
                  verbose = TRUE)
labelTopics(fit_stm19b, n=10)

set.seed(831)
plot(fit_stm19b, 
     type="perspectives", 
     topics=12,
     n=30,
     se=TRUE,
     covarlevels = c(2,3),
     main = "Group differences in the content for topic abortion")


#--------Frames with dictionary method--------


library("quanteda")
library("readtext")
library("tidyverse")
library("lubridate")

frames.lexicon <- dictionary(list(politics_frame=c("president*","democrat*","republican*","senat*","election*","white house","vote*","voting","biden","biden's","trump","trump's",
                                                   "harris","harris'","pelosi","pelosi's","speakerpelosi",
                                                   "rep","reps","sen","congress*","filibuster", "ballot*","politics","political","ammendment","campaign","government","gov",
                                                   "gop","gop-","gop's","democra*", "dem", "dems","obama*","governor*"),
                                  economy_frame=c("cost","poor","financ*","money","dollar*","-dollar","profit*","rich*","tax","taxes","taxed","econom*","net loss","business*","revenue*",
                                                  "industr*","corporat*","capitali*","company","companies","employe*","price","prices","inflation","expens*","socioeconomic"),
                                  medical_frame=c("medic*","health","healthcare","hospital*","clinic*","pill","pills","mortalit*","contracept*","pregnan*","fetus*","birth control","embryo*",
                                                  "womb*","miscarriage*","family planning","vasectom*", "trimester*", "planned parenthood", "plan b ",
                                                  "in utero","intact dilation","mifepristone", "misoprostol","vacuum aspiration","forced birth",
                                                  "gameprost","partial birth", "partial-birth","doctor*","nurse*","drug*","pharma*","abortion care","reproductive care"),
                                  moral_frame=c("religio*","church*","catholic*","protestant*","christian*","pope*","spiritual*","preach*","holy","pray*","holier","communion","moral","morality","immoral*","ethic*","unethic*","feticide*",
                                                "infanticide*","god","godly","godless","faith*","bishop","archbishop","homicide*","conscionable","unconscionable","megachurch","bible","conscience",
                                                "biblical","divinity","doctrine","heaven","hell","damn*","righteous","sacred","sin","sinful","sinister","satan*","virtue","virtuous","theocra*"),
                                  public_frame=c("poll","polls","public","protest","protests","protester*","protesting","supporter*","activist*","survivor*","population*","demonstrator*","voter*","Americans")))
frames.lexicon
dfm.frames <- dfm(corp_quanteda, dictionary = frames.lexicon)
dfm.frames <- dfm_weight(dfm.frames, scheme = "boolean")
frames_dic <- convert(dfm.frames, "data.frame")
names(frames_dic)[1]="id"
frames <- merge(frames_dic,news_data,by="id",all=T)

frames_a <- frames%>%
  filter(abortion_post==1)

table(frames_a$politics_frame)
table(frames_a$economy_frame)
table(frames_a$moral_frame)
table(frames_a$medical_frame)
table(frames_a$public_frame)

#write.csv2(frames, "frames.sav")

#Plot frequency of frames per channel
df <- frames %>%
  filter(abortion_post==1) %>% 
  select(c("Date","name","text","politics_frame","economy_frame","medical_frame",
           "moral_frame","public_frame"))

table(df$politics_frame)
politics_ratio <- 308/(888)
table(df$economy_frame)
economy_ratio <- 54/(888)
table(df$medical_frame)
medical_ratio <- 179/(888)
table(df$moral_frame)
moral_ratio <- 79/(888)
table(df$public_frame)
public_ratio <- 164/(888)

# plot

df<-frames_a

#political frame
df_p <- df %>%
  filter(politics_frame==1) %>%
  select(Date, name)
p<-ggplot(df_p, aes(x=Date, fill=name)) +
  geom_bar(stat="count",position="stack") +
  #ylim (0, 40) +
  labs(x = " ", y = " ",
       #title = "Frequency of political frame per channel",
       subtitle = "Political Frame",
       fill = "Channels") + theme(legend.position = "none")
p

#economy frame
df_e <- df %>%
  filter(economy_frame==1) %>%
  select(Date, name)
e<-ggplot(df_e, aes(x=Date, fill=name)) +
  geom_bar(stat="count",position="stack") +
  ylim (0, 40) +
  labs(x = " ", y = " ",
       #title = "Frequency of economy frame per channel",
       subtitle = "Economical Frame",
       fill = "Channels") + theme(legend.position = "none")
e

#health frame
df_h <- df %>%
  filter(medical_frame==1) %>%
  select(Date, name)
h<-ggplot(df_h, aes(x=Date, fill=name)) +
  geom_bar(stat="count",position="stack") +
  ylim (0, 40) +
  labs(x = " ", y = " ",
       #title = "Frequency of medical frame per channel",
       subtitle = "Medical Frame",
       fill = "Channels") + theme(legend.position = "none")
h

#moral frame
df_m <- df %>%
  filter(moral_frame==1) %>%
  select(Date, name)
m<-ggplot(df_m, aes(x=Date, fill=name)) +
  geom_bar(stat="count",position="stack") +
  ylim (0, 40) +
  labs(x = " ", y = " ",
       #title = "Frequency of moral frame per channel",
       subtitle = "Moral Frame",
       fill = "Channels") + theme(legend.position = "none")
m

#public frame
df_pub <- df %>%
  filter(public_frame==1) %>%
  select(Date, name)
pub<-ggplot(df_pub, aes(x=Date, fill=name)) +
  geom_bar(stat="count",position="stack") +
  ylim (0, 40) +
  labs(x = " ", 
       y = "Number of tweets",
       #    title = "Frequency of public frame per channel",
       subtitle = "Public Frame",
       fill = "Channels") + theme(legend.position = "none")  
pub 

library(ggpubr)
plot<-ggarrange(p,h,pub,m,e,ncol=2, nrow=3, common.legend = TRUE, legend = "bottom")
annotate_figure(plot, top = text_grob("Distribution of frames across time"))


#---------- Sentiment Analysis ----------


#install.packages("textdata")
library(textdata)

sent <- roe2
sent <- sent %>%
    #subset(abortion_post=="1") %>%
    #select(id,text) %>%
    unnest_tokens(word, text) %>% # tokenize data
    inner_join(get_sentiments("bing")) %>% # Find words also in bing, classified by sentiment
    count(id, sentiment) %>% 
    tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% #Move to one row per episode
    mutate(sent_diff_bing = positive - negative) # Take the difference of positive and negative words

sent_data <- merge(roe2,sent)

#Plot all on one graph
bing_plot <- ggplot(sent_data, aes(x=Date, y=sent_diff_bing, colour = name, group = name)) +
  geom_line(stat="smooth", size = .8) +
  geom_point(stat="summary", fun="mean", size = 1)+
  geom_ribbon(stat='smooth', se=TRUE, alpha=0.07, 
              aes(color = NULL, group = factor(name))) +
  #ylim (-3,3) +
    labs(x = "Date", y = "Sentiment",
       title = "Sentiment of all tweets",
       subtitle = "Average sentiment per day",
       col = "Channels")
bing_plot

# Sentiment of abortion related tweets only
sent2 <- roe2 %>%
  subset(abortion_post=="1") %>%
  #select(id,text) %>%
  unnest_tokens(word, text) %>% # tokenize data
  inner_join(get_sentiments("bing")) %>% # Find words also in bing, classified by sentiment
  count(id, sentiment) %>% 
  tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% #Move to one row per episode
  mutate(sent_diff_bing = positive - negative) # Take the difference of positive and negative words

sent_data1 <- merge(roe2,sent2)

#Plot all on one graph
bing_plot1 <- ggplot(sent_data1, aes(x=Date, y=sent_diff_bing, colour = name, group = name)) +
  geom_line(stat="smooth", size = .8) +
  geom_point(stat="summary", fun="mean", size = 1)+
  geom_ribbon(stat='smooth', se=TRUE, alpha=0.07, 
              aes(color = NULL, group = factor(name))) +
  #ylim (-3,3) +
  labs(x = "Date", y = "Sentiment",
       title = "Sentiment of abortion related tweets only",
       subtitle = "Average sentiment per day",
       col = "Channels")
bing_plot1


#------- Save workspace -------


# Save all objects in work space
#save.image(file = "roe_workspace_final.RData")

#Load work space
#load("roe_workspace_final.RData")
