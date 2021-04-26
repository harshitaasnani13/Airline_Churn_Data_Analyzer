# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

#Download and install tidyverse
#install.packages("tidyverse")
library(tidyverse)

#Downlod and install jsonlite library
#install.packages("jsonlite")
library(jsonlite)

#Downlod and install ggplot2 library
#install.packages("ggplot2")
library(ggplot2)

#Download and Install ggmap
#install.packages("maps")
library(maps)

#Download and install arules package
#install.packages("arules")
library(arules)

#download and install arulesViz package
#install.packages("arulesViz")
library(arulesViz)

#Load data from json into datafram
df<-jsonlite::fromJSON("//hd.ad.syr.edu/02/ee5b06/Documents/Desktop/Final.json")

#Explore structure of imported data
#str(df)
#summary(df)
View(df)
check_na<-function(df){
  #Excute for loop to identify where NAs are present in data set
  for (Var in names(df)) {
    missing <- sum(is.na(df[,Var]))
    if (missing > 0) {
      print(c(Var,missing))
    }
  }
}

QuartileGrouping<-function(columnName,newColumnName){
  newColumnName[columnName <= quantile(columnName, probs=0.25, na.rm=TRUE)]<-paste("<=",quantile(columnName, probs=0.25, na.rm=TRUE))
  newColumnName[columnName > quantile(columnName, probs=0.25, na.rm=TRUE) & columnName <= quantile(columnName, probs=0.5, na.rm=TRUE)]<-paste(quantile(columnName, probs=0.25, na.rm=TRUE),"-",quantile(columnName, probs=0.5, na.rm=TRUE))
  newColumnName[columnName> quantile(columnName, probs=0.5, na.rm=TRUE) & columnName <=quantile(columnName, probs=0.75, na.rm=TRUE)]<-paste(quantile(columnName, probs=0.5, na.rm=TRUE),"-",quantile(columnName, probs=0.75, na.rm=TRUE))
  newColumnName[columnName >quantile(columnName, probs=0.75, na.rm=TRUE)]<-paste(">",quantile(columnName, probs=0.75, na.rm=TRUE))
  newColumnName<-as.factor(newColumnName)
  return(newColumnName)
}

clean_data<-function(df){
  #Convert approriate variables to factors
  df$Destination.State<-as.factor(df$Destination.State)
  df$Origin.State<-as.factor(df$Origin.State)
  df$Airline.Status<-as.factor(df$Airline.Status)
  df$Type.of.Travel<-as.factor(df$Type.of.Travel)
  df$Class<-as.factor(df$Class)
  df$Flight.cancelled<-as.factor(df$Flight.cancelled)
  df$Partner.Code<-as.factor(df$Partner.Code)
  df$Gender<-as.factor(df$Gender)
  df$Price.Sensitivity<-as.factor(df$Price.Sensitivity)
  df$Total.Freq.Flyer.Accts<-as.factor(df$Total.Freq.Flyer.Accts)
  #df$Likelihood.to.recommend<-as.factor(df$Likelihood.to.recommend)
  
  #Remove State Abbreviations from destination and origin cit fields. Data is already caputured in another field
  df$Origin.City<-gsub(", ..","",df$Origin.City)
  df$Destination.City<-gsub(", ..","",df$Destination.City)
  
  #If flight not cancelled and no arrival delay, replace with 0 -> assume it arrived on-time
  df$Arrival.Delay.in.Minutes[is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="No"]<-0
  
  #create new dataframe where flight time was missing but flight was not cancelled
  missingflighttime <- df%>%
    filter(is.na(df$Flight.time.in.minutes)&df$Flight.cancelled=="No")
  
  #Calculate average flight time from completed segments between city pairs to fill in applicable missing flight times
  #Iterate through origin cities with missing flight times
  for (var1 in unique(missingflighttime$Origin.City)){
    #Iterate through destination cities with missing flight times
    for (var2 in unique(missingflighttime$Destination.City)){
      #Calculate mean of know flight times between city pairs (origin to destination OR destination to origin) to replace NAs
      df$Flight.time.in.minutes[df$Origin.City==var1&df$Destination.City==var2&df$Flight.cancelled=="No"]<-mean(df$Flight.time.in.minutes[(df$Origin.City==var1&df$Destination.City==var2)|(df$Origin.City==var2&df$Destination.City==var1)], na.rm=TRUE)
    }
  }
  #Replace delay times in flights that were cancelled with NAs to make removing for calculation easier
  df$Departure.Delay.in.Minutes[df$Flight.cancelled=="Yes"]<-NA
  
  #Convert Flight.Date to date format field instead of chr
  df$Flight.date<-as.Date(df$Flight.date,"%m/%d/%y")
  
  #Add day of the week factor calculated from flight date
  df$Day.of.Week<-as.factor(weekdays(df$Flight.date))
  
  #Add month of year factor calculated from flight date
  df$Month.of.Year<-as.factor(months(df$Flight.date))
  
  #Create groups of arrival delay in 15 min increments and store as factor in new column.
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes <= 15]<-"0-15"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >15 & df$Arrival.Delay.in.Minutes <=30 ]<-"16-30"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >30 & df$Arrival.Delay.in.Minutes <=45 ]<-"31-45"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >45 & df$Arrival.Delay.in.Minutes <=60 ]<-"46-60"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >60]<-">60"
  df$Arrival.Delay.Group<-as.factor(df$Arrival.Delay.Group)
  
  #Create groups of departure delay in 15 min increments and store as factor in new column.
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes <= 15]<-"0-15"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes>15 & df$Departure.Delay.in.Minutes <=30 ]<-"16-30"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes>30 & df$Departure.Delay.in.Minutes <=45 ]<-"31-45"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes>45 & df$Departure.Delay.in.Minutes <=60 ]<-"46-60"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes >60]<-">60"
  df$Departure.Delay.Group<-as.factor(df$Departure.Delay.Group)
  
  #df$Departure.Delay.Group<-QuartileGrouping(df$Departure.Delay.in.Minutes,df$Departure.Delay.Group)
  
  #df$Arrival.Delay.Group<-QuartileGrouping(df$Arrival.Delay.in.Minutes,df$Arrival.Delay.Group)
  
  #Create groups of flights times based on quartiles of data
  df$Flight.Time.Group<-QuartileGrouping(df$Flight.time.in.minutes,df$Flight.Time.Group)
  
  #Create age groupings based on quartiles of data
  df$ageGroup<-QuartileGrouping(df$Age,df$ageGroup)
  
  #Create year of first flight groupings based on quartiles of data
  df$yearOfFirstFlightGroup<-QuartileGrouping(df$Year.of.First.Flight,df$yearOfFirstFlightGroup)
  
  #Create eating spending groupings based on quartiles of data
  df$eatingGroup<-QuartileGrouping(df$Eating.and.Drinking.at.Airport,df$eatingGroup)
  
  #Create shopping amount groupings based on quartiles of data
  df$shoppingGroup<-QuartileGrouping(df$Shopping.Amount.at.Airport,df$shoppingGroup)
  
  #Create loyalty groupings based on quartiles of data
  df$loyaltyGroup<-QuartileGrouping(df$Loyalty,df$loyaltyGroup)
  
  #Create flights per year groupings based on quartiles of data
  df$flightsperyearGroup<-QuartileGrouping(df$Flights.Per.Year,df$flightsperyearGroup)
  
  #Create depature hour groupings based on quartiles of data
  df$departurehourGroup<-QuartileGrouping(df$Scheduled.Departure.Hour,df$departurehourGroup)
  
  
  #Create Groups of Promoters, Passives and Detractors
  df$Promoter.Score[df$Likelihood.to.recommend==9|df$Likelihood.to.recommend==10]<-"Promoter"
  df$Promoter.Score[df$Likelihood.to.recommend==7 | df$Likelihood.to.recommend==8]<-"Passive"
  df$Promoter.Score[df$Likelihood.to.recommend < 7]<-"Detractor"
  df$Promoter.Score<-as.factor(df$Promoter.Score)
  
  
  
  return(df)
}

#check_na(df)

df<-clean_data(df)

#Convert Dest and Orgin Cities to Factors (for some reason does not work in fucntion - it returns chr variables)
df$Destination.City<-as.factor(df$Destination.City)
df$Origin.City<-as.factor(df$Origin.City)

str(df)
check_na(df)

#Confirm if all missing departure delay minutes are associated with a cancelled flight
#length(df$Departure.Delay.in.Minutes[is.na(df$Departure.Delay.in.Minutes) & df$Flight.cancelled=="Yes"])
#Confirm if all missing arrival delay minutes are associated with a cancelled flight
#length(df$Arrival.Delay.in.Minutes[is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="Yes"])


agehisto<-ggplot(df)+
  aes(x=Age)+
  geom_histogram(bins=5,color="black", fill="white")+
  ggtitle("Distribution of ages")
agehisto

partnerhisto<-ggplot(df)+
  aes(x=Partner.Code)+
  geom_histogram(stat = "count", color="black", fill="white")+
  ggtitle("Distribution of flights per partner")
partnerhisto

typehisto<-ggplot(df)+
  aes(x=Type.of.Travel)+
  geom_histogram(stat = "count", color="black", fill="white")+
  ggtitle("Distribution of Travel Types")
typehisto

arrdelayhisto<-ggplot(df)+
  aes(x=Arrival.Delay.Group)+
  geom_histogram(stat = "count", color="black", fill="white")+
  ggtitle("Distribution of arrival delay in minutes")
arrdelayhisto
str(df)

flightdurationhisto<-ggplot(df)+
  aes(x=Flight.time.in.minutes)+
  geom_histogram(bins =10,color="black", fill="white")+
  ggtitle("Distribution flight time in minutes")
flightdurationhisto

summary(df$Eating.and.Drinking.at.Airport)
eathisto<-ggplot(df)+
  aes(x=Eating.and.Drinking.at.Airport)+
  geom_histogram(bins=5,color="black", fill="white")+
  ggtitle("Distribution of money spent on eating and drinking")
eathisto

NPShisto<-ggplot(df)+
  aes(x=Promoter.Score)+
  geom_histogram(stat="count",color="black", fill="white")+
  ggtitle("Promoter/Passive/Detractor Numbers")
NPShisto

us<-map_data("state") #Save state map information as dataframe from maps package

#Create vector for rules mining with only factor variables
Rulesdf<-df%>%
  filter(Partner.Code=="EV"|Partner.Code=="DL"|Partner.Code=="WN"|Partner.Code=="OO")%>%
  select(colnames(df[,sapply(df,is.factor)]))

RulesdfX<-as(Rulesdf, "transactions")

rulesetPromoter<-apriori(RulesdfX,
                         # Specify parameters for rule creation.  Set minimum support threshold at 0.5% and the confidence threshold at 50%
                         parameter = list(support=0.075, confidence=0.5), 
                         # Restrict rule creation to associations that correspond to passengers surviving.
                         appearance = list(default="lhs",rhs=("Promoter.Score=Promoter")))
inspectDT(rulesetPromoter)

rulesetDetractor<-apriori(RulesdfX,
                          # Specify parameters for rule creation.  Set minimum support threshold at 0.5% and the confidence threshold at 50%
                          parameter = list(support=0.09, confidence=0.7), 
                          # Restrict rule creation to associations that correspond to passengers surviving.
                          appearance = list(default="lhs",rhs=("Promoter.Score=Detractor")))
inspectDT(rulesetDetractor)


# text mining part
#1. analyze the word that customer use in the comment and form a word cloud <as a whole>
#(1) remove NA to get pure text that we want to analyze
comment <- df$freeText[!is.na(df$freeText)]
View(comment)
str(comment)
#(2) remove those useless parts(Capital, number, punctuatiion, meaningless words)
install.packages("tm")
library(tm)
word.vec <- VectorSource(comment)
word.corpus <- Corpus(word.vec)
word.corpus <- tm_map(word.corpus,content_transformer(tolower))   # change uppercase letter to lowercase
word.corpus <- tm_map(word.corpus,removePunctuation)   # make each character a combine of words, no more sentences
word.corpus <- tm_map(word.corpus,removeNumbers)       # since we only analyze words, not numbers. and we do not have reference data used to compare number
word.corpus <- tm_map(word.corpus,removeWords,stopwords("english")) # remove words that appear a lot but meaningless
term_Document_Matrix <- TermDocumentMatrix(word.corpus)
term_Document_Matrix
inspect(term_Document_Matrix)
#(3) word cloud
install.packages("wordcloud")
library(wordcloud)
tdm <- as.matrix(term_Document_Matrix)
View(tdm)   # term in rows, documents in column
counts <- rowSums(tdm) %>% sort(decreasing=TRUE) # count the number of appearance of each term, and sort from the most popular to the least popular
cloudframe <- data.frame(word=names(counts),freqency=counts)  # create a dataframe to store the word and their number of appearance
View(cloudframe)   # we can see which word appear the most
head(cloudframe,30)   # just look at the top 30
pal = brewer.pal(5,"Reds")     # used inside "colors" parameter to make the darkest color to the most frequently shown word, the lightest color to the least frequently shown one.
# resource:https://www.rdocumentation.org/packages/RColorBrewer/versions/1.1-2/topics/RColorBrewer
# resource:https://www.r-bloggers.com/word-cloud-in-r/
wordcloud(cloudframe$word,cloudframe$freqency,min.freq=30,colors=pal)




#2. Sensitive analysis

#(1) read txt files of positive word and negative word
positive_txt <- scan("//hd.ad.syr.edu/02/ee5b06/Documents/Desktop/positive-words.txt",character(0),sep="\n")
negative_txt <- scan("//hd.ad.syr.edu/02/ee5b06/Documents/Desktop/negative-words.txt",character(0),sep="\n")
# remove headers
positive_txt <- positive_txt[-1:-34]
negative_txt <- negative_txt[-1:-34]

#(2) find positive words and show positive word cloud
totalwords <- sum(counts)
totalwords
words <- names(counts)

positive_matched <- match(words,positive_txt,nomatch=0)
positive_count <- counts[which(positive_matched!=0)]
positive_word <- names(positive_count)
View(positive_count)  # show the positive words and their appearance frequency
positive_number <- sum(positive_count)
positive_number
positive_cloud <- data.frame(positive_word,positive_count)  # create a dataframe for word cloud
View(positive_cloud)
wordcloud(positive_cloud$positive_word,positive_cloud$positive_count,min.freq=5,colors=pal)

#(3) find negative words and show negative word cloud
negative_matched <- match(words,negative_txt,nomatch=0)
negative_count <- counts[which(negative_matched!=0)]
negative_word <- names(negative_count)
View(negative_count)  # show the negative words and their appearance frequency
negative_number <- sum(negative_count)
negative_number
negative_cloud <- data.frame(negative_word,negative_count)  # create a dataframe for word cloud
View(negative_cloud)
wordcloud(negative_cloud$negative_word,negative_cloud$negative_count,min.freq=5,colors=pal)

#(4) count the positive/negative ratio
positive_rate <- positive_number/length(words)
positive_rate
negative_rate <- negative_number/length(words)
negative_rate
