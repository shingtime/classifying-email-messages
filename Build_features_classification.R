############################ APPENDIX: R CODE #########################################


############# generate all the variables and save in one data frame.##############
load("~/Desktop/TrainingMessages.rda")

##1.isRe
isRe = function(email) {
  if("Subject" %in% names(email$header)){
    if(length(grep("^[ \t]*Re:", email$header[["Subject"]],useBytes = TRUE)) > 0) return (TRUE)
    
  }
  else FALSE   
}

isRe = lapply(1:length(trainMessages), function(i){
  isRe(trainMessages[[i]])
})
isRe = unlist(isRe)

##2.numLinesInBody
num_linesInBody = function(email){
  if(length(email$body) == 0){
    return(0)
  }
  if(length(email$body) > 0) length(email$body)
}
##Generate numlinesInBody
numlinesInBody = lapply(1:length(trainMessages), function(i){
  num_linesInBody(trainMessages[[i]])
})
numlinesInBody = unlist(numlinesInBody)

##3.numAttachments
num_Attachments = function(email){
  length(email$attachment)
}
##Generate numAttachments
numAttachments = lapply(1:length(trainMessages), function(i){
  num_Attachments(trainMessages[[i]])
})
numAttachments = unlist(numAttachments)

##4.subjectExclamationCount
subject_ExclamationCount = function(email){
  subject = email$header["Subject"]
  if(length(subject) == 0) return (0)
  #subject = paste(subject,collapse="\n")
  if(grepl("!",subject,useBytes = TRUE)){
    sapply(gregexpr("!", subject,useBytes = TRUE), length)    
  }
  else return (0)
}

##Generate subjectExclamationCount
subjectExclamationCount = lapply(1:length(trainMessages), function(i){
  subject_ExclamationCount(trainMessages[[i]])
})
subjectExclamationCount = unlist(subjectExclamationCount)

##5.numRecipients

num_recipient = function(email){
  length1 = numeric()
  length2 = numeric()
  To = email$header["To"]
  cc = email$header["Cc"]  
  if(length(To) > 0){
    if(grepl(",",To,useBytes = TRUE)==TRUE){
      length1 = nchar(gsub("[^,]","",To,useBytes = TRUE),type = "bytes") + 1
      
    }
    else length1 = 1
  }
  else length1 = 0
  
  if(length(cc) > 0){
    if(grepl(",",To,useBytes = TRUE)==TRUE){
      length2 = nchar(gsub("[^,]","",To,useBytes = TRUE),type = "bytes") + 1
      
    }
    else length2 = 1
  }
  else length2 = 0
  
  length1 + length2
  
}
##generate numRecipients 
numRecipients = lapply(1:length(trainMessages), function(i){
  num_recipient (trainMessages[[i]])
})
numRecipients  = unlist(numRecipients)

##6.isInReplyTo

is_InReplyTo = function(email){
  if(length(grep("In-Reply-To",names(email$header),perl=TRUE)) > 0){
    return(TRUE)
  }
  if(length(grep("In-Reply-To",names(email$header),perl=TRUE)) == 0){
    return(FALSE)
  }
}
##Generate isInReplyTo
isInReplyTo = lapply(1:length(trainMessages), function(i){
  is_InReplyTo (trainMessages[[i]])
})
isInReplyTo  = unlist(isInReplyTo)

##7.bodyCharacterCount

body_CharacterCount = function(email){
  if(length(email$body) == 0) return (0)
  sum(nchar(email$body,allowNA=TRUE))
}
##generate bodyCharacterCount
bodyCharacterCount = lapply(1:length(trainMessages), function(i){
  body_CharacterCount (trainMessages[[i]])
})
bodyCharacterCount = unlist(bodyCharacterCount)

##8.subjectQuestCount

subjectquestcount = function(email){
  subject = email$header["Subject"]
  if(length(subject) == 0) return (0)
  #subject = paste(subject,collapse="\n")
  if(grepl("\\?",subject,useBytes = TRUE)){
    sapply(gregexpr("\\?", subject,useBytes = TRUE), length)    
  }
  else return (0)
}
##generate subjectQuestCount
subjectQuestCount = lapply(1:length(trainMessages), function(i){
  subjectquestcount (trainMessages[[i]])
})
subjectQuestCount = unlist(subjectQuestCount)

##9.multipartText

multi_part = function(email){
  if(length(email$header["Content-Type"]) == 0) return (FALSE)
  grepl("^text/plain",email$header["Content-Type"],perl=TRUE)   
}
##generate multipartText
multipartText = lapply(1:length(trainMessages), function(i){
  multi_part (trainMessages[[i]])
})
multipartText = unlist(multipartText) 

##10.subjectSpamWords

subject_spam = function(email){
  if(length(email$header["Subject"])==0) return (FALSE)
  grepl("( viagra | pounds | free | weight | guarantee | millions | dollars | credit | risk | precription | generic | drug | (money back) | (credit card) )", email$header["Subject"],perl=TRUE)  
}
##generate subjectSpamWords
subjectSpamWords = lapply(1:length(trainMessages), function(i){
  subject_spam (trainMessages[[i]])
})
subjectSpamWords = unlist(subjectSpamWords)

##11.percentCapitals

percent_capital = function(email){
  
  BODY = email$body
  BODY = gsub("[^[:alpha:]]", "", BODY)
  BODY = BODY[BODY != ""]
  CAPITAL = gsub("[^A-Z]", "", BODY)
  CAPITAL = CAPITAL[CAPITAL !=""]
  sum(nchar(CAPITAL,type = "bytes"))/sum(nchar(BODY,type = "bytes"))
  
}
##generate percentCapitals
percentCapitals = lapply(1:length(trainMessages), function(i){
  percent_capital (trainMessages[[i]])
})
percentCapitals = unlist(percentCapitals) 

##12.percentSubjectBlanks

percent_subjectblanks = function(email){
  subject = email$header["Subject"]
  blank = gsub("[^[:space:]]","",subject,perl=TRUE)
  sum(nchar(blank,allowNA=TRUE))/sum(nchar(subject,allowNA = TRUE))
}
##generate percentSubjectBlanks
percentSubjectBlanks = lapply(1:length(trainMessages), function(i){
  percent_subjectblanks (trainMessages[[i]])
})
percentSubjectBlanks = unlist(percentSubjectBlanks) 

##13.averageWordLength

average_word = function(email){
  if(length(email$body)==0) return (0)
  body = email$body
  word = strsplit(body," ",useBytes = TRUE)
  word_m = lapply(word,length)
  cumsum(word_m)[length(cumsum(word_m))]
}
##generate averageWordLength
averageWordLength = lapply(1:length(trainMessages), function(i){
  average_word (trainMessages[[i]])
})
averageWordLength = unlist(averageWordLength) 

##14.messageIdHasNoHostname

messageId = function(email){ 
  host = email$header["Message-Id"]
  if(length(host) == 0) return (FALSE)
  grepl("@", host,useBytes = TRUE)
}

messageIdHasNoHostname = lapply(1:length(trainMessages), function(i){
  messageId (trainMessages[[i]])
})
messageIdHasNoHostname = unlist(messageIdHasNoHostname) 

##15.fromnumericEnd

from_number = function(email){
  from = email$header["From"]
  if(length(from) == 0) return (FALSE)
  
  grepl(".[0-9]@",from,perl=TRUE)
  
}

fromnumericEnd =  lapply(1:length(trainMessages), function(i){
  from_number (trainMessages[[i]])
})
fromnumericEnd = unlist(fromnumericEnd)

##16.hourSent

hour_sent = function(email){
  date = email$header["Date"]
  if(length(date) == 0) return (NA)
  date = strsplit(date," ")
  date_hour = date[[1]]
  
  date_hour = date_hour[grep(":",date_hour)]
  date_hour = gsub(":","",date_hour,perl=TRUE)
  date_hour = as.numeric(substring(date_hour,1,2))
  date_hour
}

hourSent = lapply(1:length(trainMessages), function(i){
  hour_sent (trainMessages[[i]])
})
hourSent = unlist(hourSent) 

##17.isYelling

iYelling = function(email) {
  subject = email$header["Subject"] 
  if ( length(subject) > 0)  {
    letters = gsub("[^[:alpha:]]", "", subject,perl=TRUE)
    if (nchar(letters) > 0) {
      nchar(gsub("[A-Z]", "", letters)) == 0
    }
    else FALSE
  }
  else NA
}

isYelling = lapply(1:length(trainMessages), function(i){
  iYelling (trainMessages[[i]])
})

isYelling = unlist(isYelling)

##18.isDear

is_dear = function(email){
  body = email$body
  if(length(body) == 0) return (FALSE)
  body = paste(body,collapse="\n") 
  grepl("Dear",body,useBytes = TRUE)
}

isDear = lapply(1:length(trainMessages), function(i){
  is_dear (trainMessages[[i]])
})

isDear = unlist(isDear)

##19.numDollarSigns

num_dollar = function(email){
  body = email$body
  if(length(body) == 0) return (FALSE)
  body = paste(body,collapse="\n")
  if(grepl("\\$",body,useBytes = TRUE)){
    sapply(gregexpr("\\$", body,useBytes = TRUE), length)    
  }
  else return (0)
  
}


numDollarSigns = lapply(1:length(trainMessages), function(i){
  num_dollar (trainMessages[[i]])
})
numDollarSigns = unlist(numDollarSigns)

##20.is_Spam
is_Spam  = function(dataset){
  grepl("spam",names(dataset))
}

isSpam = is_Spam(trainMessages)
isSpam = unlist(isSpam) 

#### combine all the variables into a data frame.
Spam_Ham = cbind(isSpam,numlinesInBody,numAttachments,numRecipients,isInReplyTo,
                 bodyCharacterCount,multipartText,subjectSpamWords,percentSubjectBlanks,
                 averageWordLength,messageIdHasNoHostname,fromnumericEnd,hourSent,
                 isYelling,isDear,numDollarSigns,subjectExclamationCount,
                 subjectQuestCount,percentCapitals,isRe)

Spam_Ham = as.data.frame(Spam_Ham)
## save the data.frame into a .rda file.
save(Spam_Ham, file = "Spam_Ham.rda")

############# Explore the variables to seperate HAM and SPAM #######################

##1.isSpam
library(lattice)
summary(isSpam[isSpam])
summary(isSpam[!isSpam])

##2.isRe

par(mfrow = c(1,1))
mosaicplot(table(isSpam, isRe),shade = TRUE,
           main="Distribution of isRe with isSpam",col=rainbow(2))

##It seems that the SPAM emails are less likely to contain an "Re" than the HAM emails.
##Besides, if the email is SPAM, it is more likely to have a subject without "Re".

##3.numlinesInBody

summary(numlinesInBody[isSpam])
summary(numlinesInBody[!isSpam])
par(mfrow=c(1,2))
smoothScatter(numlinesInBody[isSpam],ylim=c(0,100),xlab="count",ylab="SPAM",
              main="numlinesInBody")
abline(h = mean(numlinesInBody[isSpam]))

smoothScatter(numlinesInBody[!isSpam],ylim=c(0,100),xlab="count",ylab="HAM",
              main="numlinesInBody")
abline(h = mean(numlinesInBody[!isSpam]))

#For this variable, I can find that the average number of lines in SPAM is 74.37, 
#which is much bigger than the average number of lines in HAM. 
#It seems that there are more lines in the SPAM than HAM. 
#So this variable is relevant to clarify the SPAM and HAM.

##4.numAttachments

summary(numAttachments[isSpam])
table(numAttachments[isSpam])
summary(numAttachments[!isSpam])
table(numAttachments[!isSpam])
par(mfrow = c(1,2))
smoothScatter(numAttachments[isSpam],xlab="count",ylab="SPAM",main="numAttachments")
abline(h = mean(numAttachments[isSpam]),lwd=3,col=rainbow(1))
smoothScatter(numAttachments[!isSpam],xlab="count",ylab="HAM",main="numAttachments")
abline(h = mean(numAttachments[!isSpam]),col="yellow",lwd=3)
par(mfrow = c(1,1))
xyplot(numAttachments~numlinesInBody | as.factor(isSpam),
       type = c('p',"g"),data = Spam_Ham)

#It seems that SPAM emails contain more attachments than HAM emails in average
#(from summary and smoothscatter plot).From the scatter plot of combining numAttachment and numlinesInBody, 
#it seems that there are more attachment for isSpam==1(SPAM) rather than isSpam==0(HAM). 
#They have a pretty similiar distribution for number of lines in body in both groups

##5&6.  subjectExclamationCount & subjectQuestCount

summary(subjectExclamationCount[isSpam])
table(subjectExclamationCount[isSpam])
summary(subjectExclamationCount[!isSpam])
table(subjectExclamationCount[!isSpam])
summary(subjectQuestCount[isSpam])
table(subjectQuestCount[isSpam])
summary(subjectQuestCount[!isSpam])
table(subjectQuestCount[!isSpam])

xyplot(subjectExclamationCount~subjectQuestCount | as.factor(isSpam),
       type = c('p',"g"),data = Spam_Ham)

#It seems that SPAM emails contain far more exclamation marks than the HAM emails in average. 
#But for question marks, SPAM emails are slightly more than HAM emails in average.
#I can also find the same pattern in the xyplot. 
#So maybe subjectExclamationCount is the more useful variable than subjectQuestCount 
#variable for classifying SPAM and HAM. 
#Because there are no significant differences in counting the numbers of question marks. 
#This subjectQuestCount may not be a good variable to predict if an email is SPAM or HAM.

#We can also find the same patterns from frequency tables. 
#For subjectExclamationCount,a relative high percentage of SPAM emails have one exclamation, 
#while the percentage is lower for HAM email. But for subjectQuestCount, this pattern doesn't hold.

##7.numRecipients

summary(numRecipients[isSpam])
table(numRecipients[isSpam])
summary(numRecipients[!isSpam])
table(numRecipients[!isSpam])
par(mfrow = c(1,2))
smoothScatter(numRecipients[isSpam],xlab="count",ylab="SPAM",main="numRecipients")
abline(h = mean(numRecipients[isSpam]),lwd=3,col=rainbow(1))
smoothScatter(numRecipients[!isSpam],xlab="count",ylab="HAM",main="numRecipients")
abline(h = mean(numRecipients[!isSpam]),col="yellow",lwd=3)

#It seems that SPAM emails contain more number of recipients than HAM emails. 
#From the frequency table, I can find that most of HAM emails only have recipients less or equal to 4.
#But for SPAM emails, there are more recipients. 
#From the scatter plot, I find that SPAM emails have more extreme value than HAM emails, 
#which strengthens the conclusion.

##8.isInReplyTo
par(mfrow = c(1,1))
mosaicplot(table(isSpam, isInReplyTo),shade = TRUE,
           main="Distribution of isInReplyTo with isSpam",col=rainbow(2))
mosaicplot(table(isInReplyTo, isRe),shade = TRUE,
           main="Distribution of isRe with isInReplyTo",col=rainbow(2))

#From the first mosaic plot , I can find that most of SPAM emails don't have In-Reply-To field. 
#Only a very small part of SPAM emails in the training dataset have In-Reply-to field in the header.

#I can also compare isRe variable and isInReplyTo variable. 
#In the second mosaic plot,I find that most of emails both have "Re" in the subject and In-Reply-To in the header or neither. 
#If an email has both "Re" and In-Reply-To, this email has higher probability to belong to HAM emails.

##9.bodyCharacterCount

summary(bodyCharacterCount[isSpam])
summary(bodyCharacterCount[!isSpam])
par(mfrow = c(1,2))
smoothScatter(bodyCharacterCount[isSpam],ylim=c(0,10000),xlab="count",
              ylab="SPAM",main="bodyCharacterCount")
smoothScatter(bodyCharacterCount[!isSpam],ylim=c(0,10000),xlab="count",
              ylab="HAM",main="bodyCharacterCount")

#Although the average numbers of body part's character in SPAM emails are bigger than the average numbers in HAM emails.
#But the difference here is not significant. If givening more data, this difference may be more significant.
#Generally , I don't think this variable is very useful for prediction.

##10.multipartText

par(mfrow = c(1,1))
mosaicplot(table(isSpam, multipartText),shade = TRUE,
           main="Distribution of multipartText with isSpam",col=rainbow(2))

#In the mosaic plot, it shows a pattern that if the email is not SPAM, the message is a multipart. 
#If an email is SPAM, it is less likely that the message is a multipart. It may only have text.

##11.subjectSpamWords

par(mfrow = c(1,1))
mosaicplot(table(isSpam, subjectSpamWords),shade = TRUE,
           main="Distribution of subjectSpamWords with isSpam",col=rainbow(2))
length(subjectSpamWords[isSpam])
length(subjectSpamWords[!isSpam])

##12.percentCapitals

summary(percentCapitals[isSpam])
summary(percentCapitals[!isSpam])
par(mfrow = c(1,2))
smoothScatter(percentCapitals[isSpam],ylim=c(0,0.6),xlab="count",ylab="SPAM",main="percentCapitals")
abline(h = mean(percentCapitals[isSpam],na.rm=TRUE),lwd=5,col="yellow")
smoothScatter(percentCapitals[!isSpam],xlab="count",ylab="HAM",main="percentCapitals")
abline(h = mean(percentCapitals[isSpam],na.rm=TRUE),lwd=5,col=rainbow(1))
par(mfrow = c(1,1))
boxplot(percentCapitals ~ isSpam,main="percentCapitals",xlab="isSpam",ylab="Percentage",col=rainbow(2))

##I can find that SPAM emails contains more capital letters in the body of the message.
#From the box plot, I can also find the mean of SPAM is higher than HAM. And 75 percentile of SPAM is much more higher than HAM. 
#It seems that it has higher probability in SPAM to have exterme value for this variable.

##13.averageWordLength

summary(averageWordLength[isSpam])
summary(averageWordLength[!isSpam])
par(mfrow = c(1,2))
smoothScatter(averageWordLength[isSpam],xlab="count",ylab="SPAM",main="averageWordLength")
abline(h = mean(averageWordLength[isSpam],na.rm=TRUE),lwd=5,col="yellow")
smoothScatter(averageWordLength[!isSpam],xlab="count",ylab="HAM",main="averageWordLength")
abline(h = mean(averageWordLength[isSpam],na.rm=TRUE),lwd=5,col=rainbow(1))

#I can find that SPAM emails' average word lengths are longer than HAM emails'. 
#From scatter plot, I can also find the same pattern. 
#So this variable is useful to classify the SPAM and HAM.

##14.messageIdHasNoHostname

par(mfrow = c(1,1))
mosaicplot(table(isSpam, messageIdHasNoHostname),shade = TRUE,
           main="Distribution of messageIdHasNoHostname with isSpam",col=rainbow(2))

##Relatively, from mosaic plot, I can find that most of emails without hostnames belong to SPAM emails. 
##This variable is useful to classify SPAM and HAM.

##15.fromnumericEnd

mosaicplot(table(isSpam, fromnumericEnd),shade = TRUE,
           main="Distribution of FromNumericEnd with isSpam",col=rainbow(2))

#It seems that the HAM emails are more likely to have an end without any numbers in the address.
#So from the training dataset, I can  know that if one email has a normal end in the From: field,
#this email is very possible from HAM emails.

##16. hourSent

summary(hourSent[isSpam])
summary(hourSent[!isSpam])
table(hourSent[isSpam])
table(hourSent[!isSpam])
## calculate the mode for SPAM emails
head(sort(table(hourSent[isSpam]),decreasing=TRUE),n=1)
## calculate the mode for HAM emails
head(sort(table(hourSent[!isSpam]),decreasing=TRUE),n=1)

#For this variable, I don't think mean and median can provide useful information to classify SPAM and HAM. 
#But I can calculate mode. It seems that most SPAM emails were sent at 19:00. Most of HAM emails were sent at 08:00. 

#When observing the frequeny tables of hour_Sent for HAM and SPAM. 
#The distribution of HAM emails is not uniform. There are more emails during the work hours(8:00-18:00). 
#On the constrary, the distribution of SPAM email is pretty uniform distributed.

#So if we have some new email data, we can calculate the frequency table for hour_Sent,  
#the distribution is roughly uniform, those emails may come from SPAM emails.

##17.isYelling

mosaicplot(table(isSpam, isYelling),shade = TRUE,
           main="Distribution of isYelling with isSpam",col=rainbow(2))

#It seems that most of HAM emails have the subject not all in the capital letters.
#On the contrary, only a small parts of SPAM emails have the subject in capital letters.

#So if an email's subject is not all capital letters. This email is highly possible to belong to HAM email. 
#If an email's subject is all capital letters. This email is highly possible to belong to SPAM email.


##18. isDear

mosaicplot(table(isSpam, isDear),shade = TRUE,
           main="Distribution of isDear with isSpam",col=rainbow(2))

#It seems that most of HAM emails don't have "Dear" in their body messages. 
#If one email's body message has "Dear", this email is more likely to come from SPAM emails.

##19. numDollarSigns

summary(numDollarSigns[isSpam])
summary(numDollarSigns[!isSpam])
par(mfrow = c(1,2))
smoothScatter(numDollarSigns[isSpam],xlab="count",ylab="SPAM",main="numDollarSigns")
abline(h = mean(numDollarSigns[isSpam],na.rm=TRUE),lwd=5,col="yellow")
smoothScatter(numDollarSigns[!isSpam],xlab="count",ylab="HAM",main="numDollarSigns")
abline(h = mean(numDollarSigns[isSpam],na.rm=TRUE),lwd=5,col=rainbow(1))
boxplot(numDollarSigns ~ isSpam,main="numDollarSigns",xlab="isSpam",ylab="numbers",col=rainbow(2))

#It is obvious that the average number of dollar sign in SPAM emails is much higher than the average number in HAM emails. 
#So this variable is useful for classifying the SPAM and HAM.

##20.  percentSubjectBlanks

summary(percentSubjectBlanks[isSpam])
summary(percentSubjectBlanks[!isSpam])

#It seems that the percent of subject spaces doesn't have big difference between SPAM emails and HAM emails. 
#So this variable may not be very useful for classifying SPAM and HAM.
