
split_one_email = function(file){
file = readLines(file)
## find the files that are not actual emails.
if(grepl("^mv ", file[1])) { 
print ("This is not email.")
return(NULL)
}

## find the boundary for head and body in the email.
head_body = match("",file)

## check if the first line belongs to the header.
if(grepl("^From",file[1]) == TRUE){
header = file[2:(head_body-1)]
## get the corresponding formatted header
header = get_header(header)
body  = file[(head_body+1):length(file)] 
}

if(grepl("^From" ,file[1]) == FALSE){
header = file[1:(head_body-1)] 
## get the corresponding formatted header
header = get_header(header)
body  = file[(head_body+1):length(file)] 

}
## call the corresponding function to split the body part 
real_body = split_body(header,body)
## genenrate a list, the first element is the header of the email, the second part is the body, which contains attachments and rest body parts.
list(email_header = header, email_body = real_body)
}

get_header = function(header){
header = textConnection(header)
header = read.dcf(header,all = TRUE)
## convert the data.frame into a list.
header_v = c(t(header))
## use the corresponding key vaule as the element's name.
names(header_v) = names(header)
## convert the list into a vector
unlist(header_v)
}

split_body = function(header,body){
  
  ## generate the boundary
  ##generate a character vector to store boundary string
  boundary=c()
  ## check if the boundary string exists
  if (grepl("boundary", header["Content-Type"]) == TRUE){
    ## find the pattern of the boundary string
    ## remove the irrelevant characters and store the boundary string into a new vector
    boundary = gsub(".*boundary= *([^;]*);?.*", "\\1", header["Content-Type"])
  }
  
  ## If we cannot find the pattern of the boundary string, the boundary doesn't exist. We should return null value.
  else {
    boundary = NULL
  }
  
  boundary
  ## if the body doesn't contain any attachements, we return the original body part
  if(is.null(boundary)){
    return(list(body_message = body))    
  }
  
  ## Usually ,the boundary line will start with "--" , so I need to generate a new string to combine these seperated strings.
  
  string_begin = paste("--",boundary,sep="")
  
  ## The end of one attachment is "--"
  string_end = paste(string_begin,"--",sep="")
  
  ## figure out the line numbers where the boundary string appears
  
  ## for beginning part, if length(begin_1) >0 , it means there are several attachment
  begin_1 = which(body == string_begin)
  
  ## The line numbers for the end of the attachments
  end_1 = which(body == string_end)
  ## consider cases that some attachments end at the end of the whole body.
  if(length(end_1)==0){
    end_1 = length(body)
  }
  
  position_1 = c(begin_1, end_1)
  
  ##consider the cases that we cannot find the boundary and return the whole body
  if (length(position_1) == 0){
    print("Cannot find boundary for the attachment")
    return(list(body_message = body))
  }
  position_min_1 = min(position_1)
  position_max_1 = max(position_1)
  
  ## generate the body part excluding all the attachments
  
  rest_body = body[-seq(position_min_1,position_max_1)]
  
  ## generate the attachment part
  ## We should use the exactly same way to split header as what I have done for 
  attachment = body[seq(position_min_1,position_max_1)]
  ## In order to split the attachment, I need to relocate the location of boundary line.
  begin_2 = which(attachment == string_begin)
  end_2 = which(attachment == string_end)
  ## consider the cases that there are no ending boundary string for the attachments.
  if(length(end_2)==0){
    end_2 = length(attachment)
  }
  position_2 = sort(c(begin_2, end_2))
  ## split the attachments 
  spt = list()
  for(i in 1:(length(position_2)-1)) {
    ## find the lines between two boundary, which is a attachment.
    part = (position_2[i] + 1):(position_2[i+1]-1)
    ## call "splitAttachment" to split the specific attachment's header and body.
    spt[[i]] = splitAttachment(attachment[part])
  }
  ## return a list of the rest text and the attachments
  list(body=rest_body,attachment = spt)
} 

splitAttachment = function(input)
{
  ## find the boundary between header and body
  attach_sep = match("", input)
  ## consider the cases that there is no actual header for the attachment
  if(attach_sep==1){
    header = input[1]
  }
  else{
    header = input[1:(attach_sep-1)]
    ## call function "get_header" to deal with the header part.
    header = get_header(header)
  }
  list(header = header, body = input[-c(1:attach_sep)])
}


##Generate the training data set

setwd("/Users/bruce/Desktop/STA 141 assignment3/SpamAssassinTraining")
paths = list.files("/Users/bruce/Desktop/STA 141 assignment3/SpamAssassinTraining",recursive=TRUE)

## generate a list to store all the results.
TrainMessages = list()  
for (i in 1:length(paths)){
  TrainMessages[[i]] = split_one_email(paths[i])   
}
names(TrainMessages) = paths
TrainMessages
save(TrainMessages, file = "TrainingMessages.rda")
}




