
mapTextSL<- function(sent, ppl= 11, xOffset= 50, ResY= 1080){
    
    ###
    char<- 0:(nchar(string)-1)
    letter<- unlist(strsplit(string, ''))
    space<- rep(NA, nchar(string))
    space[which(letter==" ")]<- 1
    line<- rep(1, nchar(string))
    line_char<- char+1
    y1<- rep(ResY/2, nchar(string))- ppl*2
    y2<- rep(ResY/2, nchar(string))+ ppl*2
    x1<- rep(NA, nchar(string))
    x2<- rep(NA, nchar(string))
    sent<- rep(NA, nchar(string))
    word<- rep(NA, nchar(string))
    wordID<- rep(NA, nchar(string))
    char_word<- rep(NA, nchar(string))
    
    coords<- data.frame(char, letter, x1, y1, x2, y2, space, sent,
                        line, word, line_char, wordID, char_word)
    
    count<- xOffset
    word_count<- 1
    sent_count<- 1
    for(j in 1:nrow(coords)){
      if(coords$letter[j]== " "){
        word_count<- word_count+1
      }
    coords$sent[j]<- sent_count  
    coords$word[j]<- word_count
    coords$x1[j]<- count
    coords$x2[j]<- count+ ppl
    count<- count + ppl
      
    if(coords$letter[j]== "." | coords$letter[j]== "!" | coords$letter[j]== "?"){
        word_count<- 1 # new sentence, reset word count
        sent_count<- sent_count+1 # next sent
      }
    }
    
    
    # word identity and char per word:
    word_list<- NULL; cols<- NULL
    sent_list= unique(coords$sent)
    
    for(i in 1: length(sent_list)){ # for each sentence
      word_list<- unique(coords$word[which(coords$sent==i)])
      
      for(j in 1:length(word_list)){
        cols<- which(coords$sent == i & coords$word == j)
        coords$wordID[cols]<- paste(coords$letter[cols], collapse = "")
        
        if(coords$letter[cols[1]]==""){
          coords$char_word[cols]<- 0:(length(cols)-1)
        }else{
          if(j==1 & i==1){
            coords$char_word[cols]<- 1:length(cols)
          }else{
            coords$char_word[cols]<- 1:length(cols)-1
          }
          
        }
        
      }
    }
    return(coords)
  
#  message("\n\nPlease always check the stimuli dimensions to make sure they are correct!\n")
#  message("The stimuli mapping was developed with English stimuli, although it should 
#          in theory also work with other (spaced) alphabetical languages.\n")
#  message("\nIf you notice any errors or inconsistencies, please let the developer know.\n")
#  
  
}
