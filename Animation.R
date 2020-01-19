
fps= 25
interval= 1/fps

ResX= 1920
ResY= 1080
border_color= "white" #"#CFCBCB" #

trial= 41;

#file= 'D:/Data/DEVS2/DEVSN1.asc'
file="D:/Data/Font_size/RSFNT1.asc" 
source("R/utility.R")

asc<- readLines(file)

trial_db<- trial_info(file = asc, maxtrial = 999, selectEXP = T)
db<- trial_db[trial,]

text<- get_text(file = asc[db$ID:db$start])
coords<- get_coord(text)
map<- coord_map(coords, x = ResX, y = ResY)

# Extract raw fixations from data and map them to the text:
#if(max(coords$line)>1){
  
#}else{
  try(raw_fix<- parse_fix(asc, map, coords, db, trial, ResX, ResY, tBlink=150, SL= TRUE))
#}



remap_letters<- function(letter, y, keep= TRUE){ # adjusted y position for easier reading
  letter<- as.character(letter)
  ascenders<- c("b", "d", "f", "h", "i", "k", "l", "t")
  descenders<- c("g", "j", "p", "q", "y")
  punct<- c(",", ".")
  t<- 't'
  caps<- which(grepl("[A-Z]",letter))
  
  which_asc<- which(is.element(letter, ascenders))
  which_desc<- which(is.element(letter, descenders))
  which_punct<- which(is.element(letter, punct))
  which_t<- which(is.element(letter, t))
  
  if(keep){
    y[which_desc]<- y[which_desc]- 2
    y[which_asc]<- y[which_asc]+1.5
    y[which_t]<- y[which_t]-1
    y[which_punct]<- y[which_punct]-4
    y[caps]<- y[caps]+1
  }else{
    y[which_asc]<- y[which_asc]+2.5
  }

  return(y)
}

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

# plot text coordinates:

#png(filename = 'test.png', width = ResX, height = ResY,
#    units = "px", pointsize = 12, bg="white", res = 100)

#par(mar=c(0,0,0,0)) # set plot margins to 0
#plot(NA, axes=F, main="", xlab="" ,ylab="", col="black", ylim=c(0, ResY), xlim=c(0, ResX), xaxs="i", yaxs="i")

#text(x= coords$x1[1], y= coords$y1[1], label= 'lol')
# Plot stimuli that appeared on screen:
# rect(coords$x1, ResY-coords$y1, coords$x2, ResY-coords$y2, col= "white", border= border_color) #"#CFCBCB")
# 
linesT<- NULL

for(i in 1:max(coords$line)){
  temp<- subset(coords, line== i)
  x<- temp$x1+ (temp$x2- temp$x1)/2
  y<- ResY-temp$y1- (temp$y2- temp$y1)/2
  
  if(i>1){
    y<- remap_letters(temp$letter, y, keep= FALSE) 
  }else{
    y<- remap_letters(temp$letter, y) 
  }
  
  
  xpos= c(xpos, x)
  ypos= c(ypos, y)
  
  lets<- temp$letter
  lets[which(lets=="")]<- ' '
  string= paste(lets, collapse= '')
  linesT<- c(linesT, string)
}



 #xpos= coords$x1+ (coords$x2- coords$x1)/2
 #ypos= ResY-coords$y1- (coords$y2- coords$y1)/2
# 
# # correct y pos of some letter for better readibility:
 

# 
# # print letters:
# text(xpos, ypos, coords$letter, col= "black", family='mono')
# 
# dev.off()

##
# generate animation sequence
start<- raw_fix$SFIX[1]
end<- raw_fix$EFIX[nrow(raw_fix)]
time_span<- end-start 

nframes= round(time_span/(interval*1000))

f_x<- NULL
f_y<- NULL
f_time<- NULL
f_dur<- NULL
f_cex<- NULL

curr_cex<- 1.2

time<- interval*1000

for(i in 1:nframes){
  pos<- which(raw_fix$SFIX> start+time)[1]-1
  f_time[i]<- time
  f_x[i]<- raw_fix$xPos[pos]
  f_y[i]<- raw_fix$yPos[pos]
  f_dur[i]<- raw_fix$fix_dur[pos] 
  
  time<- time+ interval*1000
  
  if(is.na(f_dur[i])){
    f_cex[i]<- NA
    next
  }
  
  if(i>1){
    
    if(f_dur[i]== f_dur[i-1]){
      curr_cex<- curr_cex+0.3
      f_cex[i]<- curr_cex
    }else{
      curr_cex<- 1.2
      f_cex[i]<- curr_cex
    }
    
  }    else{
    f_cex[i]<- curr_cex
    curr_cex<- curr_cex+0.3
  }
  

}

library(animation)

saveGIF({
  
  ani.options(interval = interval)
  
  for (i in 1:length(f_time)) {
    
    par(mar=c(0,0,0,0)) # set plot margins to 0
    plot(NA, axes=F, main="", xlab="" ,ylab="", col="black", ylim=c(0, ResY), xlim=c(0, ResX), xaxs="i", yaxs="i")
    
    # letter boxes
    rect(coords$x1, ResY-coords$y1, coords$x2, ResY-coords$y2, col= "white", border= border_color) #"#CFCBCB")
    
    # print letters:
    #text(xpos, ypos, coords$letter, col= "black", family='mono')
    
    for(j in 1:length(linesT)){
      tc<- subset(coords, line==j)
      text(tc$x1[1]-15, ResY- tc$y1-(tc$y2[1]-tc$y1[1])/2, linesT[j], col= "black", family='mono', pos=4, cex=0.8)

    }
    
    # fixations:
    points(x= f_x[i], y=ResY-f_y[i], pch = 16,  col= add.alpha("#52ff33",0.40),
           cex= f_cex[i])
    points(x= f_x[i], y=ResY-f_y[i], pch = 16, cex=0.7, col="#147823")
    
    # saccades
    if(i>1){
      if(!is.na(f_x[i])){
        if(f_x[i]!=f_x[i-1]){
          arrows(f_x[i-1], ResY-f_y[i-1], f_x[i], ResY-f_y[i], col = add.alpha("red",0.4), lty= 1, lwd=2, length=0.10)
        }
      }
      
    }
    
    cat(i); cat(" ")
    ani.pause() }
  
}, movie.name = 'test.gif',

ani.width = ResX/2, ani.height = ResY/2)


