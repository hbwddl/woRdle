## Text-based wordle
generate_word <- function(wordlen=5,common=F){
  if(common){
    wordbank <- unique(c(lexicon::sw_fry_1000,lexicon::sw_loughran_mcdonald_short,
                         lexicon::sw_dolch,lexicon::sw_mallet,lexicon::pos_action_verb,
                         lexicon::pos_preposition))
    return(toupper(sample(wordbank[which(nchar(wordbank) == wordlen)],size=1)))
  } else{
    return(toupper(sample(words::words[which(words::words$word_length == wordlen),1],size=1)))
  }
}

## Function for checking against true word
## returns -1 for right letter wrong place, 0 for wrong letter wrong place, 1 for right letter right place
check_word <- function(guess_word,true_word){
  wordlen <- nchar(guess_word)
  guess_list <- unlist(strsplit(guess_word,split=""))
  true_list <- unlist(strsplit(true_word,split=""))
  guess_result <- rep(0,wordlen)
  
  guess_freq <- table(guess_list)
  true_freq <- table(true_list)
  
  guess_letters <- names(guess_freq)
  
  freq_count <- rep(NA,wordlen)
  
  for(i in 1:length(guess_letters)){
    freq_count[guess_list==guess_letters[i]] <- 1:guess_freq[i]
  }
  
  for(i in 1:wordlen){
    if(guess_list[i] == true_list[i]){
      guess_result[i] <- 1
    }
  }
  for(i in 1:wordlen){
    if(guess_list[i] %in% true_list & guess_list[i] != true_list[i]){
      if(guess_freq[guess_list[i]] <= true_freq[guess_list[i]]){
        guess_result[i] <- -1
      } else{
        n_match <- sum(guess_result[true_list==guess_list[i]])
        if((freq_count[i] + n_match) <= true_freq[guess_list[i]]){
          guess_result[i] <- -1
        }
      }
    }
  }
  
  return(guess_result)
}

check_word_text <- function(guessed){
  text.check <- rep(0,length(guessed))
  for(i in 1:length(guessed)){
    if(guessed[i] == -1){
      text.check[i] <- "/"
    } else if(guessed[i] == 1){
      text.check[i] <- "X"
    }
  }
  
  string.check <- paste0(text.check,collapse="")
  
  return(string.check)
}

remaining_letters <- function(guessed.letters,guessed.values){
  guessed.upper <- toupper(guessed.letters)
  ind.letters <- unlist(strsplit(guessed.upper,""))
  not.letters <- ind.letters[guessed.values==0]
  
  remaining <- LETTERS[!(LETTERS %in% not.letters)]
  
  return(remaining)
}

woRdle <- function(word.length=5,max.guesses=5,challenge=F){
  require(words)
  require(dplyr)
  require(lexicon)
  
  true.word <- toupper(generate_word(word.length,!challenge))
  n.guesses <- 0
  guessed.words <- ""
  guessed.checks <- c()
  
  if(challenge){
    wordbank <- toupper(words::words[which(words::words$word_length == wordlen),1])
  } else{
    wordbank <- toupper(unique(c(lexicon::sw_fry_1000,lexicon::sw_loughran_mcdonald_short,
                         lexicon::sw_dolch,lexicon::sw_mallet,lexicon::pos_action_verb,
                         lexicon::pos_preposition)))
  }
  
  print(paste0("Word Length: ", word.length,"; Max Guesses: ",max.guesses))
  print("0 = wrong letter; / = right letter, wrong place; X = right letter, right place")
  print("Enter q to quit")
  print("Use option challenge=T to use all scrabble words as a wordbank")
  
  while(n.guesses <= max.guesses){
    print(paste0("Guesses remaining: ",max.guesses - n.guesses))
    guess.val <- toupper(readline(prompt="GUESS: "))
    
    if(guess.val == "Q"){
      break
    }
    if(guess.val == true.word){
      print("YOU WIN!")
      break
    }
    if(!(guess.val %in% wordbank)){
      print("Not in word bank")
      next
    }
    if(nchar(guess.val) != nchar(true.word)){
      print("Incorrect number of characters")
      next
    }

    guess.check <- check_word_text(check_word(guess.val,true.word))
    print(guess.check)
    
    guessed.words <- paste0(guessed.words,guess.val,sep="")
    guessed.checks <- c(guessed.checks,check_word(guess.val,true.word))
    
    remaining.letters <- remaining_letters(guessed.words,guessed.checks)
    
    print(paste0("Remaining letters: ",paste0(remaining.letters,collapse=" ")))
    
    n.guesses <- n.guesses + 1
  }
  
  print(paste0("Answer: ",true.word,sep=""))
}