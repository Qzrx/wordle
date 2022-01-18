library(stringr)
library(dplyr)
source('words.R')

# TODO: speed up scoring. Think it chugs in n_unique_letters
# TODO: make n_unique_letters use remaining letters, not all letters

# Helper Functions --------------------------------------------------------
get_pair_chars <- function(char_vec) {
    all_pairs <- paste0(char_vec[-length(char_vec)], char_vec[-1])
    return(as.vector(all_pairs[nchar(all_pairs) == 2]))
}

get_word_bigrams <- function(words){
    unlist(lapply(strsplit(words, ""), get_pair_chars))
}

get_bigrams_frequency <- function(words){
    table(get_word_bigrams(words))
}

score_word_bigrams <- function(words){
    bigram_frequencies <- get_bigrams_frequency(words) / length(words)  # is this normalization necessary?
    word_bigram_tuples = Map(get_word_bigrams, words)
    
    score_word <- function(word_bigrams){
        sum(bigram_frequencies[which(names(bigram_frequencies) %in% word_bigrams)])
    }
    
    unlist(Map(score_word, word_bigram_tuples))
}

is_word_valid <- function(word, Placed_letters =  c('.', '.', '.', '.', '.'), Existing_letters = character(), Scrubbed_letters = character(), Wrongplace_letters){
    # Given a word, a list of known letters, and known placements, return if it's a candidate or not
    # TODO: turn this into a sequence of boolean-AND operations rather than branching if-statements
    placed_regex <- paste('(', paste(Placed_letters, collapse = ''), ')', sep = '')
    scrubbed_regex <- paste('[', paste(Scrubbed_letters, collapse = ''), ']', sep = '')
    exists_regex <- paste('[', paste(Existing_letters, collapse = ''), ']', sep = '')
    wrongplace_regex <- paste('[', Wrongplace_letters, ']', collapse = '')
    word_array <- strsplit(word, '')[[1]]
    
    # First, check if word contains "scrubbed" letters
    if(length(Scrubbed_letters) > 0) {
        if (grepl(scrubbed_regex, word)) {
            return(FALSE)   
        }
    }
    
    # Second, check if word invalidates any matches for known letter positions
    if(!grepl(placed_regex, word)) {
        return(FALSE)
    }
    
    # Check if word contains all known letters
    if (!all(Existing_letters %in% strsplit(word, '')[[1]])) {
        return(FALSE)    
    }
    
    # Check Wrongplace_letters mask
    for (location in 1:5) {
        if (word_array[[location]] %in% Wrongplace_letters[[location]]) {
            return(FALSE)
        }
    }
    
    return(TRUE)
}

# Wordle --------------------------------------------------------
# How in the hell do we build an algorithm for "solving" wordle?
# For each step: 
#     1) Remove words from candidate list that do not match Existing or Placement lists
#     2) Compute most-common-remaining bigrams from candidate list
#     3) Compute words covering most remaining bigrams 
#         In case of tie, sort by fewest known letters. 
#         In case of second tie, sort "best" by most common letters. 
#         In case of remaining tie take first word.
#     4) Guess "best" word, update Existing list, update Placement list 
wordle <- function(target_word, played_words, words, Placed_letters, Existing_letters, Scrubbed_letters, Wrongplace_letters){
    # Given world state, play wordle round
    # Narrow word list to valid words
    words = Filter(function(w){is_word_valid(w, Placed_letters, Existing_letters, Scrubbed_letters, Wrongplace_letters)}, words)
    
    # Calculate most valuable next move, update played words list
    # This is very naive; can do better with tiebreaking but w/e, this should work
    n_char_unique <- function(word){
        word_array = strsplit(word, '')[[1]]
        word_array %>% unique() %>% length()
    }
    
    word_scores = score_word_bigrams(words)
    word_complexity = Map(n_char_unique, names(word_scores)) %>% unlist()  # Change this to unique *remaining* characters
    guessed_word = names(((word_complexity * word_scores) %>% sort() %>% tail(1)))[1]
    word_scores = word_complexity * word_scores
    
    
    played_words = append(guessed_word, played_words)
    words = Filter(function(w){!grepl(guessed_word, w)}, words)
    
    
    # Compare played word to target word, update Existing, Placed, and Scrubbed words
    guessed_word_array = strsplit(guessed_word, '')[[1]]
    target_word_array = strsplit(target_word, '')[[1]]
    
    # Updates the indexes of the Placed_letters list with the values of the boolean mask of the guessed + target words
    Placed_letters[which(guessed_word_array == target_word_array)] <- guessed_word_array[which(guessed_word_array == target_word_array)]
    
    # Updates the Existing_letters array
    Existing_letters = append(Existing_letters, intersect(guessed_word_array, target_word_array)) %>% unique()
    
    # Updates the Scrubbed_letters array
    Scrubbed_letters = append(Scrubbed_letters, setdiff(guessed_word_array, target_word_array)) %>% unique()
    
    # Updates the Wrongplace_letters array
    Wrongplace_mask <- !(target_word_array == guessed_word_array) & (guessed_word_array %in% Existing_letters)  # For letters that aren't placed AND exist
    for (location in 1:5) {
        if (Wrongplace_mask[[location]]) {
            # Add the misplaced letter to the list
            Wrongplace_letters[[location]] <- append(Wrongplace_letters[[location]], guessed_word_array[[location]])
        }
    }
    
    # Updates the Wrongplace_letters array
    # Wrongplace is a mask that says where Existing_letters can't exist -> take guessed word, and if a letter exists but
    #     is in the wrong place, add that to the mask at that location
    
    cat(paste('Guessed: ', guessed_word, '\n'))
    cat(paste('Placed: ', paste(Placed_letters, collapse = ''), '\n'))
    cat(paste('Existing: ', paste(Existing_letters, collapse = ''), '\n'))
    cat(paste('Scrubbed: ', paste(Scrubbed_letters, collapse = ''), '\n'))
    cat(paste('Valid remaining words: ', length(words), '\n'))
    
    # Return world state
    return(list('words' = words, 
        'played_words' = played_words,
        'Placed_letters' = Placed_letters,
        'Existing_letters' = Existing_letters, 
        'Scrubbed_letters' = Scrubbed_letters,
        'Wrongplace_letters' = Wrongplace_letters)
    )
}


Wordle <- function(target_word, rounds = 6, Words = append(La, Ta)){
    # Initial round always runs to set state
    Placed_letters <- c('.', '.', '.', '.', '.')
    Wrongplace_letters <- list('a-z', 'a-z', 'a-z', 'a-z', 'a-z')
    Existing_letters <- character()
    Scrubbed_letters <- character()
    played_words = c()
    
    # Run any remaining rounds
    for (round in 1:rounds) {
        cat(paste('\nRound ', round, '\n'))
        w = wordle(target_word, played_words, Words, Placed_letters, Existing_letters, Scrubbed_letters, Wrongplace_letters)
        played_words = w$played_words
        Words = w$words
        Placed_letters = w$Placed_letters
        Existing_letters = w$Existing_letters
        Scrubbed_letters = w$Scrubbed_letters
        Wrongplace_letters = w$Wrongplace_letters
        
        if(played_words[1] == target_word){
            print(paste('Victory in ', round, ' rounds!'))
            break
        }
    }
    
    return(round)
}

Words_list = append(La, Ta)
Wordle('panic', 6, Words_list)
