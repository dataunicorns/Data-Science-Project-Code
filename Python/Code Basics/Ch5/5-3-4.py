def main():
    ## Analyze word frequencies in the Gettysburg Address.
    listOfWords = formListOfWords("Gettysburg.txt") 
    freq = createFrequencyDictionary(listOfWords)
    displayWordCount(listOfWords, freq)
    displayMostCommonWords(freq)
        
def formListOfWords(fileName):
    infile = open(fileName)
    originalLine = infile.readline().lower()
    # Remove punctuation marks from the line.
    line = ""
    for ch in originalLine:
        if ('a' <= ch <= 'z') or (ch == " "):
            line += ch
    # Place the individual words into a list.
    listOfWords = line.split()
    return listOfWords    

def createFrequencyDictionary(listOfWords):
    ## Create dictionary with each item having the form word:word frequency.
    freq = {}   # an empty dictionary    
    for word in listOfWords:    
        freq[word] = 0
    for word in listOfWords:
        freq[word] = freq[word] + 1
    return freq

def displayWordCount(listOfWords, freq):
    print("The Gettysburg Address contains", len(listOfWords),"words.")
    print("The Gettysburg Address contains", len(freq), "different words.")
    print()
    
def displayMostCommonWords(freq):
    print("The most common words and their frequencies are:")
    listOfMostCommonWords = []
    for word in freq.keys():
        if freq[word] >= 6:
            listOfMostCommonWords.append((word, freq[word]))
    listOfMostCommonWords.sort(key=lambda x: x[1], reverse=True)
    for item in listOfMostCommonWords:
        print("   ", item[0] + ':', item[1])
        
main()


