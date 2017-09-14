def main():
    ## Translate an English sentence into textese.
    texteseDict = createDictionary("Textese.txt")
    print("Enter a simple sentence in lowercase letters without")
    sentence = input("any punctuation: ")
    print()
    translate(sentence, texteseDict)
                     
def createDictionary(fileName):
    infile = open(fileName, 'r')
    textList = [line.rstrip() for line in infile]
    infile.close()
    return dict([x.split(',') for x in textList])

def translate(sentence, texteseDict):
    words = sentence.split()
    for word in words:
        print(texteseDict.get(word, word) + " ", end="")
    
main()
