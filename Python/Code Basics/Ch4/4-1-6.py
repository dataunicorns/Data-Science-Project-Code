def isVowelWord(word):
    word = word.upper()
    vowels = ('A', 'E', 'I', 'O', 'U')
    for vowel in vowels:
        if vowel not in word:
            return False
    return True

## Determine if a word contains every vowel.
word = input("Enter a word: ")
if isVowelWord(word):
    print(word, "contains every vowel.")
else:
    print(word, "does not contain every vowel.")

