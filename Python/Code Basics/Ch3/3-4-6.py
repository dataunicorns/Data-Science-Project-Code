## Reverse the letters in a word.
word = input("Enter a word: ")
reversedWord = ""
for ch in word:
    reversedWord = ch + reversedWord
print("The reversed word is " + reversedWord + ".")
