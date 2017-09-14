
def main():
    ## Custom sort a list of words.
    list1 = ["democratic", "sequoia", "equals", "brrr", "break", "two"]
    list1.sort(key=len)
    print("Sorted by length in ascending order:")
    print(list1, '\n')
    list1.sort(key=lastCharacter)
    print("Sorted by last character in ascending order:")
    print(list1, '\n')
    list1.sort(key=numberOfVowels, reverse = True)
    print("Sorted by number of vowels in descending order:")
    print(list1)

def lastCharacter(value):
    return value[-1]

def numberOfVowels(word):
    vowels = ('a', 'e', 'i', 'o', 'u')
    total = 0
    for vowel in vowels:
        total += word.count(vowel)
    return total

main()




        









 
