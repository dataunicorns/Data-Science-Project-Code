def isPalindrome(word):
    word = word.lower()        # Convert all letters to lowercase.
    if len(word) <= 1:         # Words of zero or one letters are palindromes.
        return True          
    elif word[0] == word[-1]:  # First and last letters match.
        word = word[1:-1]      # Remove first and last letters.
        return isPalindrome(word)
    else:
        return False
    
      

