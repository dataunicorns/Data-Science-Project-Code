def main():
    ## Request that the user enter a proper response.
    phoneticAlphabet = {'a':"alpha", 'b':"bravo", 'c':"charlie"}
    while True:
        try:
            letter = input("Enter a, b, or c: ")
            print(phoneticAlphabet[letter])
            break
        except KeyError:
            print("Unacceptable letter was entered.")

main()        
        
