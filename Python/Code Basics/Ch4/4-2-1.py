def main():
    ## Demonstrate functions calling other functions.
    firstPart()
    print(str(4) + ": from function main")

def firstPart():
    print(str(1) + ": from function firstPart")
    secondPart()
    print(str(3) + ": from function firstPart")

def secondPart():
    print(str(2) + ": from function secondPart")

main()
