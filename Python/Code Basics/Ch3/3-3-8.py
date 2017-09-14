## Display facts about the United States.
print("Enter a number from the menu to obtain a fact")
print("about the United States or to exit the program.\n")
print("1. Capital")
print("2. National Bird")
print("3. National Flower")
print("4. Quit\n")
while True:
    num = int(input("Make a selection from the menu: "))
    if num == 1:
        print("Washington, DC is the capital of the United States.")
    elif num == 2:
        print("The American Bald Eagle is the national bird.")
    elif num == 3:
        print("The Rose is the national flower.")
    elif num == 4:
        break


        


