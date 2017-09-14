def main():
    ## Create a text file containing the 50 states in alphabetical order.
    statesList = getListFromFile("States.txt")
    createSortedFile(statesList, "StatesAlpha.txt")

def getListFromFile(fileName):
    infile = open(fileName, 'r')
    desiredList = [line.rstrip() for line in infile]
    infile.close()
    return desiredList

def createSortedFile(listName, fileName):
    listName.sort()
    for i in range(len(listName)):
        listName[i] = listName[i] + "\n" 
    outfile = open(fileName, 'w')
    outfile.writelines(listName)
    outfile.close()
    
main()    

