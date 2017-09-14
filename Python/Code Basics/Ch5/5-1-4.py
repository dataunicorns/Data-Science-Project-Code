def main():
    ## Create a file of the presidents who also served as vice-presidents.
    vicePresList = getListFromFile("VPres.txt")
    createNewFile(vicePresList, "USPres.txt", "Both.txt")

def getListFromFile(fileName):
    infile = open(fileName, 'r')
    desiredList = [line.rstrip() for line in infile]
    infile.close()
    return desiredList

def createNewFile(listName, oldFileName, newFileName):
    infile = open(oldFileName, 'r')
    outfile = open(newFileName, 'w')
    for person in infile:
        if person.rstrip() in listName:
            outfile.write(person)
    infile.close()
    outfile.close()

main()                  


