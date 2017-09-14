def main():
    ## Create a file of the presidents who also served as vice-presidents.
    vicePresSet = createtSetFromFile("VPres.txt")
    presSet = createtSetFromFile("USPres.txt")
    bothPresAndVPresSet = createIntersection(vicePresSet, presSet)
    writeNamesToFile(bothPresAndVPresSet, "PresAndVPres.txt")
    
def createtSetFromFile(fileName):
    # Assume that the last line of the file ends with a newline character.
    infile = open(fileName, 'r')
    namesSet = {name for name in infile}
    infile.close()
    return namesSet

def createIntersection(set1, set2):
    return set1.intersection(set2)
    
def writeNamesToFile(setName, fileName):
    outfile = open(fileName, 'w')
    outfile.writelines(setName)
    outfile.close()
    
main()                  

