import pickle

def main():
    ## Display the data for an individual country.
    nations = getDictionary("UNdict.dat")
    nation = inputNameOfNation(nations)
    displayData(nations, nation)

def getDictionary(fileName):    
    infile = open(fileName, 'rb')
    nations = pickle.load(infile)
    infile.close()
    return nations

def inputNameOfNation(nations):
    nation = input("Enter the name of a UN member nation: ")
    while nation not in nations:
        print("Not a member of the UN. Try again.")
        nation = input("Enter the name of a UN member nation: ")
    return nation    

def displayData(nations, nation):
    print("Continent:", nations[nation]["cont"])
    print("Population:", nations[nation]["popl"], "million people")   
    print("Area:", nations[nation]["area"], "square miles")

main()

