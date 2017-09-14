import pickle

def main():
    ## Display countries (and their population) from a specified continent.
    nations = getDictionary("UNdict.dat")
    continent = input("Enter the name of a continent other than Antarctica: ")
    continentDict = constructContinentNations(nations, continent)
    displaySortedResults(continentDict)

def getDictionary(fileName):    
    infile = open(fileName, 'rb')
    countries = pickle.load(infile)
    infile.close()
    return countries

def constructContinentNations(nations, continent):
    ## Reduce the full 193 item dictionary to a dictionary consisting  
    ## solely of the countries in the specified continent.

    continentDict = {}  # an empty dictionary
    for nation in nations:
        if nations[nation]["cont"] == continent:
            continentDict [nation] = nations[nation]
    return continentDict

def displaySortedResults(dictionaryName):
    ## Display countries in descending order by population.
    continentList = sorted(dictionaryName.items(),
                     key=lambda k: k[1]["popl"], reverse=True)
    for k in continentList:
         print("  {0:s}: {1:,.2f}".format(k[0], k[1]["popl"]))
         
main()                        

