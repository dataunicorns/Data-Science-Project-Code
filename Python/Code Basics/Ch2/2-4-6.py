regions = [("Northeast", 55.3), ("Midwest", 66.9),
           ("South", 114.6), ("West", 71.9)]
print("The 2010 population of the", regions[1][0], "was", regions[1][1],
      "million.")
totalPop = regions[0][1] + regions[1][1] + regions[2][1] + regions[3][1]
print("Total 2010 population of the U.S: {0:.1f} million.".format(totalPop))
      
