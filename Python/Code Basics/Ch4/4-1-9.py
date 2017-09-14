def main():
    ## Calculate the population density of Hawaii.
    describeTask()
    calculateDensity("Hawaii", 1375000, 6423)

def describeTask():
    print("This program displays the population")
    print("density of the last state to become")
    print("part of the United States.\n")

def calculateDensity(state, pop, landArea):
    density = pop / landArea
    print("The density of", state, "is")
    print("{0:,.2f} people per square mile.".format(density))

main()



