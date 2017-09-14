def main():
    ## Do statistical analysis of countries areas.
    areasAsStrings = extractField("UN.txt", 4)   # Place areas into a list.
    areas = [eval(num) for num in areasAsStrings]
    displaySomeStatistics(areas)

def extractField(fileName, n):
    # Extract the nth field from each record of a CSV file
    # and place the data into a list.
    return [line.rstrip().split(',')[n - 1] for line in open(fileName, 'r')]

def displaySomeStatistics(listOfNumbers):
    ## Display the average, median, and standard deviation of the areas.
    average = sum(listOfNumbers) / len(listOfNumbers)
    median = calculateMedian(listOfNumbers)
    standardDeviation = calculateStandardDeviation(listOfNumbers, average)
    print("Average area: {0:,.2f} square miles".format(average))
    print("Median area: {0:,d} square miles".format(median))
    print("Standard deviation: {0:,.2f} square miles".format(standardDeviation))
    
def calculateMedian(listOfNumbers):
    listOfNumbers.sort()
    if len(listOfNumbers) % 2 == 1:
        median = listOfNumbers[int(len(listOfNumbers) / 2)]   # middle number
    else:
        # Median will be the average of the two middle numbers.
        m = int(len(listOfNumbers) / 2)
        median = (listOfNumbers[m] + listOfNumbers[m + 1]) / 2
    return median

def calculateStandardDeviation(listOfNumbers, average):
    m = average
    n = len(listOfNumbers)
    listOfSquaresOfDeviations = [0] * n
    for i in range(n):
        listOfSquaresOfDeviations[i] = (listOfNumbers[i] - m) ** 2
    standardDeviation = (sum(listOfSquaresOfDeviations) / n) ** .5
    return standardDeviation

main()



