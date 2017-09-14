INTEREST_RATE = .04    # annual rate of interest

def main():
    ## Calculate the balance and interest earned from a savings account.
    (principal, numberOfYears) = getInput()
    bal, intEarned = balanceAndInterest(principal, numberOfYears)
    displayOutput(bal, intEarned)
    
def getInput():
    principal = eval(input("Enter the amount of the deposit: "))
    numberOfYears = eval(input("Enter the number of years: "))
    return (principal, numberOfYears)

def balanceAndInterest(prin, numYears):
    balance = prin * ((1 + INTEREST_RATE) ** numYears)
    interestEarned = balance - prin                   
    return (balance, interestEarned)

def displayOutput(bal, intEarned):
    print("Balance: ${0:,.2f}   Interest Earned: ${1:,.2f}". 
           format(bal, intEarned))

main()



