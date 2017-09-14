## Calculate balance in savings account after every three months.
# Obtain input.
initialDeposit = eval(input("Enter amount deposited: "))
prompt = "Enter annual rate of interest; such as .02, .03, or .04: "               
annualRateOfInterest = eval(input(prompt))
monthlyRateOfInterest = annualRateOfInterest / 12
# Display table.
print("{0}{1:>15}".format("Month", "Balance"))
for i in range(3, 13, 3):
    print("{0:2}           ${1:<15,.2f}".
        format(i, initialDeposit * (1 + monthlyRateOfInterest) ** i))            
  
