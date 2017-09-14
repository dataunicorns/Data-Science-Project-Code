## Calculate the number of years to become a millionaire.
numberOfYears = 0
balance = eval(input("Enter initial deposit: "))
while balance < 1000000:
    balance += .04 * balance
    numberOfYears += 1
print("In", numberOfYears, "years you will have a million dollars.")
               

               
