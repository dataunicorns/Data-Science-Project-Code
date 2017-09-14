## Evaluate profit.
# Obtain input from user.
costs = eval(input("Enter total costs: "))
revenue = eval(input("Enter total revenue: "))
# Determine and display profit or loss.
if costs == revenue:
    result = "Break even."
else:
    if costs < revenue:
        profit = revenue - costs
        result = "Profit is ${0:,.2f}.".format(profit)
    else:
        loss = costs - revenue
        result = "Loss is ${0:,.2f}.".format(loss)
print(result)





