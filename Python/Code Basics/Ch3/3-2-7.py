## Calculate FICA tax for a single employee.
# Obtain earnings.
str1 = "Enter total earnings for this year prior to current pay period: "
ytdEarnings = eval(input(str1))    # year-to-date earnings
curEarnings = eval(input("Enter earnings for the current pay period: "))
totalEarnings = ytdEarnings + curEarnings
# Calculate the Social Security Benefits tax.
socialSecurityBenTax = 0
if totalEarnings <= 117000:
    socialSecurityBenTax = 0.062 * curEarnings
elif ytdEarnings < 117000:
    socialSecurityBenTax = 0.062 * (117000 - ytdEarnings)
# Calculate and display the FICA tax. 
medicareTax = 0.0145 * curEarnings
if ytdEarnings >= 200000:
    medicareTax += 0.009 * curEarnings
elif totalEarnings > 200000:
    medicareTax += 0.009 * (totalEarnings - 200000)
ficaTax = socialSecurityBenTax + medicareTax
print("FICA tax for the current pay period: ${0:0,.2f}".format(ficaTax))



