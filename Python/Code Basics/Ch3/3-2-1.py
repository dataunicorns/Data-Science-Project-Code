## Determine the larger of two numbers.
# Obtain the two numbers from the user.
num1 = eval(input("Enter the first number: "))
num2 = eval(input("Enter the second number: "))
# Determine and display the larger value.
if num1 > num2: 
    largestValue = num1  # execute this statement if the condition is true
else:
    largestValue = num2  # execute this statement if the condition is false
print("The larger value is", str(largestValue) + ".")


