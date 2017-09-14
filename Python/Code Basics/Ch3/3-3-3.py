## Find the minimum, maximum, and average of a sequence of numbers. 
count = 0   # number of nonnegative numbers input
total = 0   # sum of the nonnegative numbers input
# Obtain numbers and determine count, min, and max.
print("(Enter −1 to terminate entering numbers.)")
num = eval(input("Enter a nonnegative number: "))
min = num
max = num
while num != -1:
    count += 1
    total += num
    if num < min:
        min = num
    if num > max:
        max = num
    num = eval(input("Enter a nonnegative number: "))
# Display results.
if count > 0:
    print("Minimum:", min)
    print("Maximum:", max)
    print("Average:", total / count)
else:
    print("No nonnegative numbers were entered.") 

                    










    




 



    
