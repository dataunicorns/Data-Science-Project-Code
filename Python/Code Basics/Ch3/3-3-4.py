## Find the minimum, maximum, and average of a sequence of numbers.
# Obtain list of numbers.
list1 = []
print("(Enter −1 to terminate entering numbers.)")
num = eval(input("Enter a nonnegative number: "))
while num != -1:
    list1.append(num)
    num = eval(input("Enter a nonnegative number. "))
# Display results
if len(list1) > 0:
    list1.sort()
    print("Minimum:", list1[0])
    print("Maximum:", list1[-1])
    print("Average:", sum(list1) / len(list1))
else:
    print("No nonnegative numbers were entered.")
