## Bestow graduation honors.
# Request grade point average.
gpa = eval(input("Enter your gpa: "))
# Determine if honors are warranted.
if gpa >= 3.9:
    honors = " summa cum laude."
elif gpa >= 3.6:
    honors = " magna cum laude."
elif gpa >= 3.3:
    honors = " cum laude."
elif gpa >= 2:
    honors = "."
    # Display conclusion.
print("You graduated" + honors)




