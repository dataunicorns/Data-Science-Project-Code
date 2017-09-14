fullName = input("Enter a full name: ")
n = fullName.rfind(" ")
print("Last name:", fullName[n+1:])
print("First name(s):", fullName[:n])
