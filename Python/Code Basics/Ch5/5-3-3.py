def main():
    ## Determine an admission fee based on age group.
    print("Enter the person's age group ", end="")
    ageGroup = input("(child, minor, adult, or senior): ")
    print("The admission fee is", determineAdmissionFee(ageGroup), "dollars." )       

def determineAdmissionFee(ageGroup):
    if ageGroup == "child":     # age < 6
        return 0                # free
    elif ageGroup == "minor":   # age 6 to 17
        return 5                # $5
    elif ageGroup == "adult":   # age 18 to 64
        return 10
    elif ageGroup == "senior":  # age >= 65
        return 8

main()


##def determineAdmissionFee(ageGroup):
##    dict = {"child":0, "minor":5, "adult":10, "senior":8}
##    return dict[ageGroup]


