import lgStudent

def main():
    ## Calculate and display several students' semester letter grades.
    listOfStudents = []  # List to hold an object for each student.
    carryOn = 'Y'
    while carryOn == 'Y':
        st = lgStudent.LGstudent()
        # Obtain student's name, grade on midterm exam, and grade on final.
        name = input("Enter student's name: ")
        midterm = float(input("Enter student's grade on midterm exam: "))
        final = float(input("Enter student's grade on final exam: "))
        # Create an instance of an LGstudent object.
        st = lgStudent.LGstudent(name, midterm, final)
        listOfStudents.append(st)   # Insert object into list.
        carryOn = input("Do you want to continue (Y/N)? ")
        carryOn = carryOn.upper()
    print("\nNAME\tGRADE")
    # Display students, names and semester letter grades.
    for pupil in listOfStudents:    
        print(pupil)

main()
