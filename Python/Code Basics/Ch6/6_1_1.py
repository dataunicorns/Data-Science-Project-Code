def main():
    ## Display the reciprocal of a number in a file.
    try:
        fileName = input("Enter the name of a file: ")
        infile = open(fileName, 'r')
        num = float(infile.readline())
        print(1 / num)
    except FileNotFoundError as exc1:
        print(exc1)
    except TypeError as exc2:
        print(exc2)

main()        
 


