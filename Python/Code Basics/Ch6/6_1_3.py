def main():
    ## Calculate the average and total of the numbers in a file.
    total = 0
    counter = 0
    foundFlag = True
    try:
        infile = open("Numbers.txt", 'r')
    except FileNotFoundError:
        print("File not found.")
        foundFlag = False
    if foundFlag:
        try:
            for line in infile:
                counter += 1
                total += float(line)
            print("average:", total / counter)
        except ValueError:
            print("Line", counter, "could not be converted to a float.")
            if counter > 1:
                print("Average so far:", total / (counter - 1))
                print("Total so far:", total)
            else:
                print("No average can be calculated.")
        except ZeroDivisionError:
            print("File was empty.")
        else:
            print("Total:", total)
        finally:
            infile.close()
          
main()
