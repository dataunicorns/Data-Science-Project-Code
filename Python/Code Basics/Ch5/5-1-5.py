def main():
    ## Add next three presidents to the file containing first three presidents.
    outfile = open("FirstPresidents.txt", 'a')
    list1 = ["James Madison\n", "James Monroe\n"]
    outfile.writelines(list1)
    outfile.write("John Q. Adams\n")
    outfile.close()
    
main()    


