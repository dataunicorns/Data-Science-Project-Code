## Display the last line of a text file.
infile = open("aFile.txt", 'r')
for line in infile:
    pass
print(line.rstrip())
infile.close()
