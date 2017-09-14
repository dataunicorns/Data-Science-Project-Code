from tkinter import *

def displayBirds():
    infile = open("StateBirds.txt", 'r')
    birdSet = {line.split(',')[1].rstrip() for line in infile}
    infile.close()
    conOFlstBirds.set(tuple(sorted(birdSet)))   # sorted(birdSet) is a list
    numBirds = len(birdSet)
    conOFentNumBirds.set(numBirds)
    
window = Tk()
window.title("State Birds")
textForButton = "Display the Different State Birds"
btnDisplay = Button(window, text=textForButton, command=displayBirds)
btnDisplay.grid(row=0, column=0, columnspan=3, pady=5)
yscroll = Scrollbar(window, orient=VERTICAL)
yscroll.grid(row=1, column=1, rowspan=10, pady=(0,5), sticky=NS)
conOFlstBirds = StringVar()
lstBirds = Listbox(window, width=20, height=8, listvariable=conOFlstBirds,
                   yscrollcommand=yscroll.set)
lstBirds.grid(row=1, column=0, padx=(5,0), pady=(0,5), rowspan=10)
yscroll["command"] = lstBirds.yview
textForLabel = "Number of\ndifferent\nstate birds:"
Label(window, text=textForLabel).grid(row=1, column=2, padx=10, pady=5)
conOFentNumBirds = StringVar()
entNumBirds = Entry(window, width=2, state="readonly",
                    textvariable=conOFentNumBirds)
entNumBirds.grid(row=2, column=2)
window.mainloop()



