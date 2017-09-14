from tkinter import *

class StateBirds:
    def __init__(self):
        window = Tk()
        window.title("State Birds")
        textForButton = "Display the Different State Birds"
        btnDisplay = Button(window, text=textForButton,
                            command=self.displayBirds)
        btnDisplay.grid(row=0, column=0, columnspan=3, pady=5)
        yscroll = Scrollbar(window, orient=VERTICAL)
        yscroll.grid(row=1, column=1, rowspan=10, pady=5, sticky=NS)
        self._conOFlstBirds = StringVar()
        self._lstBirds = Listbox(window, width=20, height=8, 
                  listvariable=self._conOFlstBirds, yscrollcommand=yscroll.set)
        self._lstBirds.grid(row=1, column=0, padx=(5,0), pady=(0,5),
                            rowspan=10)
        yscroll["command"] = self._lstBirds.yview
        textForLabel = "Number of\ndifferent\nstate birds:"
        Label(window, text=textForLabel).grid(row=1, column=2, padx=10, pady=5)
        self._conOFentNumBirds = StringVar()
        entNumBirds = Entry(window, width=2, state="readonly",
                            textvariable=self._conOFentNumBirds)
        entNumBirds.grid(row=2, column=2)
        window.mainloop()
        
    def displayBirds(self):
        infile = open("StateBirds.txt", 'r')
        birdSet = {line.split(',')[1].rstrip() for line in infile}
        infile.close()
        self._conOFlstBirds.set(tuple(sorted(birdSet)))
        numBirds = len(birdSet)
        self._conOFentNumBirds.set(numBirds)
        


