from tkinter import *

def convertToUpperCase(event):
    conOFentName.set(conOFentName.get().upper())
        
window = Tk()     
window.title("Entry widget")
conOFentName = StringVar()   # contents of the Entry widget
entName = Entry(window, textvariable=conOFentName )
entName.grid(padx=100, pady=15)
entName.bind("<Button-3>", convertToUpperCase)
window.mainloop()




