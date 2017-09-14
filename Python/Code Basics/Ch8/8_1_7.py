from tkinter import *

def sortItems(event):
    L.sort()
    conOFlstColors.set(tuple(L))

window = Tk()
window.title("Colors")
L = ["red", "yellow", "light blue", "orange"]
conOFlstColors = StringVar()
lstColors = Listbox(window, width=10, height=5, listvariable=conOFlstColors)
lstColors.grid(padx=100, pady=15)    
conOFlstColors.set(tuple(L))
lstColors.bind("<Button-3>", sortItems)
window.mainloop()




