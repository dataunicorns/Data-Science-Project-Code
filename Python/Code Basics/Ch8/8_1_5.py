from tkinter import *

window = Tk()
window.title("Colors")
L = ["red", "yellow", "light blue", "orange"]
conOFlstColors = StringVar()   # contents of the list box 
lstColors = Listbox(window, width=10, height=5,
                    listvariable=conOFlstColors)
lstColors.grid(padx=100, pady=15)
conOFlstColors.set(tuple(L))
window.mainloop()

 
