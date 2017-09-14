from tkinter import *

def changeBackgroundColor(event):
    lstColors["bg"]=lstColors.get(lstColors.curselection())
    
window = Tk()
window.title("Colors")
L = ["red", "yellow", "light blue", "orange"]
conOFlstColors = StringVar()
lstColors = Listbox(window, width=10, height=5,
                    listvariable=conOFlstColors)
lstColors.grid(padx=100, pady=15)    
conOFlstColors.set(tuple(L))
lstColors.bind("<<ListboxSelect>>", changeBackgroundColor)
window.mainloop()







