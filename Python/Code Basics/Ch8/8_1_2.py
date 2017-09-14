from tkinter import *

def changeColor(event):          
    if entName["fg"] == "blue":  # if Entry widget text is colored blue
        entName["fg"] = "red"    # change color of text to red
    else:
        entName["fg"] = "blue"   # change color of text to blue
        
window = Tk()     
window.title("Entry widget")
entName = Entry(window, fg="blue")
entName.grid(padx=100, pady=15)
entName.bind("<Button-3>", changeColor)  # specify right-click as an event
window.mainloop()




