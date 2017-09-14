
import turtle

def main():
    t = turtle.Turtle()
    t.hideturtle()
    # Draw the three stripes.
    drawFilledRectangle(t, 0, 0, 150, 25, "light blue", "light blue")
    drawFilledRectangle(t, 0, 25, 150 , 50, "blue", "blue")
    drawFilledRectangle(t, 0, 75, 150, 25, "light blue", "light blue")
    # Draw white dot. Center of flag is (75, 50).  40 = .8 * 50.
    drawDot(t, 75, 50, 40, "white")
    
def drawFilledRectangle(t, x, y, w, h, colorP = "black", colorF = "white"):
    ## Draw a filled rectangle with bottom-left corner (x, y),
    ## width w, height h, pen color colorP, and fill color colorF.
    t.pencolor(colorP)
    t.fillcolor(colorF)
    t.up()
    t.goto(x , y)         # bottom-left corner of rectangle
    t.down()
    t.begin_fill()
    t.goto(x + w, y)      # bottom-right corner of rectangle
    t.goto(x + w, y + h)  # top-right corner of rectangle
    t.goto(x, y + h)      # top-left corner of rectangle
    t.goto(x , y)         # bottom-left corner of rectangle
    t.end_fill()

def drawDot(t, x, y, diameter, colorP):
    ## Draw a dot with center (x, y) and color colorP.
    t.up()
    t.goto(x, y)
    t.pencolor(colorP)
    t.dot(diameter) 

main()    
        




