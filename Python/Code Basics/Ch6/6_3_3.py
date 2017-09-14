import turtle

def main():
    t = turtle.Turtle()
    t.hideturtle()
    lengthOfSide = 200
    drawFivePointStar(t, 0, 0, lengthOfSide)
         
def drawFivePointStar(t, x, y, lengthOfSide):
    # Drawing begins at (x, y) and moves in a north-east direction.
    t.up()
    t.goto(x, y)
    t.left(36)
    t.down()
    for i in range(5):
        t.forward(lengthOfSide)
        t.left(144)
    
main()
