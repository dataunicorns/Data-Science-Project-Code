import random

def main():
    for i in range(3):
        outcome = spinWheel()
        print(outcome, end="  ")
                  
def spinWheel():
    n = random.randint(1, 20)
    if n > 15:
        return "Cherries"
    elif n > 10:
        return "Orange"
    elif n > 5:
        return "Plum"
    elif n > 2:
        return "Melon"
    elif n > 1:
        return "Bell"
    else:
        return "Bar"
    
main()


    


 



    

    









    
   






