import random

def main():
    ## Select a card at random.
    c = Card()  # Create an instance of a Card object and call _ _init_ _ method.
    c.selectAtRandom()   # Invokes the selectAtRandom method on the object c.
    print(c)    # Calls the _ _str_ _ method that displays the returned value

class Card:
    def __init__(self, rank="", suit=""):
        self._rank = rank
        self._suit = suit

    def selectAtRandom(self):
        ## Randomly select a rank and a suit.
        ranks = ['2', '3', '4', '5', '6', '7', '8', '9',
                 "10", "jack", "queen", "king", "ace"]
        self._rank = random.choice(ranks)
        self._suit = random.choice(["spades", "hearts", "clubs", "diamonds"])

    def __str__(self):
        return (self._rank + " of " + self._suit)

main()

