## Display the names of the 52 cards in a deck of cards.
ranks = ['2', '3', '4', '5', '6', '7', '8', '9',
         "10", "jack", "queen", "king", "ace"]
suits = ["spades", "hearts", "clubs", "diamonds"]  
deckOfCards = []  # List to hold the names of the 52 cards in a deck.
# Use nested loops to fill the deckOfCards list.
for rank in ranks:
    for suit in suits:
        deckOfCards.append(rank + " of " + suit)
# Display the 52 cards.        
for card in deckOfCards:
    print(card)
        





