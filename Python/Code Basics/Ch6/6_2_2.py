import random
import pickle

infile = open("DeckOfCardsList.dat", 'rb')
deckOfCards = pickle.load(infile)
infile.close()
pokerHand = random.sample(deckOfCards, 5)
print(pokerHand)










    


 



    

    









    
   






