def main():
    ## A quiz
    q = "What is the capital of California? "
    a = "Sacramento"
    askQuestion(q, a)

def askQuestion(question, answer, numberOfTries=3):
    numTries = 0
    while numTries < numberOfTries:
        numTries += 1
        ans = input(question)
        if ans == answer:
            print("Correct!")
            break
    if ans != answer:
        print("You have used up your allotment of guesses.")
        print("The correct answer is", answer + '.')
          
main()


  
