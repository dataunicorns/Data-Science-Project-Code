## A quiz.
# Obtain answer to question. 
answer = eval(input("How many gallons does a ten-gallon hat hold? "))
# Evaluate answer.
if (0.5 <= answer <= 1):
    print("Good, ", end="")
else:
    print("No, ", end="")
print("it holds about 3/4 of a gallon.")

