def main():
    # Use a set to remove the duplicates from a list.
    words = ["nudge", "nudge", "wink", "wink"]
    terms = set(words)
    print(terms)
    words = list(terms)
    print(words)
    # Demonstrate the effect of the add method.
    terms.add("nudge")  # Had no effect since 'nudge' was already in the set.
    terms.add("maybe")
    print(terms)
    # Demonstrate the effect of the discard method.
    terms.discard("nudge")
    print(terms)
    # Convert the set to a tuple.
    words = tuple(terms)
    print(words)
    
main()


