## Interpret weather beacon.
# Get color and mode.
color = input("Enter the color (BLUE or RED): ")
mode = input("Enter the mode (STEADY or FLASHING): ")
color = color.upper()
mode = mode.upper()             
result = ""
# Analyze responses and display weather forecast.
if color == "BLUE":
    if mode == "STEADY":
        result = "Clear View."
    else:  # mode is FLASHING
        result = "Clouds Due."
else:      # color is RED
    if mode == "STEADY":
        result = "Rain Ahead."
    else:  # mode is FLASHING
        result = "Snow Ahead."
print("The weather forecast is", result)

