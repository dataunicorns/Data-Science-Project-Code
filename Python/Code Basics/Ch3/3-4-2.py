## Display population from 2014 to 2018.
pop = 300000
print("{0:10} {1}".format("Year", "Population"))
for year in range(2014, 2019):
    print("{0:<10d} {1:,d}".format(year, round(pop)))
    pop += 0.03 * pop     # Increase pop by 3 percent.                 

