def power(r, n):
   ## iterative definition of power function
   value = 1
   for i in range(1, n + 1):
       value = r * value
   return value    

print(power(2,3))    
        
    

      

