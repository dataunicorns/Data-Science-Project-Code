class RegularPolygon:
   def __init__(self, side=1):
       self._side = side

class Square(RegularPolygon):
   def area(self, side):
       return side * side

class EquilateralTriangle(RegularPolygon):
   def area(self, side):
       return side * side * 0.433
