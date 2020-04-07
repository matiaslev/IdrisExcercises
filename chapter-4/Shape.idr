
-- Union types

||| Represents shapes
data Shape =  ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double


area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle base height) = base * height
area (Circle radius) = pi * radius * radius

-- Recursive types

{-
* A Primitive shape
* A combination of two other pictures
* A picture rotated through an angle
* A picture translated to a different location
-}

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture


rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

-- Generic data types
-- data Biggest = NoTriangle | Size Double // Representad with maybe

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape@(Triangle x y)) = Just (area shape)
biggestTriangle (Primitive (Rectangle x y)) = Nothing
biggestTriangle (Primitive (Circle x)) = Nothing
biggestTriangle (Combine pic pic1) = case biggestTriangle pic > biggestTriangle pic1 of
                                          False => biggestTriangle pic1
                                          True => biggestTriangle pic
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
