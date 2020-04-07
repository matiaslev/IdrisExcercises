import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (firstVector :: otherVectors) = let otherVectorsTrans = transposeMat otherVectors in
                                             zipWith (::) firstVector otherVectorsTrans

--

sumVectors : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect m a
sumVectors [] [] = []
sumVectors (x :: xs) (y :: ys) = x + y :: sumVectors xs ys

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = sumVectors x y :: addMatrix xs ys

--

multVecs : Num a => (xs : Vect n a) -> (ys : Vect n a) -> a
multVecs xs ys = sum (zipWith (*) xs ys)

mkRow : Num a => (x : Vect n a) -> (ys_trans : Vect p (Vect n a)) -> Vect p a
mkRow x [] = []
mkRow x (y :: xs) = multVecs x y :: mkRow x xs

multMatrix_helper : Num numType => (xs : Vect n (Vect m numType)) -> (ysTrans : Vect p (Vect m numType)) -> Vect n (Vect p numType)
multMatrix_helper [] ysTrans = []
multMatrix_helper (x :: xs) ysTrans = mkRow x ysTrans :: multMatrix_helper xs ysTrans

multMatrix : Num numType =>
             Vect n (Vect m numType) -> Vect m (Vect p numType) ->
             Vect n (Vect p numType)
multMatrix xs ys = let ysTrans = transposeMat ys in
                   multMatrix_helper xs ysTrans
