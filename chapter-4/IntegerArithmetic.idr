
data Expr = Val Int
          | Add Expr Expr
          | Subs Expr Expr
          | Mult Expr Expr
%name Expr expr, expr1, expr2

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add expr expr1) = evaluate expr + evaluate expr1
evaluate (Subs expr expr1) = evaluate expr - evaluate expr1
evaluate (Mult expr expr1) = evaluate expr * evaluate expr1
