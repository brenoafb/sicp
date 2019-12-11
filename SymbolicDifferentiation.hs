type Symbol = String

data Exp
  = Variable Symbol
  | Number Integer
  | Sum Exp Exp
  | Product Exp Exp
  | Exponentiation Exp Exp
  deriving (Eq, Show)

deriv :: Exp -> Symbol -> Exp
deriv exp var = simplify result
  where
    result =
      case exp of
        Number x -> Number 0
        Variable s ->
          if s == var
            then (Number 1)
            else (Number 0)
        Sum exp1 exp2 -> Sum (deriv exp1 var) (deriv exp2 var)
        Product exp1 exp2 ->
          Sum (Product (deriv exp1 var) exp2) (Product (deriv exp2 var) exp1)
        Exponentiation base exponent ->
          Product exponent
                  (Product (Exponentiation base (Sum exponent (Number (-1))))
                           (deriv base var))

simplify :: Exp -> Exp
simplify exp =
  case exp of
    Sum (Number m) (Number n)      -> Number (m + n)
    Sum exp1 (Number 0)            -> simplify exp1
    Sum (Number 0) exp2            -> simplify exp2
    Sum exp1 exp2                  -> simplify $ Sum (simplify exp1) (simplify exp2)
    Product (Number m) (Number n)  -> Number (m * n)
    Product (Number 1) exp2        -> simplify exp2
    Product exp1 (Number 1)        -> simplify exp1
    Product (Number 0) _           -> Number 0
    Product _ (Number 0)           -> Number 0
    Product exp1 exp2              -> simplify $ Product (simplify exp1) (simplify exp2)
    Exponentiation _ (Number 0)    -> Number 1
    Exponentiation base (Number 1) -> simplify base
    Exponentiation (Number 0) _    -> Number 0
    Exponentiation (Number 1) _    -> Number 1
    _ -> exp
