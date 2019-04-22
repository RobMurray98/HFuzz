# HFuzz

Implementation for third year project, Differential Privacy in Haskell. Robert Murray, 1608035.

HFuzz implements an EDSL that creates a deep embedding for expressions which have their function sensitivity inferred at the type-level. This is then used to apply the Laplace mechanism to provide a differential privacy guarantee. See the project report "Differential Privacy in Haskell" for a full discussion of the project.

## Usage

Example: sum of a list using `xsum`, guaranteeing $0.1$-differential privacy.

```haskell
>> run1 xsum [1,2,3,4,5] 0.1
16.311819406922847
```

You can use various exported functions to construct your own programs which you can then use the evaluation functions on to evaluate in a differentially-private manner.

```haskell
TODO
```

## Extension

Our EDSL implements a useful set of functions across the types that we allow. If this is not sufficient to express a desired function, it is possible to extend the functionality of our EDSL by following the same pattern used to implement existing functionality. We can fork the existing library, add a constructor for some function that we want to perform (with appropriate context handling) and add an evaluation case for this. We will look at the toy example of creating a list from a value. Obviously we could define this as the cons of a value to the empty list, but we want to demonstrate how to extend the EDSL's functionality. First we add the corresponding constructor in the `Expr` data type:

```haskell
data Expr (xs :: Context) (ys :: Context) (t :: Ty) where
    ...
    XListOfElem :: Expr xs ys (TPrim pt) ->
                   Expr xs ys (TPrim (PrimList 1 pt))
```

We leave the existing contexts unchanged in the resultant `Expr`, but change the type to be a list of the type of the value given, with length `1`. We must next add a corresponding constructor in `UExpr`:

```haskell
data UExpr where
...
    UXListOfElem :: UExpr -> UExpr
```

Next we add a case to the `untype` function:

```haskell
untype :: Expr xs ys t -> UExpr
...
untype (XListOfElem e) = UXListOfElem (untype e)
```

Finally we add a case for the interpretation of `UXListOfElem` in the `evaluate'` function:

```haskell
evaluate' :: RuntimeContext -> UExpr -> Value
...
evaluate' c (UXListOfElem e) = VList ((evaluate' c e) : [])
```

We can follow these four steps to extend the functionality of our EDSL.