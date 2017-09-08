# calc

Experiments in Haskell: command line calculator.

## Usage

```bash
$ stack build
$ stack exec calc-exe
```

### As library

```haskell
import Term

computed = eval $ Sum (ValueF 10) (Pi)
```


```haskell
import Term
import Text.Parsec

parseCalculation text = fmap toExpr $ parse termsP "my calc" text
execParsed parsed = fmap eval parsed
```

