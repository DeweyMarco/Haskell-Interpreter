# Haskell-Interpreter

This Haskell code implements a simple interpreter for a programming language with basic arithmetic expressions and variable assignments. The interpreter parses input code, constructs an Abstract Syntax Tree (AST), evaluates expressions, and provides error handling for various scenarios.

## Testing
To run the Hspec tests use the following commands
```
$ stack build
$ stack test
```

QuickCheck tests where not used for this project as they need boolean values to determine correctness 

### Tests
The 'eval' function and the 'eval_Ast . produce_Ast' are tested in exactly the same way to demonstrait that they do the same thing. 

The tests are split in to the following catagories
* Read Numbers - All possible ways a double is represented
* Ignore Comments - Ignoring comments and spaces
* Chaining Operators - Ability to do multiple operations in the right order
* Parentheses - Using parentheses correctly
* Exponents - Order of operations with exponents
* Multiple Statements - Multiple statments in the input string
* Variables - Use of variables
* Negation - Outlier cases line '1--1' being '(1-(-1))' 
* Errors - All error statements

## Eval
The eval function takes a string and parses it to return a list of doubles, if an error occures the function will return a string with the error. 

Inputs : String

Outputs : Either [Double] String

### Examples
Basic eval
```
> eval "1+1"
Left [2.0]
```

Variables
```
> eval "x=3;x+7"
Left [10.0]
```

Multiple Statments
```
> eval "6^2;9/3"
Left [36.0,3.0]
```

Errors
```
> eval "1/0"
Right "Division by Zero"
```

## Produce Ast
The produce_Ast function takes a string and produces an abstract syntax tree, if an error occures the function will return a string with the error. 

Inputs : String

Outputs : Either AST String

### AST
The abstract syntax trees are represented in the following way.

"1+1" would become '(1.0 + 1.0)' which represents 
```
    |---1.0
---"+"
    |---1.0
```

A more complex example would be "a=7-(6^3)" would become '(Var "a" = (7.0 - (6.0 ^ 3.0)))'
```
              |---3.0
         |---"^"
         |    |---6.0
         |
    |---"-"
    |    |---7.0
    |
---"="
    |---"a"
```

### Examples
Basic produce_Ast
```
> produce_Ast "1+1"
Left [(1.0 + 1.0)]
```

Variables
```
> produce_Ast "x=3;x+7"
Left [(Var "x" = 3.0),(Var "x" + 7.0)]
```

Multiple Statments
```
> produce_Ast "6^2;9/3"
Left [(6.0 ^ 2.0),(9.0 / 3.0)]
```

Errors
```
> produce_Ast "1++1"
Right "Parser Error"
```

## Eval Ast
The eval_Ast function takes an abstract syntax tree and parses it to return a list of doubles, if an error occures the function will return a string with the error. The eval_Ast can take a Error string as input if the produce_Ast fucntion fails and is chained to the eval_Ast function, if it takes a string as input it will throw that string as an error. 

Inputs : Either AST String

Outputs : Either [Double] String

### Examples
Basic eval_Ast
```
> eval_Ast (produce_Ast "1+1")
Left [2.0]
```

Variables
```
> eval_Ast (produce_Ast "x=3;x+7")
Left [10.0]
```

Multiple Statments
```
> eval_Ast (produce_Ast "6^2;9/3")
Left [36.0,3.0]
```

Errors
```
> eval_Ast (produce_Ast "y=2;x+4")
Right "Use Before Defined"
```
