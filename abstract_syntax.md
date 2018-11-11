# Abstarkte Algebra

```
Program :: ClassDeclaration *
ClassDeclaration :: _Symbol_ (ClassMember *)
ClassMember :: _Symbol_ ClassMemberKind
ClassMemberKind = Field | Method | MainMethod
Field :: Type
Method :: Type (Parameter *) Block
MainMethod :: _Symbol_ Block
Type :: BasicType `u64`
BasicType = _Int_ | _Bool_ | _Void_ | Custom
Custom :: _Symbol_
Parameter :: Type _Symbol_
Block :: Stmt *
Stmt = Block | _Empty_ | If | Expr | While | Return | LocalVariableDeclaration
If :: Expr Stmt (Stmt ?)
While :: Expr Stmt
Return :: Expr ?
LocalVariableDeclaration :: Type _Symbol_ (Expr ?)
Expr = Assignment | Binary | Unary | Postfix | _Null_ | Boolean | Int | Var | MethodPrimary | _This_ | NewObject | NewArray
Assignment :: Expr (Expr *)
Binary :: BinaryOp Expr Expr
Unary :: (UnaryOp *) Expr
Postfix :: Expr (PostfixOp *)
Boolean :: `bool`
Int :: _Symbol_
Var :: _Symbol_
MethodPrimary :: _Symbol_ (Expr *)
NewObject :: _Symbol_
NewArray :: BasicType Expr `u64`
BinaryOp = _Equals_ | _NotEquals_ | _LessThan_ | _GreaterThan_ | _LessEquals_ | _GreaterEquals_ | _LogicalOr_ | _LogicalAnd_ | _Add_ | _Sub_ | _Mul_ | _Div_ | _Mod_
UnaryOp = _Not_ | _Neg_
PostfixOp = MethodInvocation | FieldAccess | ArrayAccess
MethodInvocation :: _Symbol_ (Expr *)
FieldAccess :: _Symbol_
ArrayAccess :: Expr
```

##### Program

This is the top-level AST node. It stores all class declarations of the MiniJava program.

##### ClassDeclaration

This AST node stores the Class declaration, which consists of a name and the members of the class.

##### ClassMember

This AST node describes a class member. Variants of class members are defined in ClassMemberKind. Every class member has a name.

##### ClassMemberKind

A class member is either one of
* `Field`: a declaration of a field of a class
* `Method`: a method of a class
* `MainMethod`: a main method, which is a special method that is only
allowed once in a MiniJava Program

##### Field

Holds a `Type`.

##### Method

Holds a `Type`, a `ParameterList` and a `Block`.

##### MainMethod

Holds a `Symbol` (The parameter name) and a `Block`.

##### Type

A `Type` is basically a `BasicType`. Optional it can be an (n-dimensional)
array type.

##### BasicType

A `BasicType` is either one of
* `Int`: a 32-bit integer
* `Boolean`: a boolean
* `Void`: a void type
* `Custom`: a custom defined type

##### Custom

Holds a `Symbol`, which is the name of the type.

##### Parameter

This AST node represents a method parameter. A parameter consists of a
`Type` and a name.

##### Block

A `Block` in the AST is basically just a vector of statements.

##### Stmt

A statement can have one of the kinds:
* `Block`: A block defined in `Block`
* `Empty`: An empty statement: `;`
* `If`: a if expression consisting of the condition, its body and
optionally an else statement
* `While`: a while loop consisting of the condition and its body
* `Expression`: an expression defined in `Expr`
* `Return`: a return which can optionally return an expression
* `LocalVariableDeclaration`: a declaration and optional initialization of
a local variable

##### If

Holds an `Expr` (condition), a `Stmt` (body) and a optional `Stmt` (else body)

##### While

Holds an `Expr` (condition) and a `Stmt` (body)

##### Return

Holds an optional `Expr`

##### LocalVariableDeclaration

Holds a `Type`, a `Symbol` and an optional `Expr`

##### Expr

An expression is either one of
* `Assignment`: an assignment expression
* `Binary`: one of the binary operations defined in `BinaryOp`
* `Unary`: one of the unary operations defined in `UnaryOp`
* `Postfix`: a primary expression with arbitrary many postfix operations
defined in `PostfixOp`

The primary expression from the original grammar are also part of this,
since the distinction is only required for correct postfix-op parsing. These
are:
* `Null`: the `null` keyword
* `Boolean`: a boolean literal
* `Int`: an integer literal
* `Var`: use of a variable
* `MethodInvocation`: a method invocation
* `This`: the `this` keyword
* `NewObject`: generating a new object, e.g. `new Foo()`
* `NewArray`: generating a new array, e.g. `new int[]`

##### Assignment

Holds an `Expr` and a list of `Expr`s

##### Binary

Holds a `BinaryOp` and two `Expr`s

##### Unary

Holds an `UnaryOp` and two `Expr`s

##### Postfix

Holds an `Expr` and a list of `PostfixOp`s

##### Boolean

Holds a `bool`

##### Int

Holds a `Symbol`/`String`

##### Var

Holds a `Symbol`

##### MethodPrimary

Holds a `Symbol` and a list of `Expr`s

##### NewObject

Holds a `Symbol`

##### NewArray

Holds a `BasicType`, an `Expr` (Array size) and a `u64` (number of following `[]`)

##### BinaryOp

Binary operations like comparisons (`==`, `!=`, `<=`, ...), logical
operations (`||`, `&&`) or algebraic operation (`+`, `-`, `*`, `/`, `%`).

##### UnaryOp

One of the unary operations `!` and `-`

##### PostfixOp

A postfix operation is either one of
* `MethodInvocation`: a method invocation on a primary expression: `foo.method()`
* `FieldAccess`: a field access on a primary expression: `foo.bar`
* `ArrayAccess`: an array access on a primary expression: `foo[42]`

##### MethodInvocation

Holds a `Symbol` and a list of `Expr`s

##### FieldAccess

Holds a `Symbol`

##### ArrayAccess

Holds an `Expr` (Index)

