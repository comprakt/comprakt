# Grammar

* Grammar symbols are generally space-separated.
* Tokens are generally lower-case or special characters.
    * `IDENT` is the most notable exception to this rule.
    * The `(`, `)`, `|` and `*` tokens are escaped with `\`, e.g. `\(`, because they are part of the meta-language as well.
* A meta-language parentheses pair `(X)` groups the symbols `X`
* The meta-language operator `?` with `X?` or `(Y)?` means *zero or one* occurence of symbol `X` or symbols `Y`
* The meta-language operator `*` with `X*` or `(X)*` means *zero or more* occurences of sybmol `X` or sybmoles `Y`


```
Program                             ClassDeclaration*
ClassDeclaration                    class IDENT { ClassMember* }
ClassMember                         Field
                                    | Method
                                    | MainMethod
Field                               public Type IDENT ;
MainMethod                          public static void IDENT \( String [ ] IDENT \) MethodRest Block
Method                              public Type IDENT \( Parameters \) MethodRest Block
MethodRest                          (throws IDENT)?

Parameters                          (Parameter (, Parameter)*)?
Parameter                           Type IDENT
Type                                Type [ ]
                                    | BasicType
BasicType                           int
                                    | boolean
                                    | void
                                    | IDENT


Statement                           Block
                                    | EmptyStatement
                                    | IfStatement
                                    | ExpressionStatement
                                    | WhileStatement
                                    | ReturnStatement
Block                               { BlockStatement* }
BlockStatement                      Statement
                                    | LocalVariableDeclarationStatement
LocalVariableDeclarationStatement   Type IDENT (= Expression)? ;
EmptyStatement                      ;
WhileStatement                      while \( Expression \) Statement
IfStatement                         if \( Expression \) Statement IfStatement'
IfStatement'                        eps
                                    | else Statement
ExpressionStatement                 Expression ;
ReturnStatement                     return Expression? ;

Expression                          LogicalOrExpression (= LogicalOrExpression)*
LogicalOrExpression                 LogicalAndExpression (\|\| LogicalAndExpression)*
LogicalAndExpression                EqualityExpression (&& EqualityExpression)*
EqualityExpression                  RelationalExpression ( (== | !=) RelationalExpression)*
RelationalExpression                AddititveExpression ( (< | <= | > | >=) AdditiveExpression )*
AdditiveExpression                  MultiplicativeExpression ( (+|-) MultiplicativeExpression )*
MultiplicativeExpression            UnaryExpression ( (\*|/|%) UnaryExpression)*
// Unary has highest precedence (i.e. there is also no higher precedence)
// => precendence clibming will do parse_unary() instead of parse_primary() (like in slides)
UnaryExpression                     PostfixExpression | (! | -) UnaryExpression
PostfixExpression                   PrimaryExpression (PostfixOp)*
PrimaryExpression                   null
                                    | false
                                    | true
                                    | INTEGER_LITERAL
                                    | IDENT
                                    | IDENT \( Arguments \)
                                    | this
                                    | \( Expression \)
                                    | NewObjectExpression
                                    | NewArrayExpression

PostfixOp                           MethodInvocation
                                    | FieldAccess
                                    | ArrayAccess
MethodInvocation                    . IDENT \( Arguments \)
FieldAccess                         . IDENT
ArrayAccess                         [ Expression ]
Arguments                           (Expression (, Expression)*)?



NewObjectExpression                 new IDENT \( \)
NewArrayExpression                  new BasicType [ Expression ] ([ ])*
```

## SLL(k)-ness of 'Expressions Sub-Grammar Variant 2'

To parsing expressions efficiently, we plan to use precedence-climbing.
Left-recursion elimination variant 2 as described in the slides translates easily into
precendence-climbing.
Thus, `Expression` rules have been converted to variant 2 form:

```
Expression    LogicalOrExpression (= LogicalOrExpression)*
```

Above production is clearly not SLL(k) because `LogicalOrExpression` can grow
arbitrarily large, because `LogicalOrExpression =*=> (Expression)` is still
left-recursive.  The following transformation fixes that problem:

```
Expression   LogicalOrExpression Expression'
Expression'  eps
             | = LogicalOrExpression
```

**Why does it fix above problem?** `Expression` no longer has alternatives.
Thus, after parsing the `LogicalOrExpression` in line 1 we can check whether
the next token is in
`First(Follow(Expression')) = First(Follow(Expression)) = {';', ')', ']'}`
or whether it is in
`First(= LogicalOrExpression Follow(Expression')) = { '=' }`.

There are no ambiguities since those two sets are disjoint (even for k = 1).
Busywork shows that this is also the case for all other *Variant 2* expression
productions in the grammar below.

## SLL(k)-ness of 'Zero or One ?'

We use `X?` or `(X Y)?` in multiple places to express that `X` or `X Y` may not
occur or occur exactly once.  We have manually verified that the set of first
characters of `X` is disjoint from the set of tokens that may follow such
occurences.

## Above Grammar is SLL(4)

We do *not* provide a proof that the grammar is SLL(4).
However, a survey of the roots of the most important rules makes this clear.
(Grouping is indicated by empty newlines in above notation):

* `BlockStatement` is SLL(2), distinguish `ExpressionStatement` from `LocalVariableDeclarationStatement` through `{IDENT,bool,int,void} IDENT`
* `Arguments` SLL(1) because `Follow(Arguments) = {')'}` and `First(Arguments) \ eps` is disjoint from `Follow(Arguments)`
* `PostfixOp` SLL(3) because `. IDENT` is common prefix of `MethodInvocation` and `FieldAccess`
* `PrimaryExpression` is SLL(3), mostly because of `NewObjectExpression` and `NewArrayExpression`
    * Since `BasicType` can be an `IDENT`, they both can start with `new IDENT`
    * => need to distinguish chec `NewObjectExpression` and `NewArrayExpression` by `(` and `[`
* `Expression` is SLL(3) because `PrimaryExpression` can come first (see above)
* `ClassDeclaration` SLL(1) obvious
* `ClassMember` is SLL(4) because `Field` and `Method` have a 3 token shared prefix
    * Could be trivially reduced, by common prefix extraction, but why bother.
* `Statement` is SLL(1) because `First(xFollow(x))` are disjoint (thanks keywords!)
