# The Grammer as is currently implemented
```
Program                             ClassDeclaration*
ClassDeclaration                    class IDENT { ClassMember* }
ClassMember                         Field
                                    | Method
Field                               public Type IDENT ;
// TODO: MainMethod missing
Method                              public static? Type IDENT \( Parameters? \) (throws IDENT)? Block

Parameters                          Parameter (, Parameter)*
Parameter                           Type IDENT
Type                                BasicType ([ ])*
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

// BEGIN precedence climbing
Expression                          LogicalOrExpression (= LogicalOrExpression)*
LogicalOrExpression                 LogicalAndExpression (\|\| LogicalAndExpression)*
LogicalAndExpression                EqualityExpression (&& EqualityExpression)*
EqualityExpression                  RelationalExpression ( (== | !=) RelationalExpression)*
RelationalExpression                AddititveExpression ( (< | <= | > | >=) AdditiveExpression )*
AdditiveExpression                  MultiplicativeExpression ( (+|-) MultiplicativeExpression )*
MultiplicativeExpression            UnaryExpression ( (\*|/|%) UnaryExpression)*
// END precedence climbing

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

PostfixOp                           MethodInvocationOrFieldAccess
                                    | ArrayAccess
MethodInvocationOrFieldAcces        . IDENT ParametrizedArgList?
ParametrizedArgList                 \( Arguments \)
ArrayAccess                         [ Expression ]
Arguments                           (Expression (, Expression)*)?



// TODO: e.g. new int() should not be allowed
NewObjectExpression                 new BasicType \( \)
NewArrayExpression                  new BasicType [ Expression ] ([ ])*
```
