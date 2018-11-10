# The Grammar as currently implemented (with attributes)
```
Program                             ClassDeclaration*                                                      $ Self.out = { classes: ClassDeclaration.all_outs }
ClassDeclaration                    class IDENT { ClassMember* }                                           $ Self.out = { name: IDENT.lexval, members: ClassMember.all_outs }
ClassMember                         Field                                                                  $ Self.out = { kind: Field.out, name: Field.IDENT.lexval }
                                    | Method                                                               $ Self.out = { kind: Method.out, name: Method.IDENT.lexval }
                                    | MainMethod                                                           $ Self.out = { kind: MainMethod.out, name: MainMethod.IDENT.lexval }

Field                               public Type IDENT ;                                                    $ Self.out = Field(Type)
Method                              public Type IDENT \( ParameterList? \) (throws IDENT)? Block           $ Self.out = Method(Type, ParameterList.out, Block)
MainMethod                          public static Type IDENT \( String[] IDENT \) (throws IDENT)? Block    $ Self.out = MainMethod(IDENT.lexval, Block)

ParameterList                       Parameter (, Parameter)*                                               $ Self.out = Parameter.all_outs
Parameter                           Type IDENT                                                             $ Self.out = { ty: Type.out, name: IDENT.lexval }
Type                                BasicType ([ ])*                                                       $ Self.out = { basic: BasicType.out, array_depth = "[ ]".len()
BasicType                           int                                                                    $ Self.out = Int
                                    | boolean                                                              $ Self.out = Boolean
                                    | void                                                                 $ Self.out = Void
                                    | IDENT                                                                $ Self.out = Custom(IDENT.lexval)

Statement                           Block                                                                  $ Self.out = Block.out
                                    | EmptyStatement                                                       $ Self.out = EmptyStatement.out
                                    | IfStatement                                                          $ Self.out = IfStatement.out
                                    | ExpressionStatement                                                  $ Self.out = ExpressionStatement.out
                                    | WhileStatement                                                       $ Self.out = WhileStatement.out
                                    | ReturnStatement                                                      $ Self.out = ReturnStatement.out

Block                               { BlockStatement* }                                                    $ Self.out = { statements: BlockStatement.all_outs }

BlockStatement                      Statement                                                              $ Self.out = Statement.out
                                    | LocalVariableDeclarationStatement                                    $ Self.out = LocalVariableDeclarationStatement.out

LocalVariableDeclarationStatement   Type IDENT (= Expression)? ;                                           $ Self.out = LocalVariableDeclaration(Type.out, IDENT.lexval, Some(Expression.out) | None)
EmptyStatement                      ;                                                                      $ Self.out = Empty
WhileStatement                      while \( Expression \) Statement                                       $ Self.out = While(Expression.out, Statement.out)
IfStatement                         if \( Expression \) Statement (else Statement)?                        $ Self.out = If(Expression.out, Statement_1, Some(Statement_2) | None)

ExpressionStatement                 Expression ;                                                           $ Self.out = Expression(Expression.out)
ReturnStatement                     return Expression? ;                                                   $ Self.out = Return(Some(Expression.out) | None)

Expression                          LogicalOrExpression (= LogicalOrExpression)*                           $ Self.out = Assignment(LogicalOrExpression_1, LogicalOrExpression_2.all_outs)

// BEGIN precedence climbing
LogicalOrExpression                 LogicalAndExpression (\|\| LogicalAndExpression)*                      $ Self.out = Binary(<binop>, Lhs, Rhs)
LogicalAndExpression                EqualityExpression (&& EqualityExpression)*                            $ <analog>
EqualityExpression                  RelationalExpression ( (== | !=) RelationalExpression)*                $ <analog>
RelationalExpression                AddititveExpression ( (< | <= | > | >=) AdditiveExpression )*          $ <analog>
AdditiveExpression                  MultiplicativeExpression ( (+|-) MultiplicativeExpression )*           $ <analog>
MultiplicativeExpression            UnaryExpression ( (\*|/|%) UnaryExpression)*                           $ <analog>
// END precedence climbing

UnaryExpression                     (! | -)* PostfixExpression                                             $ Self.out = Unary(UnaryOp::from("!|"-"), PostfixExpression.out)
PostfixExpression                   PrimaryExpression (PostfixOp)*                                         $ Self.out = Postfix(PrimaryExpression.out, PostfixOp.all_outs)
PrimaryExpression                   null                                                                   $ Self.out = Null
                                    | false                                                                $ Self.out = Boolean(false)
                                    | true                                                                 $ Self.out = Boolean(true)
                                    | INTEGER_LITERAL                                                      $ Self.out = Int(INTEGER_LITERAL.lexval)
                                    | IDENT                                                                $ Self.out = Var(IDENT.lexval)
                                    | IDENT \( ArgumentList \)                                             $ Self.out = MethodInvocation(IDENT.lexval, )
                                    | this                                                                 $ Self.out = This
                                    | \( Expression \)                                                     $ Self.out = Expression.out
                                    | NewObjectExpression                                                  $ Self.out = NewObjectExpression.out
                                    | NewArrayExpression                                                   $ Self.out = NewArrayExpression.out

PostfixOp                           MethodInvocationOrFieldAccess                                          $ Self.out = MethodInvocationOrFieldAccess.out
                                    | ArrayAccess                                                          $ Self.out = ArrayAccess.out

MethodInvocationOrFieldAcces        . IDENT ParametrizedArgList?                                           $ Self.out = if ParametrizedArgList.is_some() {MethodInvocation(IDENT.lexval, ParametrizedArgList.all_outs)} else {FieldAccess(IDENT.lexval)}
ParametrizedArgList                 \( ArgumentList \)                                                     $ Self.out = ArgumentList.out
ArrayAccess                         [ Expression ]                                                         $ Self.out = ArrayAccess(Expression.out)
ArgumentList                        (Expression (, Expression)*)?                                          $ Self.out = Expression.all_outs

NewObjectExpression                 new IDENT \( \)                                                        $ Self.out = NewObject(IDENT.lexval)
NewArrayExpression                  new BasicType [ Expression ] ([ ])*                                    $ Self.out = NEWArray(BasicType.out, Expression.out, "[]".len())
```
