# The Grammar as currently implemented (with attributes)
```
Program                             ClassDeclaration*                                                      $ Self.out = ast::Program{ classes: ClassDeclaration.all_outs }
ClassDeclaration                    class IDENT { ClassMember* }                                           $ Self.out = ast::Class{ name: IDENT.lexval, members: ClassMember.all_outs }
ClassMember                         Field                                                                  $ Self.out = ast::ClassMember{ kind: Field.out, name: Field.IDENT.lexval }
                                    | Method                                                               $ Self.out = ast::ClassMember{ kind: Method.out, name: Method.IDENT.lexval }
                                    | MainMethod                                                           $ Self.out = ast::ClassMember{ kind: MainMethod.out, name: MainMethod.IDENT.lexval }

Field                               public Type IDENT ;                                                    $ Self.out = ast::ClassMemberKind::Field(Type)
Method                              public Type IDENT \( ParameterList? \) (throws IDENT)? Block           $ Self.out = ast::ClassMemberKind::Method(Type, ParameterList.out, Block)
MainMethod                          public static Type IDENT \( String[] IDENT \) (throws IDENT)? Block    $ Self.out = ast::ClassMemberKind::MainMethod(IDENT.lexval, Block)

ParameterList                       Parameter (, Parameter)*                                               $ Self.out = Parameter.all_outs
Parameter                           Type IDENT                                                             $ Self.out = ast::Parameter{ ty: Type.out, name: IDENT.lexval }
Type                                BasicType ([ ])*                                                       $ Self.out = ast::Type{ basic: BasicType.out, array_depth = "[ ]".len()
BasicType                           int                                                                    $ Self.out = ast::BasicType::Int
                                    | boolean                                                              $ Self.out = ast::BasicType::Boolean
                                    | void                                                                 $ Self.out = ast::BasicType::Void
                                    | IDENT                                                                $ Self.out = ast::BasicType::Custom(IDENT.lexval)

Statement                           Block                                                                  $ Self.out = ast::Stmt::Block(Block.out)
                                    | EmptyStatement                                                       $ Self.out = EmptyStatement.out
                                    | IfStatement                                                          $ Self.out = IfStatement.out
                                    | ExpressionStatement                                                  $ Self.out = ExpressionStatement.out
                                    | WhileStatement                                                       $ Self.out = WhileStatement.out
                                    | ReturnStatement                                                      $ Self.out = ReturnStatement.out

Block                               { BlockStatement* }                                                    $ Self.out = ast::Block{ statements: BlockStatement.all_outs }

BlockStatement                      Statement                                                              $ Self.out = Statement.out
                                    | LocalVariableDeclarationStatement                                    $ Self.out = LocalVariableDeclarationStatement.out

LocalVariableDeclarationStatement   Type IDENT (= Expression)? ;                                           $ Self.out = ast::Stmt::LocalVariableDeclaration(Type.out, IDENT.lexval, Some(Expression.out) | None)
EmptyStatement                      ;                                                                      $ Self.out = ast::Stmt::Empty
WhileStatement                      while \( Expression \) Statement                                       $ Self.out = ast::Stmt::While(Expression.out, Statement.out)
IfStatement                         if \( Expression \) Statement (else Statement)?                        $ Self.out = ast::Stmt::If(Expression.out, Statement_1, Some(Statement_2) | None)

ExpressionStatement                 Expression ;                                                           $ Self.out = ast::Stmt::Expression(Expression.out)
ReturnStatement                     return Expression? ;                                                   $ Self.out = ast::Stmt::Return(Some(Expression.out) | None)

// BEGIN precedence climbing
Expression                          LogicalOrExpression (= Expression)?                                    $ Self.out = ast::Expr::Binary(<binop>, Lhs, Rhs)
LogicalOrExpression                 (LogicalAndExpression \|\|)? LogicalAndExpression                      $ <analog>
LogicalAndExpression                (EqualityExpression &&)? EqualityExpression                            $ <analog>
EqualityExpression                  (RelationalExpression (== | !=))? RelationalExpression                 $ <analog>
RelationalExpression                (AddititveExpression (< | <= | > | >=))? AdditiveExpression            $ <analog>
AdditiveExpression                  (MultiplicativeExpression (+|-))? MultiplicativeExpression             $ <analog>
MultiplicativeExpression            (UnaryExpression (\*|/|%))? UnaryExpression                            $ <analog>
// END precedence climbing

UnaryExpression                     (! | -)* PostfixExpression                                             $ Self.out = ast::Expr::Unary(UnaryOp::from("!|"-"), PostfixExpression.out)
PostfixExpression                   PrimaryExpression (PostfixOp)*                                         $ Self.out = ast::Expr::Postfix(PrimaryExpression.out, PostfixOp.all_outs)
PrimaryExpression                   null                                                                   $ Self.out = ast::Expr::Null
                                    | false                                                                $ Self.out = ast::Expr::Boolean(false)
                                    | true                                                                 $ Self.out = ast::Expr::Boolean(true)
                                    | INTEGER_LITERAL                                                      $ Self.out = ast::Expr::Int(INTEGER_LITERAL.lexval)
                                    | IDENT                                                                $ Self.out = ast::Expr::Var(IDENT.lexval)
                                    | IDENT \( ArgumentList \)                                             $ Self.out = ast::Expr::MethodInvocation(IDENT.lexval, ArgumentList.out)
                                    | this                                                                 $ Self.out = ast::Expr::This
                                    | \( Expression \)                                                     $ Self.out = Expression.out
                                    | NewObjectExpression                                                  $ Self.out = NewObjectExpression.out
                                    | NewArrayExpression                                                   $ Self.out = NewArrayExpression.out

PostfixOp                           MethodInvocationOrFieldAccess                                          $ Self.out = MethodInvocationOrFieldAccess.out
                                    | ArrayAccess                                                          $ Self.out = ArrayAccess.out

MethodInvocationOrFieldAcces        . IDENT ParametrizedArgList?                                           $ Self.out = if ParametrizedArgList.is_some() {ast::PostfixOp::MethodInvocation(IDENT.lexval, ParametrizedArgList.all_outs)} else {ast::PostfixOp::FieldAccess(IDENT.lexval)}
ParametrizedArgList                 \( ArgumentList \)                                                     $ Self.out = ArgumentList.out
ArrayAccess                         [ Expression ]                                                         $ Self.out = ast::PostfixOp::ArrayAccess(Expression.out)
ArgumentList                        (Expression (, Expression)*)?                                          $ Self.out = Expression.all_outs

NewObjectExpression                 new IDENT \( \)                                                        $ Self.out = ast::Expr::NewObject(IDENT.lexval)
NewArrayExpression                  new BasicType [ Expression ] ([ ])*                                    $ Self.out = ast::Expr::NewArray(BasicType.out, Expression.out, "[]".len())
```
