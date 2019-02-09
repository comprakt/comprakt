initSidebarItems({"enum":[["AST",""],["ASTDiscriminants","Auto-generated discriminant enum variants"],["Attribute","Attributes can be given to a program with `ignore #!attribute_name` at the start of the file."],["BasicType","A `BasicType<'t>` is either one of * `Int`: a 32-bit integer * `Boolean`: a boolean * `Void`: a void type * `Custom`: a custom defined type"],["BasicTypeDiscriminants","Auto-generated discriminant enum variants"],["BinaryOp","Binary operations like comparisons (`==`, `!=`, `<=`, ...), logical operations (`||`, `&&`) or algebraic operation (`+`, `-`, `*`, `/`, `%`)."],["ClassMemberKind","A class member is either one of * `Field(type)`: a declaration of a field of a class * `Method(type, params, body)`: a method of a class * `MainMethod(params, body)`: a main method, which is a special method that is only allowed once in a MiniJava Program. `params` is guaranteed to only contain the `String[] IDENT` parameter."],["ClassMemberKindDiscriminants","Auto-generated discriminant enum variants"],["Expr","An expression is either one of * `Assignment`: an assignment expression * `Binary`: one of the binary operations defined in `BinaryOp` * `Unary`: one of the unary operations defined in `UnaryOp` * `MethodInvocation`: a method invocation on a primary expression: `foo.method()` * `FieldAccess`: a field access on a primary expression: `foo.bar` * `ArrayAccess`: an array access on a primary expression: `foo[42]` The primary expression from the original grammar are also part of this, since the distinction is only required for correct postfix-op parsing. These are: * `Null`: the `null` keyword * `Boolean`: a boolean literal * `Int`: an integer literal * `Var`: use of a variable * `MethodInvocation`: a method invocation * `This`: the `this` keyword * `NewObject`: generating a new object, e.g. `new Foo()` * `NewArray`: generating a new array, e.g. `new int[]`"],["ExprDiscriminants","Auto-generated discriminant enum variants"],["Stmt","A statement can have one of the kinds: * `Block`: A block defined in `Block` * `Empty`: An empty statement: `;` * `If`: a if expression consisting of the condition, its body and optionally an else statement * `Expression`: an expression defined in `Expr` * `While`: a while loop consisting of the condition and its body * `Return`: a return which can optionally return an expression * `LocalVariableDeclaration`: a declaration and optional initialization of a local variable"],["StmtDiscriminants","Auto-generated discriminant enum variants"],["UnaryOp","One of the unary operations `!` and `-`"]],"struct":[["Block","A `Block` in the AST is basically just a vector of statements."],["ClassDeclaration","This AST node stores the Class declaration, which consists of a name and the members of the class."],["ClassMember","This AST node describes a class member. Variants of class members are defined in `ClassMemberKind`. Every class member has a name."],["Parameter","This AST node represents a method parameter. A parameter consists of a `Type<'t>` and a name."],["Program","This is the top-level AST node. It stores all class declarations of the MiniJava program."],["Type","A `Type<'t>` is basically a `BasicType<'t>`. Optional it can be an (n-dimensional) array type."]],"type":[["ArgumentList",""],["ParameterList",""]]});