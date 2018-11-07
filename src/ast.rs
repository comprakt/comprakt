use crate::{lexer::Span, strtab::Symbol};

/// This is the top-level AST node. It stores all class declerations of the
/// MiniJava program.
#[derive(Debug, PartialEq, Eq)]
pub struct Program<'t> {
    pub span: Span<'t>,
    pub classes: Vec<ClassDeclaration<'t>>,
}

/// This AST node stores the Class decleration, which consists of a name and
/// the members of the class.
#[derive(Debug, PartialEq, Eq)]
pub struct ClassDeclaration<'t> {
    pub span: Span<'t>,
    pub name: Symbol,
    pub members: Vec<ClassMember<'t>>,
}

/// This AST node describes a class member. Variants of class members are
/// defined in `ClassMemberKind`. Every class member has a type and a name.
#[derive(Debug, PartialEq, Eq)]
pub struct ClassMember<'t> {
    pub span: Span<'t>,
    pub node: ClassMemberKind<'t>,
    /// Either the type of a `Field` or the return type of a `Method*`
    pub ty: Type<'t>,
    pub name: Symbol,
    pub is_static: bool,
}

pub type ParameterList<'t> = Vec<Parameter<'t>>;

/// A class member is either one of
/// * `Field`: a decleration of a field of a class
/// * `Method`: a method of a class
/// * `MainMethod`: a main method, which is a special method that is only
/// allowed once in a MiniJava Program
#[derive(Debug, PartialEq, Eq)]
pub enum ClassMemberKind<'t> {
    Field,
    Method(ParameterList<'t>, MethodRest<'t>, Block<'t>),
}

/// Holds the `Identifier` of the `throws` method rest.
///
/// In Java a methods can throw an exception. In MiniJava this is syntactically
/// possible but since there are no exceptions in MiniJava, it can be ignored
/// in later stages. We still need to type check for the `Identifier`.
#[derive(Debug, PartialEq, Eq)]
pub struct MethodRest<'t> {
    pub span: Span<'t>,
    pub throws: Option<Symbol>,
}

/// This AST node represents a method parameter. A parameter consists of a
/// `Type` and a name.
#[derive(Debug, PartialEq, Eq)]
pub struct Parameter<'t> {
    pub span: Span<'t>,
    pub ty: Type<'t>,
    pub name: Symbol,
}

/// A `Type` is basically a `BasicType`. Optional it can be an (n-dimensional)
/// array type.
#[derive(Debug, PartialEq, Eq)]
pub struct Type<'t> {
    pub span: Span<'t>,
    pub ty: BasicType,
    /// Depth of the array type (number of `[]`) i.e. this means means `self.ty
    /// []^(self.array)`
    pub array: u64,
}

/// A `BasicType` is either one of
/// * `Int`: a 32-bit integer
/// * `Boolean`: a boolean
/// * `Void`: a void type
/// * `Custom`: a custom defined type
#[derive(Debug, PartialEq, Eq)]
pub enum BasicType {
    Int,
    Boolean,
    Void,
    Custom(Symbol),
}

/// A `Block` in the AST is basically just a vector of statements.
#[derive(Debug, PartialEq, Eq)]
pub struct Block<'t> {
    pub span: Span<'t>,
    pub statements: Vec<Stmt<'t>>,
}

/// A statement is one of the statements defined in `StmtKind`.
#[derive(Debug, PartialEq, Eq)]
pub struct Stmt<'t> {
    pub span: Span<'t>,
    pub node: StmtKind<'t>,
}

/// A statement can have one of the kinds:
/// * `Block`: A bloch defined in `Block`
/// * `Empty`: An empty statement: `;`
/// * `If`: a if expression consisting of the condition, its body and
/// optionally an else statement * `Expression`: an expression defined in `Expr`
/// * `While`: a while loop consisting of the condition and its body
/// * `Return`: a return which can optionally return an expression
/// * `LocalVariableDeclaration`: a decleration and optional initialization of
/// a local variable
#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind<'t> {
    Block(Block<'t>),
    Empty,
    If(Box<Expr<'t>>, Box<Stmt<'t>>, Option<Box<Stmt<'t>>>),
    Expression(Box<Expr<'t>>),
    While(Box<Expr<'t>>, Box<Stmt<'t>>),
    Return(Option<Box<Expr<'t>>>),
    LocalVariableDeclaration(Type<'t>, Symbol, Option<Box<Expr<'t>>>),
}

/// An expression is one of the expressions defined in `ExprKind`
#[derive(Debug, PartialEq, Eq)]
pub struct Expr<'t> {
    pub span: Span<'t>,
    pub node: ExprKind<'t>,
}

/// An expression is either one of
/// * `Assignment`: an assignment expression
/// * `Binary`: one of the binary operations defined in `BinaryOp`
/// * `Unary`: one of the unary operations defined in `UnaryOp`
/// * `Postfix`: a primary expression with arbitrary many postfix operations
/// defined in `PostfixOp`
/// The primary expression from the original grammer are also part of this,
/// since the distinction is only required for correct postfix-op parsing. These
/// are:
/// * `Null`: the `null` keyword
/// * `Boolean`: a boolean literal
/// * `Int`: an integer literal
/// * `Var`: use of a variable
/// * `Method`: an method invocation
/// * `This`: the `this` keyword
/// * `Parenthesized`: a parenthesized expression
/// * `NewObject`: generating a new object, e.g. `new Foo()`
/// * `NewArray`: generating a new array, e.g. `new int[]`
#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'t> {
    Assignment(Box<Expr<'t>>, Vec<Expr<'t>>),
    Binary(BinaryOp, Box<Expr<'t>>, Box<Expr<'t>>),
    Unary(Vec<UnaryOp>, Box<Expr<'t>>),
    Postfix(Box<Expr<'t>>, Vec<PostfixOp<'t>>),

    // The old primary expressions
    Null,
    Boolean(bool),
    Int(Symbol), // TODO Should be String?
    Var(Symbol),
    GlobalMethodInvocation(Symbol, ArgumentList<'t>),
    This,
    NewObject(BasicType),
    NewArray(BasicType, Box<Expr<'t>>, u64),
}

/// Binary operations like comparisons (`==`, `!=`, `<=`, ...), logical
/// operations (`||`, `&&`) or algebraic operation (`+`, `-`, `*`, `/`, `%`).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,

    LogicalOr,
    LogicalAnd,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// One of the unary operations `!` and `-`
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    Not,
    Neg,
}

pub type ArgumentList<'t> = Vec<Box<Expr<'t>>>;

/// A postfix operation is one of the operations defined in `PostfixOpKind`
#[derive(Debug, PartialEq, Eq)]
pub struct PostfixOp<'t> {
    pub span: Span<'t>,
    pub node: PostfixOpKind<'t>,
}

/// A postfix operation is either one of
/// * `MethodInvocation`: a method invocation on a primary expression:
/// `foo.method()` * `FieldAccess`: a field access on a primary expression:
/// `foo.bar` * `ArrayAccess`: an array access on a primary expression:
/// `foo[42]`
#[derive(Debug, PartialEq, Eq)]
pub enum PostfixOpKind<'t> {
    MethodInvocation(Symbol, ArgumentList<'t>),
    FieldAccess(Symbol),
    ArrayAccess(Box<Expr<'t>>),
}
