use crate::lexer::Span;
use crate::strtab::Symbol;

#[derive(Debug, PartialEq, Eq)]
pub struct Program<'t> {
    span: Span<'t>,
    classes: Vec<ClassDeclaration<'t>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassDeclaration<'t> {
    span: Span<'t>,
    name: Symbol,
    members: Vec<ClassMember<'t>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassMember<'t> {
    span: Span<'t>,
    node: ClassMemberKind<'t>,
    ty: Type<'t>,
    name: Symbol,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClassMemberKind<'t> {
    Field,
    Method(Vec<Parameter<'t>>, MethodRest<'t>, Block<'t>),
    MainMethod(Parameter<'t>, MethodRest<'t>, Block<'t>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct MethodRest<'t> {
    span: Span<'t>,
    throws: Symbol,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter<'t> {
    pub span: Span<'t>,
    pub ty: Type<'t>,
    pub name: Symbol,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Type<'t> {
    pub span: Span<'t>,
    pub ty: BasicType,
    /// Depth of the array type (number of `[]`) i.e. this means means `self.ty []^(self.array)`
    pub array: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BasicType{
    Int,
    Boolean,
    Void,
    Custom(Symbol),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'t> {
    span: Span<'t>,
    statements: Vec<Stmt<'t>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt<'t> {
    span: Span<'t>,
    node: StmtKind<'t>,
}

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

#[derive(Debug, PartialEq, Eq)]
pub struct Expr<'t> {
    span: Span<'t>,
    node: ExprKind<'t>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<'t> {
    LogicalOr(Box<Expr<'t>>, Box<Expr<'t>>),
    LogicalAnd(Box<Expr<'t>>, Box<Expr<'t>>),
    Equality(Box<Expr<'t>>, Box<Expr<'t>>),
    Relational(Box<Expr<'t>>, Box<Expr<'t>>),
    Additive(Box<Expr<'t>>, Box<Expr<'t>>),
    Multiplicative(Box<Expr<'t>>, Box<Expr<'t>>),
    Unary(Box<Expr<'t>>),
    Postfix(PrimaryExprKind<'t>, Vec<PostfixOp<'t>>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum PrimaryExprKind<'t> {
    Null,
    Boolean(bool),
    Int(i32),
    Var(Symbol),
    Method(Symbol, Vec<Box<Expr<'t>>>),
    This,
    Parenthesized(Box<Expr<'t>>),
    NewObject(Symbol),
    NewArray(BasicType, Box<Expr<'t>>, u64),
}

#[derive(Debug, PartialEq, Eq)]
pub struct PostfixOp<'t> {
    span: Span<'t>,
    node: PostfixOpKind<'t>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PostfixOpKind<'t> {
    MethodInvocation(Symbol, Vec<Box<Expr<'t>>>),
    FieldAccess(Symbol),
    ArrayAccess(Box<Expr<'t>>),
}

