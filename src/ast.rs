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

pub type ParameterList<'t> = Vec<Parameter<'t>>;

#[derive(Debug, PartialEq, Eq)]
pub enum ClassMemberKind<'t> {
    Field,
    Method(ParameterList<'t>, MethodRest<'t>, Block<'t>),
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

    Mod,
    Sub,
    Add,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    Not,
    Neg,
}

pub type ArgumentList<'t> = Vec<Box<Expr<'t>>>;

#[derive(Debug, PartialEq, Eq)]
pub struct PostfixOp<'t> {
    span: Span<'t>,
    node: PostfixOpKind<'t>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PostfixOpKind<'t> {
    MethodInvocation(Symbol, ArgumentList<'t>),
    FieldAccess(Symbol),
    ArrayAccess(Box<Expr<'t>>),
}
