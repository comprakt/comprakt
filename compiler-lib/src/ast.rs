use crate::{asciifile::Spanned, lexer::IntLit, strtab::Symbol};
use strum_macros::EnumDiscriminants;

#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants, Debug, PartialEq, Eq)]
pub enum AST<'t> {
    Empty,
    Program(Spanned<'t, Program<'t>>),
}

/// This is the top-level AST node. It stores all class declarations of the
/// MiniJava program.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program<'t> {
    pub classes: Vec<Spanned<'t, ClassDeclaration<'t>>>,
}

/// This AST node stores the Class declaration, which consists of a name and
/// the members of the class.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassDeclaration<'t> {
    pub name: Spanned<'t, Symbol<'t>>,
    pub members: Vec<Spanned<'t, ClassMember<'t>>>,
}

/// This AST node describes a class member. Variants of class members are
/// defined in `ClassMemberKind`. Every class member has a name.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassMember<'t> {
    pub kind: ClassMemberKind<'t>,
    pub name: Symbol<'t>,
}

pub type ParameterList<'t> = Vec<Spanned<'t, Parameter<'t>>>;

/// A class member is either one of
/// * `Field(type)`: a declaration of a field of a class
/// * `Method(type, params, body)`: a method of a class
/// * `MainMethod(params, body)`: a main method, which is a special method that
/// is only allowed once in a MiniJava Program. `params` is guaranteed to
/// only contain the `String[] IDENT` parameter.
#[strum_discriminants(derive(Display, Hash, PartialOrd, Ord))]
#[derive(EnumDiscriminants, Debug, PartialEq, Eq, Clone)]
pub enum ClassMemberKind<'t> {
    Field(Spanned<'t, Type<'t>>),
    Method(
        Spanned<'t, Type<'t>>,
        Spanned<'t, ParameterList<'t>>,
        Spanned<'t, Block<'t>>,
    ),
    MainMethod(Spanned<'t, ParameterList<'t>>, Spanned<'t, Block<'t>>),
}

/// This AST node represents a method parameter. A parameter consists of a
/// `Type<'t>` and a name.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter<'t> {
    pub ty: Spanned<'t, Type<'t>>,
    pub name: Symbol<'t>,
}

/// A `Type<'t>` is basically a `BasicType<'t>`. Optional it can be an
/// (n-dimensional) array type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type<'t> {
    pub basic: Spanned<'t, BasicType<'t>>,
    /// Depth of the array type (number of `[]`) i.e. this means means `self.ty
    /// []^(self.array)`
    pub array_depth: u64,
}

/// A `BasicType<'t>` is either one of
/// * `Int`: a 32-bit integer
/// * `Boolean`: a boolean
/// * `Void`: a void type
/// * `Custom`: a custom defined type
#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants, Debug, PartialEq, Eq, Clone)]
pub enum BasicType<'t> {
    Int,
    Boolean,
    Void,
    Custom(Symbol<'t>),
    MainParam,
}

/// A `Block` in the AST is basically just a vector of statements.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block<'t> {
    pub statements: Vec<Spanned<'t, Stmt<'t>>>,
}

/// A statement can have one of the kinds:
/// * `Block`: A block defined in `Block`
/// * `Empty`: An empty statement: `;`
/// * `If`: a if expression consisting of the condition, its body and
/// optionally an else statement
/// * `Expression`: an expression defined in `Expr`
/// * `While`: a while loop consisting of the condition and its body
/// * `Return`: a return which can optionally return an expression
/// * `LocalVariableDeclaration`: a declaration and optional initialization of
/// a local variable
#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants, Debug, PartialEq, Eq, Clone)]
// TODO: should all be named, especially variants with 3 arguments.
// `LocalVariableDeclaration`
pub enum Stmt<'t> {
    Block(Spanned<'t, Block<'t>>),
    Empty,
    If(
        Box<Spanned<'t, Expr<'t>>>,
        Box<Spanned<'t, Stmt<'t>>>,
        Option<Box<Spanned<'t, Stmt<'t>>>>,
    ),
    While(Box<Spanned<'t, Expr<'t>>>, Box<Spanned<'t, Stmt<'t>>>),
    Expression(Box<Spanned<'t, Expr<'t>>>),
    Return(Option<Box<Spanned<'t, Expr<'t>>>>),
    LocalVariableDeclaration(
        Spanned<'t, Type<'t>>,
        Spanned<'t, Symbol<'t>>,
        Option<Box<Spanned<'t, Expr<'t>>>>,
    ),
}

/// An expression is either one of
/// * `Assignment`: an assignment expression
/// * `Binary`: one of the binary operations defined in `BinaryOp`
/// * `Unary`: one of the unary operations defined in `UnaryOp`
/// * `MethodInvocation`: a method invocation on a primary expression:
/// `foo.method()`
/// * `FieldAccess`: a field access on a primary expression:
/// `foo.bar`
/// * `ArrayAccess`: an array access on a primary expression:
/// `foo[42]`
/// The primary expression from the original grammar are also part of this,
/// since the distinction is only required for correct postfix-op parsing. These
/// are:
/// * `Null`: the `null` keyword
/// * `Boolean`: a boolean literal
/// * `Int`: an integer literal
/// * `Var`: use of a variable
/// * `MethodInvocation`: a method invocation
/// * `This`: the `this` keyword
/// * `NewObject`: generating a new object, e.g. `new Foo()`
/// * `NewArray`: generating a new array, e.g. `new int[]`
#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants, Debug, PartialEq, Eq, Clone)]
pub enum Expr<'t> {
    Binary(
        BinaryOp,
        Box<Spanned<'t, Expr<'t>>>,
        Box<Spanned<'t, Expr<'t>>>,
    ),
    Unary(UnaryOp, Box<Spanned<'t, Expr<'t>>>),

    // Postfix ops
    MethodInvocation(
        Box<Spanned<'t, Expr<'t>>>,
        Spanned<'t, Symbol<'t>>,
        Spanned<'t, ArgumentList<'t>>,
    ),
    FieldAccess(Box<Spanned<'t, Expr<'t>>>, Spanned<'t, Symbol<'t>>),
    ArrayAccess(Box<Spanned<'t, Expr<'t>>>, Box<Spanned<'t, Expr<'t>>>),

    // The old primary expressions
    Null,
    Boolean(bool),
    Int(Spanned<'t, IntLit<'t>>),
    Var(Spanned<'t, Symbol<'t>>),
    ThisMethodInvocation(Spanned<'t, Symbol<'t>>, Spanned<'t, ArgumentList<'t>>),
    This,
    NewObject(Spanned<'t, Symbol<'t>>),
    NewArray(Spanned<'t, BasicType<'t>>, Box<Spanned<'t, Expr<'t>>>, u64),
}

/// Binary operations like comparisons (`==`, `!=`, `<=`, ...), logical
/// operations (`||`, `&&`) or algebraic operation (`+`, `-`, `*`, `/`, `%`).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    Assign,

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

pub type ArgumentList<'t> = Vec<Spanned<'t, Expr<'t>>>;

impl<'f> ClassMemberKind<'f> {
    pub fn is_method(&self) -> bool {
        use self::ClassMemberKind::*;
        match self {
            Method(_, _, _) => true,
            MainMethod(_, _) => true,
            Field(_) => false,
        }
    }
    pub fn method_params(&self) -> Option<&Spanned<'_, ParameterList<'_>>> {
        use self::ClassMemberKind::*;
        match &self {
            Method(_t, pl, _block) => Some(pl),
            MainMethod(pl, _block) => Some(pl),
            Field(_) => None,
        }
    }
    pub fn method_body(&self) -> Option<&Spanned<'_, Block<'_>>> {
        use self::ClassMemberKind::*;
        match &self {
            Method(_t, _pl, block) => Some(block),
            MainMethod(_pl, block) => Some(block),
            Field(_) => None,
        }
    }
}

impl std::cmp::PartialOrd for ClassMember<'_> {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(crate::print::pretty::compare_class_member(self, rhs))
    }
}

impl std::cmp::Ord for ClassMember<'_> {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        crate::print::pretty::compare_class_member(self, rhs)
    }
}
