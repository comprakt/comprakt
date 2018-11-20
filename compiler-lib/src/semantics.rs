use crate::{
    asciifile::{MaybeSpanned, Span, Spanned},
    ast,
    context::Context,
    strtab::{self, Symbol},
    visitor::NodeKind,
};
use failure::Fail;
use std::collections::HashMap;

#[derive(Debug, Fail)]
pub enum SemanticError {
    #[fail(display = "redefinition of {} '{}'", kind, name)]
    RedefinitionError {
        kind: String, // "class", "parameter", ...
        name: String, // name of the parameter class...
    },

    #[fail(
        display = "Usage of the parameter '{}' of the main function",
        name
    )]
    MainMethodParamUsed { name: String },

    #[fail(display = "Only the 'main' method can be static")]
    StaticMethodNotMain,

    #[fail(display = "No 'main' method found")]
    NoMainMethod,

    #[fail(
        display = "{}. definition of a static method. Only one is allowed",
        amount
    )]
    MultipleStaticMethods { amount: u64 },

    #[fail(display = "non-static variable 'this' cannot be referenced from a static context")]
    ThisInStaticMethod,

    #[fail(display = "method '{}' might not return", method_name)]
    MightNotReturn { method_name: String },

    #[fail(
        display = "non-static method '{}' cannot be referenced from a static context",
        method_name
    )]
    ThisMethodInvocationInStaticMethod { method_name: String },

    #[fail(display = "cannot call static method '{}'", method_name)]
    CannotCallStaticMethod { method_name: String },

    #[fail(display = "condition must be boolean")]
    ConditionMustBeBoolean,

    #[fail(display = "cannot lookup var or field '{}'", name)]
    CannotLookupVarOrField { name: String },

    #[fail(
        display = "cannot access non static field '{}' in static method",
        field_name
    )]
    CannotAccessNonStaticFieldInStaticMethod { field_name: String },

    #[fail(display = "method cannot return a value")]
    VoidMethodCannotReturnValue,

    #[fail(display = "type 'void' is not allowed here")]
    VoidNotAllowed,

    #[fail(display = "method must return a value of type '{}'", ty)]
    MethodMustReturnSomething { ty: String },

    #[fail(
        display = "invalid type: Expected expression of type '{}', but was of type '{}'",
        ty_expected,
        ty_expr
    )]
    InvalidType {
        ty_expected: String,
        ty_expr: String,
    },

    #[fail(display = "cannot reference class '{}' here", class_name)]
    InvalidReferenceToClass { class_name: String },

    #[fail(display = "class '{}' does not exist", class_name)]
    ClassDoesNotExist { class_name: String },

    #[fail(display = "cannot index non-array type '{}'", ty)]
    CannotIndexNonArrayType { ty: String },

    #[fail(
        display = "method '{}' does not exist on type '{}'",
        method_name,
        ty
    )]
    MethodDoesNotExistOnType { method_name: String, ty: String },

    #[fail(
        display = "field '{}' does not exist on type '{}'",
        field_name,
        ty
    )]
    FieldDoesNotExistOnType { field_name: String, ty: String },

    #[fail(
        display = "method argument count does not match: Expected {} arguments, but found {}",
        expected_args,
        actual_args
    )]
    MethodArgCountDoesNotMatch {
        expected_args: usize,
        actual_args: usize,
    },

    #[fail(
        display = "cannot compare values of type '{}' with values of type '{}'",
        ty1,
        ty2
    )]
    CannotCompareValuesOfType1WithType2 { ty1: String, ty2: String },

    #[fail(display = "not a statement")]
    NotAStatement,

    #[fail(
        display = "invalid assignment - can only assign to \
                   local variables, parameters, field and array fields"
    )]
    InvalidAssignment,

    #[fail(display = "Cannot write to read-only field '{}'", field_name)]
    CannotWriteToReadOnlyField { field_name: String },

    #[fail(display = "integer number too large: {}", int)]
    IntTooLarge { int: String },
}

/// `check` returns an `Err` iff at least one errors was emitted through
/// `context`.
pub fn check<'a, 'f>(
    strtab: &mut strtab::StringTable<'f>,
    ast: &'a ast::AST<'f>,
    context: &Context<'f>,
) -> Result<(), ()> {
    let mut first_pass_visitor = ClassesAndMembersVisitor::new(context);
    first_pass_visitor.do_visit(&NodeKind::from(ast));

    // Check if a static method was found. If multiple static methods were found or
    // the static method is not called `main` the error is already emitted in
    // the visitor
    if first_pass_visitor.static_method_found == 0 {
        context
            .diagnostics
            .error(&MaybeSpanned::WithoutSpan(SemanticError::NoMainMethod));
    }

    if context.diagnostics.errored() {
        return Err(());
    }

    crate::type_checking::check(strtab, &ast, &context);
    if context.diagnostics.errored() {
        return Err(());
    }
    Ok(())
}

struct ClassesAndMembersVisitor<'f, 'cx> {
    context: &'cx Context<'cx>,
    static_method_found: u64,
    class_member_to_its_span: HashMap<*const ast::ClassMember<'f>, Span<'f>>,
}

impl<'f, 'cx> ClassesAndMembersVisitor<'f, 'cx> {
    pub fn new(context: &'cx Context<'_>) -> Self {
        Self {
            context,
            static_method_found: 0,
            class_member_to_its_span: HashMap::new(),
        }
    }

    fn do_visit(&mut self, node: &NodeKind<'_, 'f>) {
        use self::{ast, NodeKind::*};
        node.for_each_child(&mut |child| {
            match child {
                ClassDeclaration(decl) => {
                    let decl_node = NodeKind::from(decl);
                    decl_node.for_each_child(&mut |member_node| {
                        let member_decl: &Spanned<'_, ast::ClassMember<'_>> = match member_node {
                            NodeKind::ClassMember(m) => m,
                            _ => panic!("class children are expected to be class members"),
                        };
                        self.class_member_to_its_span
                            .insert(&member_decl.data as *const _, member_decl.span);
                    });
                }

                ClassMember(member) => {
                    if let ast::ClassMemberKind::MainMethod(params, _) = &member.kind {
                        debug_assert!(params.len() == 1);
                        self.static_method_found += 1;
                        if &member.name != "main" {
                            self.context.diagnostics.error(&Spanned {
                                span: member.span,
                                data: SemanticError::StaticMethodNotMain,
                            });
                        }
                        if self.static_method_found > 1 {
                            self.context.diagnostics.error(&Spanned {
                                span: member.span,
                                data: SemanticError::MultipleStaticMethods {
                                    amount: self.static_method_found,
                                },
                            })
                        }
                    }

                    match &member.kind {
                        ast::ClassMemberKind::Method(ty, pl, block)
                            if ty.basic.data != ast::BasicType::Void =>
                        {
                            let ptr = (&member.data) as *const _;
                            let member_decl = self
                                .class_member_to_its_span
                                .get(&ptr)
                                .expect("must have current_member_decl while visiting ClassMember");
                            let highlight_span = Span::from_positions(&[
                                member_decl.start_position(),
                                pl.span.end_position(),
                            ])
                            .unwrap();
                            self.check_method_always_returns(&member.name, highlight_span, block)
                        }
                        _ => (),
                    }
                }

                Stmt(stmt) => {
                    use crate::ast::Expr::*;
                    if let ast::Stmt::Expression(expr) = &stmt.data {
                        match &expr.data {
                            Binary(ast::BinaryOp::Assign, _, _)
                            | MethodInvocation(..)
                            | ThisMethodInvocation(..) => (),
                            _ => {
                                //Err
                                self.context.diagnostics.error(&Spanned {
                                    span: stmt.span,
                                    data: SemanticError::NotAStatement,
                                });
                            }
                        }
                    }
                }

                Expr(expr) => match &expr.data {
                    ast::Expr::NegInt(int) if int.data != "2147483648" => {
                        self.check_int(int.data, int.span)
                    }
                    ast::Expr::Int(int) => self.check_int(int.data, int.span),
                    _ => (),
                },

                _ => (),
            };

            self.do_visit(&child)
        });
    }

    fn check_int(&self, int: &str, span: Span<'_>) {
        if int.parse::<i32>().is_err() {
            self.context.diagnostics.error(&Spanned {
                span,
                data: SemanticError::IntTooLarge {
                    int: int.to_string(),
                },
            });
        }
    }

    fn check_method_always_returns(
        &self,
        method_name: &Symbol<'_>,
        hightlight_span: Span<'_>,
        method_body: &Spanned<'_, ast::Block<'_>>,
    ) {
        fn always_returns<'t>(stmt: &Spanned<'t, ast::Stmt<'t>>) -> bool {
            match &stmt.data {
                // An if-else stmt always returns iff both arms always return
                ast::Stmt::If(_, then_arm, else_arm) => {
                    let then_arm_always_returns = always_returns(&*then_arm);
                    let else_arm_always_returns = else_arm
                        .as_ref()
                        .map_or(false, |else_arm| always_returns(&*else_arm));

                    then_arm_always_returns && else_arm_always_returns
                }

                // An empty block does not return
                ast::Stmt::Block(block) if block.statements.is_empty() => false,
                // A non-empty block always returns iff any of its top-level statements
                // always returns
                ast::Stmt::Block(block) => block.statements.iter().any(always_returns),

                // A return stmt always returns
                ast::Stmt::Return(_) => true,

                // All other stmts do not always return
                _ => false,
            }
        }

        // FIXME de-duplicate empty block logic from always_returns
        if method_body.statements.is_empty() || !method_body.statements.iter().any(always_returns) {
            self.context.diagnostics.error(&Spanned {
                span: hightlight_span,
                data: SemanticError::MightNotReturn {
                    method_name: format!("{}", method_name),
                },
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        asciifile::AsciiFile,
        lexer::{Lexer, TokenKind},
        parser::Parser,
        strtab::StringTable,
    };
    use mjtest::SemanticTestCase;
    use mjtest_macros::gen_semantic_tests;

    macro_rules! gen_check_code {
        ($check_res:ident = $input:expr) => {
            let ascii_file = AsciiFile::new($input).unwrap();
            let context = Context::dummy(&ascii_file);
            let mut strtab = StringTable::new();
            let lexer = Lexer::new(&mut strtab, &context);
            let unforgiving_lexer = lexer.filter_map(|result| match result {
                Ok(token) => match token.data {
                    TokenKind::Whitespace | TokenKind::Comment(_) => None,
                    _ => Some(token),
                },
                Err(lexical_error) => panic!("{}", lexical_error),
            });
            let ast = Parser::new(unforgiving_lexer).parse().unwrap();
            let $check_res = super::check(&mut strtab, &ast, &context);
        };
    }

    fn do_mjtest_semantic_test(tc: &SemanticTestCase) {
        println!("file name: {:?}", tc.file_name());
        let input = std::fs::read_to_string(tc.path()).unwrap().into_bytes();
        gen_check_code!(check_res = &input);
        match (tc, check_res) {
            (SemanticTestCase::Valid(_), Ok(_)) => (),
            (SemanticTestCase::Invalid(_), Err(_)) => (),
            (tc, res) => {
                println!("test case: {:?}", tc);
                println!("result:    {:?}", res);
                assert!(false);
            }
        }
    }
    gen_semantic_tests!((do_mjtest_semantic_test, []));

    #[derive(Debug, PartialEq, Eq)]
    enum BinopCheckResult {
        Accept,
        Error,
    }

    macro_rules! binop_test {
        (internal $mjcode:expr, $expected:ident, $testname:ident)  => {
            #[test]
            fn $testname() {
                let prog = $mjcode;
                let input = prog.into_bytes();
                gen_check_code!(check_res = &input);
                let exp = BinopCheckResult::$expected;
                match (exp, check_res) {
                    (BinopCheckResult::Accept, Ok(_)) => (),
                    (BinopCheckResult::Error, Err(_)) => (),
                    (exp, act) => {
                        println!("expected: {:?}", exp);
                        println!("actual:   {:?}", act);
                        assert!(false);
                    }
                }
            }
        };
        ($t1:expr, $val1:expr, $t2:expr, $val2:expr, $t3:expr, $op:expr, $exp:ident, $n:ident) => {
            binop_test!(internal format!(r"
                class C {{}}
                class D {{}}
                class BinopCheck {{
                    public static void main(String[] args) {{
                        {} v1 = {};
                        {} v2 = {};
                        {} v3 = v1 {} v2;
                    }}
                }}",
                $t1, $val1, $t2, $val2, $t3, $op), $exp, $n
            );
        };
        ($lit1:expr, $op:expr, $lit2:expr, $expected:ident, $testname:ident) => {
            binop_test!(internal format!(r"
                class C {{}}
                class D {{}}
                class BinopCheck {{
                    public static void main(String[] args) {{
                        if ({} {} {}) {{
                            System.out.println(23);
                        }}
                    }}
                }}",
                $lit1, $op, $lit2), $expected, $testname
            );
        };
    }

    #[rustfmt::skip]
    mod binop {
        use super::*;
        binop_test!("int", "1", "boolean", "true", "int", "==",    Error,  eq_int_bool_eq);
        binop_test!("C", "null", "C", "null", "boolean", "==",     Accept, eq_same_class_null);
        binop_test!("C", "null", "int", "23", "boolean", "==",     Error,  eq_class_int);
        binop_test!("C", "null", "boolean", "23", "boolean", "==", Error,  eq_class_bool);
        binop_test!("C", "null", "D", "null", "boolean", "==",     Error,  eq_class_class_eq);
        binop_test!("23", "==", "42",                              Accept, eq_ints);
        binop_test!("23", "==", "null",                            Error,  eq_integer_null);
        binop_test!("0", "==", "null",                             Error,  eq_integer0_null);
        binop_test!("true", "==", "null",                          Error,  eq_true_null);
        binop_test!("false", "==", "null",                         Error,  eq_false_null );
        binop_test!("0", "==", "false",                            Error,  eq_zero_false);
        binop_test!("1", "==", "true",                             Error,  eq_one_true);
        binop_test!("(new C())", "==", "null",                     Accept, eq_new_null);
        binop_test!("(new C())", "==", "(new D())",                Error,  eq_new_new_2types);
    }
}
