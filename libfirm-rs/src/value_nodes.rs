use crate::{nodes::*, tarval::Tarval};

#[derive(Debug)]
pub struct DowncastErr(Node);

macro_rules! downcast_node {
    ($cast_name: ident, $trait_name: ident, [$($variant: ident),*]) => (
        pub fn $cast_name(node: Node) -> Result<Box<dyn $trait_name>, DowncastErr> {
            match node {
                $(
                    Node::$variant(node) => Ok(Box::new(node)),
                )*
                _ => Err(DowncastErr(node)),
            }
        }
    );
}

// ============ Value Nodes ============

pub trait ValueNode: NodeTrait {
    fn value_nodes(&self) -> Vec<Box<dyn ValueNode>>;
    fn compute(&self, values: Vec<Tarval>) -> Tarval;
}

impl ValueNode for Const {
    fn value_nodes(&self) -> Vec<Box<dyn ValueNode>> {
        vec![]
    }

    fn compute(&self, values: Vec<Tarval>) -> Tarval {
        assert!(values.is_empty());
        self.tarval()
    }
}

impl ValueNode for Phi {
    fn value_nodes(&self) -> Vec<Box<dyn ValueNode>> {
        self.in_nodes()
            .map(|n| try_as_value_node(n).unwrap())
            .collect()
    }

    fn compute(&self, values: Vec<Tarval>) -> Tarval {
        values.iter().fold(Tarval::unknown(), |a, b| a.join(*b))
    }
}

impl ValueNode for Proj {
    fn value_nodes(&self) -> Vec<Box<dyn ValueNode>> {
        match self.kind() {
            ProjKind::Div_Res(_) => vec![try_as_value_node(self.pred()).unwrap()],
            ProjKind::Mod_Res(_) => vec![try_as_value_node(self.pred()).unwrap()],
            _ => vec![],
        }
    }

    fn compute(&self, values: Vec<Tarval>) -> Tarval {
        assert!(values.len() <= 1);
        if values.len() == 1 {
            values[0]
        } else {
            Tarval::bad()
        }
    }
}

downcast_node!(
    internal_try_as_value_node,
    ValueNode,
    [
        Const, Phi, // special
        Address, Alloc, Bitcast, Call, Free, Load, Member, Mulh, Offset, Shl, Shr, Shrs,
        Size, // none
        Minus, Conv, // unary
        Add, Sub, Mul, Div, Mod, Eor, Cmp // binary
    ]
);

pub fn try_as_value_node(node: Node) -> Result<Box<dyn ValueNode>, DowncastErr> {
    match node {
        Node::Proj(node, _) => Ok(Box::new(node)),
        _ => internal_try_as_value_node(node),
    }
}

// ============ EmptyValueNode ============
macro_rules! empty_value_node_impl {
    ($node_ty: ident) => {
        impl ValueNode for $node_ty {
            fn value_nodes(&self) -> Vec<Box<dyn ValueNode>> {
                vec![]
            }

            fn compute(&self, values: Vec<Tarval>) -> Tarval {
                assert!(values.len() == 0);
                Tarval::bad()
            }
        }
    };
}

empty_value_node_impl!(Address);
empty_value_node_impl!(Alloc);
empty_value_node_impl!(Bitcast);
empty_value_node_impl!(Call);
empty_value_node_impl!(Free);
empty_value_node_impl!(Load);
empty_value_node_impl!(Member);
empty_value_node_impl!(Mulh);
empty_value_node_impl!(Offset);
empty_value_node_impl!(Shl);
empty_value_node_impl!(Shr);
empty_value_node_impl!(Shrs);
empty_value_node_impl!(Size);

// ============ BinOps ============

pub trait BinOp {
    fn left(&self) -> Box<dyn ValueNode>;
    fn right(&self) -> Box<dyn ValueNode>;
    fn compute(&self, left: Tarval, right: Tarval) -> Tarval;
}

macro_rules! binop_impl {
    ($node_ty: ident, $compute: expr) => {
        impl BinOp for $node_ty {
            fn left(&self) -> Box<dyn ValueNode> {
                try_as_value_node($node_ty::left(*self)).unwrap()
            }
            fn right(&self) -> Box<dyn ValueNode> {
                try_as_value_node($node_ty::right(*self)).unwrap()
            }
            fn compute(&self, left: Tarval, right: Tarval) -> Tarval {
                $compute(self, left, right)
            }
        }

        impl ValueNode for $node_ty {
            fn value_nodes(&self) -> Vec<Box<dyn ValueNode>> {
                vec![BinOp::left(self), BinOp::right(self)]
            }

            fn compute(&self, values: Vec<Tarval>) -> Tarval {
                assert!(values.len() == 2);
                BinOp::compute(self, values[0], values[1])
            }
        }
    };
}

binop_impl!(Add, |_n, l, r| l + r);
binop_impl!(Sub, |_n, l, r| l - r);
binop_impl!(Mul, |_n, l, r| l * r);
binop_impl!(Div, |_n, l, r| l / r);
binop_impl!(Mod, |_n, l, r| l % r);
binop_impl!(Eor, |_n, l, r| l ^ r);
binop_impl!(Cmp, |n: &Cmp, l: Tarval, r| l.lattice_cmp(n.relation(), r));

downcast_node!(try_as_bin_op, BinOp, [Add, Sub, Mul, Div, Mod, Eor, Cmp]);

// ============ UnaryOps ===========

pub trait UnaryOp {
    fn operand(&self) -> Box<dyn ValueNode>;
    fn compute(&self, val: Tarval) -> Tarval;
}

macro_rules! unaryop_impl {
    ($node_ty: ident, $compute: expr) => {
        impl UnaryOp for $node_ty {
            fn operand(&self) -> Box<dyn ValueNode> {
                try_as_value_node(self.op()).unwrap()
            }
            fn compute(&self, val: Tarval) -> Tarval {
                $compute(self, val)
            }
        }

        impl ValueNode for $node_ty {
            fn value_nodes(&self) -> Vec<Box<dyn ValueNode>> {
                vec![self.operand()]
            }

            fn compute(&self, values: Vec<Tarval>) -> Tarval {
                assert!(values.len() == 1);
                UnaryOp::compute(self, values[0])
            }
        }
    };
}

unaryop_impl!(Minus, |_n, val: Tarval| -val);
unaryop_impl!(Conv, |n: &Conv, val: Tarval| val
    .cast(n.mode())
    .unwrap_or_else(Tarval::bad));
downcast_node!(try_as_unary_op, UnaryOp, [Minus, Conv]);
