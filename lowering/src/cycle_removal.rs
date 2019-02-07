//! Cycle-removal for copy-propagation (swap problem) & function call arguments.

use std::{
    collections::{HashMap, HashSet, VecDeque},
    ops::{Deref, DerefMut},
};

#[derive(Clone)]
pub struct Node<R> {
    reg: R,

    in_: Option<R>,
    outs: HashSet<R>,
}

impl<R: std::hash::Hash + Eq> Node<R> {
    fn new(reg: R) -> Self {
        Self {
            reg,
            in_: None,
            outs: HashSet::new(),
        }
    }

    fn set_in_edge(&mut self, reg: R) {
        debug_assert!(self.in_.is_none());
        self.in_ = Some(reg);
    }

    fn add_out_edge(&mut self, reg: R) {
        self.outs.insert(reg);
    }

    fn is_sink(&self) -> bool {
        self.outs.is_empty()
    }

    fn is_source(&self) -> bool {
        self.in_.is_none()
    }
}

#[derive(Default)]
pub struct RegGraph<R: std::hash::Hash + Eq>(HashMap<R, Node<R>>);

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub struct RegToRegTransfer<R: std::hash::Hash + Eq + Clone + Copy> {
    pub(super) src: R,
    pub(super) dst: R,
}

pub enum RegGraphMinLeftEdgeInstruction<R: std::hash::Hash + Eq + Clone + Copy> {
    Push(R),
    Pop(R),
    Mov(RegToRegTransfer<R>),
}

impl<R: std::hash::Hash + Eq + Clone + Copy> RegGraph<R> {
    pub fn new(transfers: Vec<RegToRegTransfer<R>>) -> Self {
        let mut reg_graph = Self(HashMap::new());
        for RegToRegTransfer { src, dst } in transfers {
            if src != dst {
                let src_node = Node::new(src);
                let dst_node = Node::new(dst);
                reg_graph.entry(src).or_insert(src_node).add_out_edge(dst);
                reg_graph.entry(dst).or_insert(dst_node).set_in_edge(src);
            }
        }

        reg_graph
    }

    /// This is a greedy cycle removal algorithm from "Graph Drawing: Algorithms
    /// for the Visualization of Graphs" by Eades et al.
    // TODO(flip1995): comment what it does
    // TODO(problame): use Vecs (r_nodes.push_front) can be replace by reversing the
    // vec in the end)
    fn gen_node_list_with_min_left_edges(mut self) -> VecDeque<Node<R>> {
        let mut l_nodes = VecDeque::new();
        let mut r_nodes = VecDeque::new();
        let mut visited = HashSet::new();

        while !self.is_empty() {
            let mut sink_exists = true;
            while sink_exists {
                let sinks: Vec<_> = self
                    .values()
                    .filter(|node| {
                        node.is_sink() || node.outs.iter().all(|reg| visited.contains(reg))
                    })
                    .cloned()
                    .collect();
                sink_exists = false;
                for node in &sinks {
                    self.remove(&node.reg);
                    visited.insert(node.reg);
                    r_nodes.push_front(node.clone());
                    if let Some(pred) = &node.in_ {
                        if let Some(pred_node) = self.get(&pred) {
                            if pred_node.outs.iter().all(|reg| visited.contains(reg)) {
                                if pred_node.in_.is_none() {
                                    // Isolated node
                                    r_nodes.push_front(pred_node.clone());
                                    self.remove(&pred);
                                } else {
                                    sink_exists = true;
                                }
                            }
                        }
                    }
                }
            }

            let mut source_exists = true;
            while source_exists {
                let sources: Vec<_> = self
                    .values()
                    .filter(|node| node.is_source() || visited.contains(&node.in_.unwrap()))
                    .cloned()
                    .collect();
                source_exists = false;
                for node in &sources {
                    self.remove(&node.reg);
                    visited.insert(node.reg);
                    l_nodes.push_back(node.clone());
                    source_exists = true;
                }
            }

            if !self.is_empty() {
                let node = self
                    .values()
                    .max_by_key(|node| {
                        node.outs
                            .iter()
                            .filter(|reg| !visited.contains(reg))
                            .count()
                            - if node.in_.is_some() { 1 } else { 0 }
                    })
                    .cloned()
                    .unwrap();
                self.remove(&node.reg);
                visited.insert(node.reg);
                l_nodes.push_back(node);
            }
        }

        l_nodes.append(&mut r_nodes);
        l_nodes
    }

    pub fn into_instructions<I: From<RegGraphMinLeftEdgeInstruction<R>>>(
        self,
    ) -> impl Iterator<Item = I> {
        let mut instrs = vec![];

        let reg_graph_len = self.len();
        let nodes = self.gen_node_list_with_min_left_edges();

        debug_assert_eq!(nodes.len(), reg_graph_len);

        let mut recover = vec![];
        use self::RegGraphMinLeftEdgeInstruction::*;
        let mut visited = HashSet::new();
        for node in nodes.iter().rev() {
            visited.insert(node.reg);
            for out in node.outs.iter().filter(|reg| !visited.contains(reg)) {
                instrs.push(Push(node.reg));
                recover.push(Pop(*out));
            }

            if let Some(in_) = node.in_ {
                if !visited.contains(&in_) {
                    instrs.push(Mov(RegToRegTransfer {
                        src: in_,
                        dst: node.reg,
                    }));
                }
            }
        }
        instrs.extend(recover.into_iter().rev());
        instrs.into_iter().map(From::from)
    }
}

impl<R: std::hash::Hash + Eq> Deref for RegGraph<R> {
    type Target = HashMap<R, Node<R>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<R: std::hash::Hash + Eq> DerefMut for RegGraph<R> {
    fn deref_mut(&mut self) -> &mut HashMap<R, Node<R>> {
        &mut self.0
    }
}
