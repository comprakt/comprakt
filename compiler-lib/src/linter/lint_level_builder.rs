use super::UnknownLint;
use asciifile::Spanned;
use diagnostics::{lint::LintId, MessageLevel};
use parser::ast::Attribute;
use std::collections::HashMap;

#[derive(Debug)]
pub(super) struct LintLevelBuilder {
    sets: Vec<LintSet>,
    by_name: HashMap<String, LintId>,
    cur: u32,
}

#[derive(Debug)]
struct LintSet {
    specs: HashMap<LintId, MessageLevel>,
    parent: u32,
}

pub struct BuilderPush {
    prev: u32,
}

impl LintLevelBuilder {
    pub fn new(by_name: &HashMap<String, LintId>) -> Self {
        Self {
            sets: vec![],
            by_name: by_name.clone(),
            cur: 0,
        }
    }

    pub fn push(
        &mut self,
        attrs: &[Spanned<'_, Attribute<'_>>],
    ) -> Result<BuilderPush, UnknownLint> {
        let mut specs = HashMap::new();
        for attr in attrs {
            if let Attribute::LintLevel(level, name) = &attr.data {
                if let Some(id) = self.by_name.get(name.as_str()) {
                    specs.insert(*id, *level);
                } else {
                    return Err(UnknownLint {
                        name: name.to_string(),
                    });
                }
            }
        }

        let prev = self.cur;
        if !specs.is_empty() {
            self.cur = self.sets.len() as u32;
            self.sets.push(LintSet {
                specs,
                parent: prev,
            });
        }

        Ok(BuilderPush { prev })
    }

    pub fn pop(&mut self, push: BuilderPush) {
        self.cur = push.prev;
    }

    pub fn lint_level(&self, id: LintId) -> Option<MessageLevel> {
        let mut idx = self.cur;
        if !self.sets.is_empty() {
            loop {
                let lint_set = &self.sets[idx as usize];
                log::debug!("lint_level: lint_set: {:?}", lint_set);
                if let Some(lvl) = lint_set.specs.get(&id) {
                    return Some(*lvl);
                }
                if idx == 0 {
                    break;
                }
                idx = lint_set.parent;
            }
        }

        None
    }
}
