use crate::codegen::Instruction;

pub(crate) fn global_peephole(function_asm: &mut Vec<Instruction>) {
    let peepholers: Vec<Box<dyn GlobalPeepholeOptimization>> = vec![
        box jmp_targets::CondJmpSwapTargets,
        box jmp_targets::Fallthrough,
        box mov::MovSame,
    ];
    peepholers.into_iter().for_each(|mut peepholer| {
        peepholer.run(function_asm);
    })
}

trait GlobalPeepholeOptimization {
    /// Run the optimization and modify the instructin list in place.
    /// unused instruction slots should be replaced by Instruction::PeepholedOut
    fn run(&mut self, asm: &mut Vec<Instruction>);
}

mod jmp_targets;
mod mov;

/// Unsafe sliding window iterator over a given mut slice.
struct UnsafeSlidingWindow<'s, T> {
    slice: &'s mut [T],
    width: usize,
    pos: usize,
}

impl<'s, T> UnsafeSlidingWindow<'s, T> {
    fn new(slice: &'s mut [T], width: usize) -> UnsafeSlidingWindow<'s, T> {
        assert!(width > 0);
        UnsafeSlidingWindow {
            slice,
            width,
            pos: 0,
        }
    }
}

impl<'s, T> Iterator for UnsafeSlidingWindow<'s, T> {
    type Item = &'s mut [T];
    fn next(&mut self) -> Option<Self::Item> {
        let win_start = self.pos;
        // end is exclusive, i.e. not part of the window
        let win_end = self.pos + self.width;
        if self.slice.len() >= win_end {
            self.pos += 1;
            let short_lived = &mut self.slice[win_start..win_end];
            Some(unsafe { std::mem::transmute(short_lived) })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod sliding_window_test {
    use super::*;

    #[test]
    fn basics() {
        let mut input = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let win = UnsafeSlidingWindow::new(&mut input, 2);
        let wins = win.collect::<Vec<_>>();
        assert_eq!(
            &[
                &[1, 2],
                &[2, 3],
                &[3, 4],
                &[4, 5],
                &[5, 6],
                &[6, 7],
                &[7, 8]
            ],
            wins.as_slice(),
        );
    }

    #[test]
    fn mutating() {
        let mut input = vec![1, 2, 3, 4, 5, 6, 7, 8];
        UnsafeSlidingWindow::new(&mut input, 2).for_each(|win| {
            if win[0] == 3 {
                win[1] = 23;
            }
        });
        assert_eq!([1, 2, 3, 23, 5, 6, 7, 8], input.as_slice());
    }
}
