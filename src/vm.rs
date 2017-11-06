use op_codes::{Op, RunCode};
use std::mem::swap;
use types::VMState;

static BODY_PC_OFFSET: usize = 1;
// static CAPTURE_COUNT_OFFSET: usize = 0;
static CAPTURE_ARGS_OFFSET: usize = 2;

#[derive(Debug)]
pub struct VM {
    pub state: VMState,
}

// Take several additional parameters so we can start testing on a preset stack etc.
fn run_impl(mut state: VMState, wrapped_code: &RunCode) -> VMState {
    let mut pc = 0;
    let code = wrapped_code.view();

    loop {
        println!("{:?} {:?}", state.stack, code[pc]);
        match code[pc] {
            // Get the `offset` argument counting backwards from the
            // top frame on call_stack.
            Op::Argument(offset) => {
                let value = state.stack[state.enclosing_frame - offset - 1];
                state.stack.push(value);
                pc += 1;
            }
            // Assume the top of the env.stack is a closure and call it
            // Pre call stack:
            //   [arg1]...[argN][N][closure]
            //                              ^- top
            // Post call:
            //   [arg1]...[argN][N][closure][old-frame][return-address]
            //                  ^                                 top -^
            //                  ^- enclosing_frame
            Op::CallClosure => {
                let top = state.stack.len();
                let closure_base = state.stack[top - 1]; // closure

                state.stack.push(state.enclosing_frame); // old-call-stack
                state.enclosing_frame = top - 2;

                state.stack.push(pc + 1); // return address

                pc = state.store[closure_base + BODY_PC_OFFSET];
            }
            // Gets the `arg-offset` value from this frame's closure.
            // pre:
            //   [arg1]...[argN][N][closure] ...
            //                  ^           top -^
            //                  ^- enclosing_frame
            Op::ClosureArgument(offset) => {
                let closure = state.stack[state.enclosing_frame + 1];
                state
                    .stack
                    .push(state.store[closure + CAPTURE_ARGS_OFFSET + offset]);
                pc += 1;
            }
            // Define a global slot and put a value in it.
            //
            // Pre call:
            //  [value]
            // Post call:
            Op::DefineGlobal(slot) => {
                let value = state
                    .stack
                    .pop()
                    .expect("Expected to pop something into the slot");
                if state.slots.len() <= slot + 1 {
                    state.slots.resize(slot + 1, 0);
                }
                state.slots[slot] = value;
                pc += 1;
            }
            // Pre call:
            // Post call:
            //    [value-from-slot]
            Op::DerefFn(slot) => {
                state.stack.push(state.slots[slot]);
                pc += 1;
            }
            Op::Finish => break,
            Op::Fixnum(value) => {
                state.stack.push(value);
                pc += 1;
            }
            Op::PlaceHolder => panic!("PlaceHolder!!"),
            // Pre call:
            //   [][alternate-instruction][predicate]
            //                                       ^- top
            // Post call:
            //   []
            //     ^- top
            Op::If => {
                let new_len = state.stack.len() - 2;

                let mut drain = state.stack.drain(new_len..);
                let alt_pos = drain.next().expect("no alternate!");
                let pred = drain.next().expect("no predicate!");

                if pred == 0 {
                    pc = alt_pos;
                } else {
                    pc += 1;
                }
            }
            Op::IndexArg(offset, idx) => {
                let tuple_base = state.stack[state.enclosing_frame - offset - 1];
                let value = state.store[tuple_base + idx];
                state.stack.push(value);
                pc += 1;
            }
            Op::Jump(pos) => {
                pc = pos;
            }
            // Pre call stack:
            //   [capture-arg1]...[capture-arg_]
            //                             top -^
            // Post call stack:
            //   [pointer-to-closure]
            //                  top -^
            Op::MakeClosure(body_pc, capture_count) => {
                let new_top = state.stack.len() - capture_count;

                let closure = state.store.new_closure(
                    body_pc,
                    &state.stack[new_top..],
                );
                state.stack.truncate(new_top);

                state.stack.push(closure);

                pc += 1;
            }
            Op::Pop => {
                state
                    .stack
                    .pop()
                    .expect("expected something on the stack to pop!");
                pc += 1;
            }
            // [arg0]...[argN][N][closure][old-frame][return-address]...[return-value]
            //                ^- enclosing_frame                                 top -^
            // [return-value]
            //               ^- top
            Op::Return => {
                let num_args = state.stack[state.enclosing_frame];
                let result = state.stack.pop().expect("Expected result");
                pc = state.stack[state.enclosing_frame + 3];

                let new_top = state.enclosing_frame - num_args;

                state.enclosing_frame = state.stack[state.enclosing_frame];

                state.stack.truncate(new_top);
                state.stack.push(result);
            }
            // [arg1]...[argN][N][pointer to std::function]
            Op::RustFunction(ref func) => {
                let arity = state.stack.pop().expect("expeted function's arity");
                let start = state.stack.len() - arity;
                let args: Vec<_> = state.stack.drain(start..).collect();
                state.stack.push(func.0(&mut state.store, &args));
                pc += 1;
            }
            // Calls a rust function from the stack position of a
            // closure-body.  This lets me use a thinner wrapper
            // around Rust functions in function-as-value situations
            // where the VM will have to apply some closure.
            //
            // Pre call:
            //   [arg1]...[argN][N][closure][old-frame][return-address]
            //                  ^                                 top -^
            //                  ^- enclosing_frame
            // Post call:
            //   [return-value]
            //                 ^- top
            Op::RustFunctionWrapped(ref func) => {
                let num_args = state.stack[state.enclosing_frame];
                pc = state.stack[state.enclosing_frame + 3];
                let old_frame = state.stack[state.enclosing_frame + 2];

                state.stack.truncate(state.enclosing_frame);

                let start = state.enclosing_frame - num_args;
                let args: Vec<_> = state.stack.drain(start..).collect();

                state.stack.push(func.0(&mut state.store, &args));
                state.enclosing_frame = old_frame;
            }
            // Replace the current function's stack frame with arg0-argN
            // from the top of the stack, and re-use it when calling
            // `procedure-address`
            //
            // Pre call stack:
            //   [args-old]
            //   [M-old] <- enclosing_frame
            //   [closure-old]
            //   [frame]
            //   [return]
            //   ...
            //   [args]
            //   [M]
            //   [cargs]
            //   [closure] <- top
            // Post call:
            //   [args][N][closure][frame][return]
            //         ^- enclosing_frame         ^-top
            Op::TailCall => {
                let top = state.stack.len();
                let old_n = state.stack[state.enclosing_frame];

                let closure = state.stack[top - 1];
                let body_pc = state.store[closure + BODY_PC_OFFSET];
                let frame = state.stack[state.enclosing_frame + 2];
                let ret_pc = state.stack[state.enclosing_frame + 3];

                let n = state.stack[top - 2];

                let old_begin = state.enclosing_frame - old_n;
                let new_begin = top - n - 2;

                for idx in 0..n {
                    state.stack[old_begin + idx] = state.stack[new_begin + idx];
                }

                state.stack.truncate(old_begin + n);

                state.stack.push(n);
                state.stack.push(closure);
                state.stack.push(frame);
                state.stack.push(ret_pc);

                pc = body_pc;
            }
        }
    }
    state
}

impl VM {
    pub fn new() -> VM {
        VM {
            state: VMState::new(),
        }
    }

    pub fn from_state(state: VMState) -> VM {
        VM { state }
    }

    // Pops the top of the stack
    pub fn result(&mut self) -> Option<usize> {
        self.state.stack.pop()
    }

    pub fn peek(&self) -> Option<usize> {
        if let Some(val) = self.state.stack.last() {
            return Some(*val);
        }
        None
    }

    pub fn clear_stack(&mut self) {
        self.state.stack.clear()
    }

    // Environment used to size the VM's slots table
    pub fn run(&mut self, code: &RunCode) -> &mut Self {
        let mut tmp = VMState::new();
        swap(&mut tmp, &mut self.state);
        self.state = run_impl(tmp, code);
        return self;
    }
}

#[cfg(test)]
mod tests {
    use op_codes::RunCode;
    use store::Store;
    use types::{AtlFn, VMState};
    use vm::{run_impl, Op, VM};

    pub fn sub2(_store: &mut Store, args: &[usize]) -> usize {
        assert_eq!(args.len(), 2);
        args[0] - args[1]
    }

    #[test()]
    fn running_rust_function() {
        use self::sub2;
        use self::Op::{Finish, Fixnum, RustFunction};

        let code = vec![
            Fixnum(7),
            Fixnum(3),
            Fixnum(2),
            RustFunction(AtlFn(sub2)),
            Finish,
        ];

        let mut vm = VM::new();
        assert_eq!(vm.run(&RunCode::from_vec(code)).result(), Some(4));
    }

    #[test()]
    fn call_wrapped_primitive() {
        use self::sub2;
        use self::Op::{CallClosure, DerefFn, Finish, Fixnum, Jump, RustFunctionWrapped};

        let mut vm = VM::new();
        vm.state.store.new_closure(1, &[]);
        vm.state.slots.push(0);

        let code = vec![
            Jump(2),                          // 0:
            RustFunctionWrapped(AtlFn(sub2)), // 1:
            Fixnum(7),                        // 2:
            Fixnum(3),                        // 3:
            Fixnum(2),                        // 4:
            DerefFn(0),                       // 5:
            CallClosure,                      // 6:
            Finish,                           // 7:
        ];

        assert_eq!(vm.run(&RunCode::from_vec(code)).result(), Some(4));
    }

    #[test()]
    fn if_true() {
        use self::Op::{Finish, Fixnum, If, Jump};

        fn do_it(pred: Op, expect: Option<usize>) {
            let code = vec![
                Fixnum(5), // 0: alternate start
                pred,      // 1: predicate
                If,        // 2:
                Fixnum(1), // 3: consequent value
                Jump(6),   // 4: jump to end
                Fixnum(2), // 5: alternate value
                Finish,    // 6:
            ];
            let mut vm = VM::new();
            assert_eq!(vm.run(&RunCode::from_vec(code)).result(), expect);
        }

        do_it(Fixnum(0), Some(2));

        do_it(Fixnum(1), Some(1));
    }

    #[test()]
    fn return_value() {
        let code = vec![Op::Return, Op::Finish];

        let mut env = VMState::from_stack(vec![
            11, // 0: garbage
            3,  // 1: arg 0
            5,  // 2: arg 1
            2,  // 3: arg-count <--
            3,  // 4: closure
            0,  // 5: old_frame
            1,  // 6: next PC
            7,  // 7: value to return
        ]);
        env.enclosing_frame = 3;
        assert_eq!(run_impl(env, &RunCode::from_vec(code)).stack, vec![11, 7]);
    }

    #[test()]
    fn trivial_call_closure() {
        // ((\ (a b) (sub a b)) 5 3)
        let code = vec![
            Op::Fixnum(5),                 // 0:
            Op::Fixnum(3),                 // 1:
            Op::Fixnum(2),                 // 2: arg count
            Op::Fixnum(0),                 // 3: closure
            Op::CallClosure,               // 4:
            Op::Finish,                    // 5:
            Op::Argument(1),               // 6: closure start
            Op::Argument(0),               // 7:
            Op::Fixnum(2),                 // 8:
            Op::RustFunction(AtlFn(sub2)), // 9:
            Op::Return,                    // 10:
        ];

        let mut vm = VM::new();
        // (body, capture-count)
        vm.state.store.new_closure(6, &[]);
        vm.run(&RunCode::from_vec(code));

        assert_eq!(vm.state.stack, vec![2]);
    }

    #[test()]
    fn make_simple_closure() {
        // (\ (a b) (sub a b))
        let code = vec![
            Op::Jump(6),                   // 0:
            Op::Argument(0),               // 1: body
            Op::Argument(1),               // 2:
            Op::Fixnum(2),                 // 3:
            Op::RustFunction(AtlFn(sub2)), // 4:
            Op::Return,                    // 5:
            Op::MakeClosure(1, 0),         // 8:
            Op::Finish,                    // 9:
        ];

        let mut vm = VM::new();
        vm.run(&RunCode::from_vec(code));
        assert_eq!(vm.state.stack, vec![0]);

        // just [closure-id]
        assert_eq!(vm.state.store.test_view().len(), 2);
    }

    #[test()]
    fn make_capturing_closure() {
        use self::Op;

        //  ((\ (a b) (sub a b)) 5 3)
        let code = vec![
            Op::Fixnum(1),         // 0:
            Op::Fixnum(3),         // 1:
            Op::Jump(4),           // 2:
            Op::Return,            // 3:
            Op::MakeClosure(0, 2), // 6:
            Op::Finish,
        ];

        let mut vm = VM::new();
        vm.run(&RunCode::from_vec(code));
        assert_eq!(vm.state.stack, vec![0]);

        // [closure-id][bound0][bound1]
        assert_eq!(vm.state.store.test_view(), &vec![3, 0, 1, 3]);
    }

    #[test()]
    fn make_and_call_closure() {
        //  ((\ (a b) (sub a b)) 5 3)
        let code = vec![
            Op::Fixnum(8),                 // 0:
            Op::Fixnum(5),                 // 1:
            Op::Fixnum(2),                 // 2: arg count
            Op::Jump(9),                   // 3:
            Op::Argument(1),               // 4: body
            Op::Argument(0),               // 5:
            Op::Fixnum(2),                 // 6:
            Op::RustFunction(AtlFn(sub2)), // 7:
            Op::Return,                    // 8:
            Op::MakeClosure(4, 0),         // 9:
            Op::CallClosure,               // 10:
            Op::Finish,                    // 11:
        ];

        let mut vm = VM::new();
        assert_eq!(vm.run(&RunCode::from_vec(code)).result(), Some(3));
    }

    #[test()]
    fn fake_nested_closure() {
        // Make a body for our fake closure
        let code = vec![
            Op::ClosureArgument(0),        // 0:
            Op::ClosureArgument(1),        // 1:
            Op::Fixnum(2),                 // 2: args
            Op::RustFunction(AtlFn(sub2)), // 3:
            Op::Finish,                    // 4:
        ];

        // fake up the stack so our function can exit
        let mut state = VMState::from_stack(vec![
            0, // 0: args
            0, // 1: closure
            0, // 2: enclosing frame
            3, // 3: return address
        ]);
        state.store.new_closure(0, &[8, 5]);

        state = run_impl(state, &RunCode::from_vec(code));

        assert_eq!(
            state.stack,
            vec![
                0, // args    <--(enclosing-frame)
                0, // closure
                0, // enclosing frame
                3, // return address
                3, // return value
            ]
        );
    }

    // Tests the same expression as compile::tests::make_and_call_closure
    #[test()]
    fn make_and_call_nested_closure() {
        // (((\fn 1\ (a) (\fn 0\ (b) (sub2 a b))) 5) 2)
        let code = vec![
            Op::Fixnum(2),
            Op::Fixnum(1),
            Op::Fixnum(5),
            Op::Fixnum(1),
            Op::Jump(14),
            Op::Jump(11),
            Op::ClosureArgument(0),
            Op::Argument(0),
            Op::Fixnum(2),
            Op::RustFunction(AtlFn(sub2)),
            Op::Return,
            Op::Argument(0),
            Op::MakeClosure(6, 1),
            Op::Return,
            Op::MakeClosure(5, 0),
            Op::CallClosure,
            Op::CallClosure,
            Op::Finish,
        ];

        let mut vm = VM::new();
        assert_eq!(vm.run(&RunCode::from_vec(code)).result(), Some(3));
    }

    // The way a tail call works (ATM) is to place the call's arguments on
    // the top of the stack, then move them down the stack to over-write
    // its caller's stack.
    #[test()]
    fn move_n() {
        let code = vec![
            Op::Fixnum(3), // 0:
            Op::Fixnum(4), // 1:
            Op::Fixnum(2), // 2: arg-N
            Op::Fixnum(0), // 3: closure
            Op::TailCall,  // 4:
            Op::Finish,    // 5:
        ];

        let mut vm = VM::new();
        vm.state.store.new_closure(5, &[]);
        vm.state.stack = vec![
            1,   // 0:
            2,   // 1:
            2,   // 2: N-args
            0,   // 3: old closure (discarded)
            100, // 4: return pos (body exits fist)
        ];
        vm.state.enclosing_frame = 2;
        vm.run(&RunCode::from_vec(code));
        assert_eq!(vm.state.stack, vec![3, 4, 2, 0, 100, 3]);
    }

    #[test()]
    fn make_tuple() {
        use tuple::make_tuple;

        let code = vec![
            Op::Fixnum(3),                       // 0:
            Op::Fixnum(2),                       // 1:
            Op::Fixnum(2),                       // 2:
            Op::RustFunction(AtlFn(make_tuple)), // 3:
            Op::Finish,                          // 4:
        ];

        let mut vm = VM::new();
        vm.run(&RunCode::from_vec(code));

        assert_eq!(vm.state.stack, vec![0]);
        assert_eq!(vm.state.store.test_view(), &vec![3, 2]);
    }

    #[test()]
    fn destructure_tuple() {
        use tuple::make_tuple;

        let code = vec![
            Op::Jump(3),                         // 0:
            Op::IndexArg(0, 0),                  // 1:
            Op::Return,                          // 2:
            Op::MakeClosure(1, 0),               // 3: We don't need to store this address since
            Op::Pop,                             // 4: it's a toplevel function.
            Op::Fixnum(3),                       // 5:
            Op::Fixnum(2),                       // 6:
            Op::Fixnum(2),                       // 7:
            Op::RustFunction(AtlFn(make_tuple)), // 8:
            Op::Fixnum(1),                       // 9:  <- arg count
            Op::Fixnum(0),                       // 10: <- closure data pointer
            Op::CallClosure,                     // 11:
            Op::Finish,                          // 12:
        ];

        let mut vm = VM::new();
        vm.run(&RunCode::from_vec(code));

        assert_eq!(vm.state.stack, vec![3]);
    }
}
