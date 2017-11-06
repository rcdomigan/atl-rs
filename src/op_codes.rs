// Define a big old enum and evaluate a vector of them.  This doesn't
// seem like a particularly effecient approach, but I'm hoping it'll
// work.
use types::AtlFn;

#[derive(Debug, PartialEq)]
pub enum Op {
    Argument(usize),        // position (counting back)
    CallClosure,            // [arg0]..[argN][N][closure-address]
    ClosureArgument(usize), // position (counting forward)
    DefineGlobal(usize),    // slot [value]
    DerefFn(usize),         // slot
    Finish,
    Fixnum(usize),
    PlaceHolder,
    If,                        // [pc-of-alternate-branch][predicate-value]
    IndexArg(usize, usize),    // slot,index  get the element of a tuple argument
    Jump(usize),               // destination
    MakeClosure(usize, usize), // [carg]...[cargN] (body-pc, N)
    Pop,                       // [thing]
    Return,                    // [return-value]
    RustFunction(AtlFn),       // [arg1]...[argN][N]
    RustFunctionWrapped(AtlFn), // [arg1]...[argN][N][closure][old-frame][return-address]
    TailCall,                  // [arg0]..[argN][N][procedure-address]
}

// Wrap the instruction vector so we can maintain some invarients.
#[derive(Debug, PartialEq)]
pub struct BuildCode {
    code: Vec<Op>,
}

impl BuildCode {
    pub fn new() -> BuildCode {
        BuildCode {
            code: vec![Op::Jump(1)],
        }
    }

    // New code without the leading 'jump' placeholder.  Used for
    // tests.
    #[doc(hidden)]
    pub fn testing_new() -> BuildCode {
        BuildCode { code: vec![] }
    }

    // User must take care of invarients (starts with Jump to code
    // entry point)
    pub fn from_vec(code: Vec<Op>) -> Self {
        BuildCode { code }
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn push(&mut self, op: Op) {
        self.code.push(op)
    }

    // Set entry point (by modifying the starting Jump).
    pub fn set_enter(&mut self, pos: usize) {
        self.code[0] = Op::Jump(pos);
    }

    pub fn into_run(mut self) -> RunCode {
        self.code.push(Op::Finish);
        return RunCode { code: self.code };
    }

    // Access the state of the code for testing purposes
    pub fn view(&self) -> &Vec<Op> {
        &self.code
    }

    // Access the state of the code for testing purposes
    #[doc(hidden)]
    pub fn dump(self) -> Vec<Op> {
        self.code
    }
}

// Puts a placeholder in current position of 'code'.  When
// UsePosAtDrop is itself dropped the placeholder is replaced with
// current end-of-code position.  This is handy for setting jumps
// which will skip over the section of code currently being defined
// (ie for If blocks and function bodies).
pub struct UsePosAtDrop<'a, F>
where
    F: Fn(usize) -> Op,
{
    pub code: &'a mut BuildCode,
    back_patch_location: usize,
    back_patch: F,
}

impl<'a, F> UsePosAtDrop<'a, F>
where
    F: Fn(usize) -> Op,
{
    pub fn new(code: &'a mut BuildCode, back_patch: F) -> UsePosAtDrop<'a, F> {
        let back_patch_location = code.len();
        code.push(Op::PlaceHolder);

        UsePosAtDrop {
            code: code,
            back_patch_location: back_patch_location,
            back_patch: back_patch,
        }
    }

    // Returns a closure that creates a UsePosAtDrop instance later
    // (so I can delay taking ownership.  This handles over-lapping
    // skipped blocks on the same BuildCode).
    pub fn will_patch(code: &'a mut BuildCode) -> Box<Fn(&mut BuildCode, F) -> UsePosAtDrop<F>> {
        let back_patch_location = code.len();
        code.push(Op::PlaceHolder);

        Box::new(move |code, back_patch| UsePosAtDrop {
            code: code,
            back_patch_location: back_patch_location,
            back_patch: back_patch,
        })
    }
}

impl<'a, F> Drop for UsePosAtDrop<'a, F>
where
    F: Fn(usize) -> Op,
{
    fn drop(&mut self) {
        let position = self.code.len();
        self.code.code[self.back_patch_location] = (self.back_patch)(position);
    }
}

pub struct RunCode {
    code: Vec<Op>,
}

impl RunCode {
    // User must maintain invarients (starts with Jump to entry point
    // and ends with Op::Finished)
    pub fn from_vec(code: Vec<Op>) -> Self {
        RunCode { code }
    }

    pub fn into_build(mut self) -> BuildCode {
        self.code.pop();
        BuildCode { code: self.code }
    }

    pub fn view(&self) -> &Vec<Op> {
        &self.code
    }
}
