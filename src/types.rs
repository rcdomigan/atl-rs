use std::fmt;
use std::boxed::Box;
use std::rc::Rc;
use std::vec::Vec;
use store::Store;

pub type Stack = Vec<usize>;

#[derive(Debug, Clone)]
pub struct VMState {
    pub store: Store,
    pub stack: Stack,
    pub slots: Vec<usize>,
    pub enclosing_frame: usize,
}

impl VMState {
    pub fn new() -> VMState {
        VMState {
            store: Store::new(),
            stack: Vec::new(),
            enclosing_frame: 0,
            slots: Vec::new(),
        }
    }

    pub fn from_stack(stack: Stack) -> VMState {
        VMState {
            stack,
            store: Store::new(),
            enclosing_frame: 0,
            slots: Vec::new(),
        }
    }
}

pub type AtlFnSig = fn(&mut Store, &[usize]) -> usize;

// NewType since #derived traits don't work for function pointers
// which accept references.
#[derive(Clone)]
pub struct AtlFn(pub AtlFnSig);

impl fmt::Debug for AtlFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0 as usize)
    }
}

impl PartialEq for AtlFn {
    fn eq(&self, other: &Self) -> bool {
        self.0 as usize == other.0 as usize
    }
}


pub type Slot = usize;


#[derive(PartialEq, Debug, Clone)]
pub enum Any {
    AstData(usize),
    Bool(bool),
    ClosureParameter(usize),
    DeclareType,
    Define(usize),
    DefineMacro,
    Deref(usize),
    Exit,
    Fixnum(usize),
    If,
    IndexParam(usize, usize),
    Lambda(Box<ClosureMetadata>),
    Parameter(usize),
    PlaceHolder,
    RustFunction(AtlFn, Slot),
    Symbol(Rc<SymbolMetadata>),
    Tuple,
}

pub type Ast = Vec<Any>;

impl ContextManager for Ast {
    type Witness = usize;
    fn enter(&mut self) -> usize {
        let pos = self.len();
        self.push(Any::AstData(0));
        pos
    }

    fn exit(&mut self, pos: usize) {
        self[pos] = Any::AstData(self.len() - pos - 1);
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct SymbolMetadata {
    pub name: String,
}

impl SymbolMetadata {
    pub fn new(name: &str) -> SymbolMetadata {
        SymbolMetadata {
            name: name.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClosureMetadata {
    pub body: usize,
    pub formals_count: usize,
    pub captured: Vec<Any>,
}

impl ClosureMetadata {
    pub fn new() -> ClosureMetadata {
        ClosureMetadata {
            body: 0,
            formals_count: 0,
            captured: vec![],
        }
    }

    // Parameters are counted left to right, but Argument offsets go
    // right to left to save some arithmatic in the VM (since offset 0
    // is next to the frame pointer)
    pub fn argument_index(&self, parameter_index: usize) -> usize {
        self.formals_count - parameter_index - 1
    }
}

pub fn symbol(name: &str) -> Any {
    Any::Symbol(Rc::new(SymbolMetadata::new(name.clone())))
}

pub fn lambda(formals_count: usize) -> Any {
    Any::Lambda(Box::new(ClosureMetadata {
        body: 0,
        formals_count,
        captured: vec![],
    }))
}

pub fn wrap_fn(func: AtlFnSig, slot: Slot) -> Any {
    Any::RustFunction(AtlFn(func), slot)
}

pub trait ContextManager {
    type Witness;

    // Return a (possibly new) ContextManager for the inner context.
    fn enter(&mut self) -> Self::Witness;

    // Consume the inner context
    fn exit(&mut self, witness: Self::Witness);
}

pub fn nest<Managed, Err, Func>(mut context: Managed, func: Func) -> Result<Managed, Err>
where
    Func: Fn(Managed) -> Result<Managed, Err>,
    Managed: ContextManager,
{
    let witness = context.enter();
    context = func(context)?;
    context.exit(witness);
    Ok(context)
}

#[macro_export]
macro_rules! nest_attr {
    ( $instance: ident$(.$attr: ident)* , $func: expr ) => {{
        let witness = $instance.$( $attr. )*enter();
        $instance = $func($instance)?;
        $instance.$( $attr. )*exit(witness);
        Ok($instance)
    }}
}
