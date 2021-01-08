use crate::*;
use std::convert::TryFrom;
use std::convert::TryInto;

/**
 * Stores the list of global and local variables, and where they get their values from.
 * Globals initially have "unknown" state (represented as Complex), and when we call a function, all globals will have their state reset to Complex.
 * They can get their value from another global/local, or a constant.
 * When a variable is modified, all variables that reference it will get "decoupled".  This is implemented by assigning indices to the updates in increasing order.
 * Each local variable also stores its narrowest allowed type, so when the lifetime ends, we can update the local variable type.
 * The type of a global is never changed.
 * Note: PrimFuncs are not considered constants here, there is a separate optimisation pass just to devirtualize/propagate through a PrimFunc.
 * Clonability is required for branches (e.g. if/else and break).
 */
#[derive(Clone)]
pub struct VarContext{
	globals: Box<[VarState]>,
	locals: Vec<LocalVarState>,
	next_update_time: usize,
}

#[derive(Clone)]
struct LocalVarState {
	state: VarState,
	union: VarType, // the narrowest type allowed by this variable throughout its lifetime
}

/**
 * The type of this variable and where it got its value from (if from another variable or a constant).
 */
#[derive(Clone)]
struct VarState {
	pub vartype: VarType, // type of the current value of the variable, could be Any.  Defaults to the declared vartype of the variable.
	pub source: Option<Reference>, // if set, then it comes from an existing variable or constant.  otherwise, it is some complex (uninferable) thing.
	pub update_time: usize, // a monotonic "time" where this variable was last updated
}

#[derive(Clone)]
pub enum Reference {
	Constant(Constant), // some kind of constant, only PrimUndefined, PrimNumber, PrimBoolean, PrimString, PrimStructT allowed (not PrimFunc!).
	Variable(VariableRef), // a global or local variable
}

#[derive(Clone)]
pub enum Constant {
	PrimUndefined,
	PrimUnassigned,
	PrimNumber{val: f64},
	PrimBoolean{val:bool},
	PrimString{val:String},
	PrimStructT{typeidx: usize},
}

#[derive(Copy, Clone)]
pub enum VariableRef {
	Global{globalidx: usize},
	Local{localidx: usize},
}


impl VarContext {
	pub fn new_with_globals_and_params(globals: &[VarType], params: &[VarType]) -> Self {
		Self {
			globals: globals.iter().copied().map(|vartype| VarState{
				vartype,
				source: None,
				update_time: 0,
			}).collect(),
			locals:params.iter().copied().map(|vartype| LocalVarState{state:VarState{
				vartype,
				source: None,
				update_time: 0,
			},union:vartype}).collect(),
			next_update_time: 1,
		}
	}
	pub fn with_variable<R, F: FnOnce(&mut Self) -> R>(&mut self, vartype:VarType, source: Option<Reference>, f: F) -> R {
		unimplemented!();
	}
	pub fn update_variable(&mut self, variable_ref: VariableRef,  vartype:VarType, source: Option<Reference>) {
		unimplemented!();
	}
	pub fn get_type_and_try_get_reference(&self, variable_ref: VariableRef) -> (VarType, Option<Reference>) {
		unimplemented!();
	}
}

impl TryFrom<&ExprKind> for Reference {
    type Error = ();
    fn try_from(value: &ExprKind) -> Result<Self, Self::Error> {
		match value {
			ExprKind::PrimUndefined => Ok(Reference::Constant(Constant::PrimUndefined)),
			ExprKind::PrimNumber{val} => Ok(Reference::Constant(Constant::PrimNumber{val: *val})),
			ExprKind::PrimBoolean{val} => Ok(Reference::Constant(Constant::PrimBoolean{val: *val})),
			ExprKind::PrimString{val} => Ok(Reference::Constant(Constant::PrimString{val: *val})),
			ExprKind::PrimStructT{typeidx} => Ok(Reference::Constant(Constant::PrimStructT{typeidx: *typeidx})),
			ExprKind::VarName{source} => Ok(Reference::Variable(source.try_into()?)),
			_ => Err(()),
		}
	}
}

impl TryFrom<&TargetExpr> for VariableRef {
    type Error = ();
    fn try_from(value: &TargetExpr) -> Result<Self, Self::Error> {
		match value {
			TargetExpr::Global{globalidx, next} => match next {
				Some(_) => Err(()),
				None => Ok(VariableRef::Global{globalidx: *globalidx}),
			}
			TargetExpr::Local{localidx, next} => match next {
				Some(_) => Err(()),
				None => Ok(VariableRef::Local{localidx: *localidx}),
			}
		}
	}
}

impl From<(VarType, Reference)> for Expr {
	fn from(value: (VarType, Reference)) -> Self {
		unimplemented!();
	}
}