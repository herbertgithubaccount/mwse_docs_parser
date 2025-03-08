use std::path::Path;

use derive_more::{From, TryInto};
use log::debug;

use crate::{error::Error, lex::{self, Lit}, FromTokens, ParseErr};


// =============================================================================
// PACKAGE
// =============================================================================

#[derive(Debug)]
#[repr(C)]
pub struct Pkg<T> {
	/// Name of the package.
	/// This is set after parsing.
	/// It always originates from the filename.
	pub name: Box<str>,
	/// Description of the package.
	pub description: Option<Box<str>>,
	/// Is this part of the experimental API? Default: `false`.
	pub experimental: bool,

	/// Allows marking definitions as deprecated. Those definitions aren't written to the web documentation.
	pub deprecated: bool,

	/// Flag that determines whether a class is abstract.
	/// This will be `false` unless this `PkgCore` belongs to a `ClassPkg`
	/// This is stored here in order to minimize the size of `ClsPkg`
	

	///A table containing the examples. Keys are the example's name/path to the example file.
	pub examples:  Option<Vec<Example>>,

	pub ty: T,

}



#[derive(Debug, Clone)]
pub struct Example {
	pub path: Box<str>,
	pub title: Option<Box<str>>,
	pub description: Option<Box<str>>,
}



#[derive(Debug, From, TryInto)]
#[repr(C, u8)]
pub enum EPkg {
	Class(Pkg<Class>),
	Function(Pkg<Function>),
	Method(Pkg<Method>),
	Value(Pkg<Value>),
	Lib(Pkg<Lib>),
	Event(Pkg<Event>),
	Operator(Pkg<Operator>)
}


impl EPkg {

	/// Gets a `mut ptr` to the first field of each variant
	#[inline(always)]
	unsafe fn mut_ptr(&mut self) -> *mut Box<str> {
		// Add `8` to skip past the tag
		unsafe { (self as *mut EPkg as *mut Box<str>).byte_add(8) }
	}
	/// Gets a `const ptr` to the first field of each variant
	#[inline(always)]
	unsafe fn ptr(&self) -> *const Box<str> {
		// Add `8` to skip past the tag
		unsafe { (self as *const EPkg as *const Box<str>).byte_add(8) }
	}

	pub fn set_name(&mut self, new_name: Box<str>) {
		// match self {
		// 	EPkg::Class(pkg) => pkg.name = new_name,
		// 	EPkg::Function(pkg) => pkg.name = new_name,
		// 	EPkg::Method(pkg) => pkg.name = new_name,
		// 	EPkg::Value(pkg) => pkg.name = new_name,
		// 	EPkg::Lib(pkg) => pkg.name = new_name,
		// 	EPkg::Event(pkg) => pkg.name = new_name,
		// 	EPkg::Operator(pkg) => pkg.name = new_name,
		// }
		debug!("self = {self:?}\n\n\n");
		unsafe {
			*self.mut_ptr() = new_name;
		}
		debug!("updated name... = {self:?}\n\n\n");
	}
	pub fn name(&self) -> &str {
		// match self {
		// 	EPkg::Class(pkg) => &pkg.name,
		// 	EPkg::Function(pkg) => &pkg.name,
		// 	EPkg::Method(pkg) => &pkg.name,
		// 	EPkg::Value(pkg) => &pkg.name,
		// 	EPkg::Lib(pkg) => &pkg.name,
		// 	EPkg::Event(pkg) => &pkg.name,
		// 	EPkg::Operator(pkg) => &pkg.name,
		// }
		unsafe {
			(*self.ptr()).as_str()
		}
	}
}




// =============================================================================
// FUNCTIONS AND METHODS
// =============================================================================

/// Stores an argument / return value of a function.
#[derive(Debug)]
pub struct FnArg {
	pub name: Option<Box<str>>,
	pub ty: Option<Box<str>>,
	pub optional: bool,
	pub description: Option<Box<str>>,
	pub default: Option<Lit>,
	pub table_params: Option<Vec<FnArg>>,
}

#[derive(Debug)]
pub struct Function {
	pub args: Vec<FnArg>,
	pub rets: Vec<FnArg>,
}

#[derive(Debug)]
pub struct Method {
	pub args: Vec<FnArg>,
	pub rets: Vec<FnArg>,
}

// =============================================================================
// CLASSES
// =============================================================================

#[derive(Debug, Default)]
pub struct Class {
	/// The type from which this type inherits should be passed here. This will allow the documentation builders to build the proper inheritance chains. For example, when a function accepts tes3mobileActor, because tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer have inherits = "tes3mobileActor", the docs will be built with tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer parameters for that function automatically. This saves you the job of figuring out the complete inheritance chains.
	pub inherits: Option<Box<str>>,
	/// This is a flag for types that can't be accessed normally. There are some types which inherit from abstract ones.
	pub is_abstract: bool, 
	/// The value packages used by this class.
	pub values: Vec<Pkg<Value>>,

	pub functions: Vec<Pkg<Function>>,
	pub methods: Vec<Pkg<Method>>,
}

// =============================================================================
// VALUES
// =============================================================================

#[derive(Debug)]
pub struct Value{
	/// Is this read only?
	pub read_only: bool,
	/// The value type
	pub ty: Option<Box<str>>,
	/// The default value
	pub default: Option<Lit>,
}
#[derive(Debug)]
pub struct Operator {
	pub overloads: Vec<Overload>
}


/// Stores something like
/// ```lua
/// { rightType = "niColor", resultType = "niColor", description = "Adds the color channel values of two `niColor` objects." },
/// ```
#[derive(Debug)]
pub struct Overload {
	pub right_ty: Option<Box<str>>,
	pub result_ty: Option<Box<str>>,
	pub description: Option<Box<str>>,
}


// =============================================================================
// LIBRARIES
// =============================================================================

#[derive(Debug)]
pub struct Lib {
	/// External link
	pub link: Option<Box<str>>,
	///For libraries with sub-namespaces such as mwse.mcm, etc., this array contians the nested namespaces.
	pub sublibs: Option<Vec<Pkg<Lib>>>,
	// pub rets: Vec<FnArg>,
}



// =============================================================================
// EVENTS
// =============================================================================

#[derive(Debug)]
pub struct EventDatum {
	pub name: Box<str>,
	pub ty: Option<Box<str>>,
	pub read_only: bool,
	pub optional: bool,
	pub description: Option<Box<str>>,
	// should be an owned literal
	pub default: Option<Lit>,
}



#[derive(Debug)]
pub struct EventLink {
	pub name: Box<str>,
	pub path: Box<str>,
}


#[derive(Debug)]
pub struct Event {
	/// Filter for this event
	pub filter: Option<Box<str>>,
	/// Is this event blockable?
	pub blockable: bool,
	pub event_data: Vec<EventDatum>,
	pub links: Vec<EventLink>,
	pub related: Vec<Box<str>>,
}

