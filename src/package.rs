
use derive_more::{From, TryInto};
// use log::debug;

use crate::lex::Lit;


#[derive(Debug, Clone)]
pub struct Example {
	pub path: Box<str>,
	pub title: Option<Box<str>>,
	pub description: Option<Box<str>>,
}



// =============================================================================
// PACKAGE
// =============================================================================

#[derive(Debug, Default)]
#[repr(C)]
pub struct PkgCore {
	/// Name of the package.
	/// This is set after parsing.
	/// It always originates from the filename.
	pub name: Box<str>,
	/// The namespace the file was in. This will be the result of concatenating the parent directories,
	/// using `.` as a seperator.
	pub namespace: Box<str>,
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
}


#[derive(Debug, From, TryInto)]
#[repr(C, u8)]
pub enum EPkg {
	Class(ClassPkg),
	Function(FunctionPkg),
	Method(MethodPkg),
	Value(ValuePkg),
	Lib(LibPkg),
	Event(EventPkg),
	Operator(OperatorPkg)
}





impl EPkg {

	/// Gets a `mut ptr` to the `core` of each variant.
	/// This is done in a branchless manner.
	#[inline(always)]
	pub fn core_mut(&mut self) -> &mut PkgCore {
		// Safety: All variants have `repr(C)` and have `PkgCore` as their first field.
		// Add `8` to skip past the tag
		unsafe { &mut *(self as *mut EPkg as *mut PkgCore).byte_add(8) }
	}

	/// Gets a `const ptr` to the `core` of each variant.
	/// This is done in a branchless manner.
	#[inline(always)]
	pub fn core(&self) -> &PkgCore {
		// Safety: All variants have `repr(C)` and have `PkgCore` as their first field.
		// Add `8` to skip past the tag
		unsafe { &*(self as *const EPkg as *const PkgCore).byte_add(8) }
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
#[repr(C)]
pub struct FunctionPkg {
	pub core: PkgCore,
	pub args: Vec<FnArg>,
	pub rets: Vec<FnArg>,
}
impl FunctionPkg {
	pub fn stemmed_name(&self) -> &str {
		self.core.name.as_str()
			.trim_start_matches("get") 
			.trim_start_matches("set") 
			.trim_start_matches("mod")
			.trim_start_matches("is") 
			.trim_start_matches("has") 
			.trim_start_matches("can")
			.trim_start_matches("open") 
			.trim_start_matches("close")
			.trim_start_matches("add") 
			.trim_start_matches("remove")
			.trim_start_matches("enable") 
			.trim_start_matches("disable")
			.trim_start_matches("apply") 
			.trim_start_matches("update")
			.trim_start_matches("find") 
			.trim_start_matches("show")
			.trim_start_matches("create") 
			.trim_start_matches("delete")
			.trim_start_matches("test") 
			.trim_start_matches("toggle")
	}
}

#[derive(Debug)]
#[repr(C)]
pub struct MethodPkg {
	pub core: PkgCore,
	pub args: Vec<FnArg>,
	pub rets: Vec<FnArg>,
}
impl MethodPkg {
	pub fn stemmed_name(&self) -> &str {
		self.core.name.as_str()
			.trim_start_matches("get") 
			.trim_start_matches("set") 
			.trim_start_matches("mod")
			.trim_start_matches("is") 
			.trim_start_matches("has") 
			.trim_start_matches("can")
			.trim_start_matches("open") 
			.trim_start_matches("close")
			.trim_start_matches("add") 
			.trim_start_matches("remove")
			.trim_start_matches("enable") 
			.trim_start_matches("disable")
			.trim_start_matches("apply") 
			.trim_start_matches("update")
			.trim_start_matches("find") 
			.trim_start_matches("show")
			.trim_start_matches("create") 
			.trim_start_matches("delete")
			.trim_start_matches("test") 
			.trim_start_matches("toggle")
	}
}

// =============================================================================
// CLASSES
// =============================================================================

#[derive(Debug, Default)]
#[repr(C)]
pub struct ClassPkg {
	pub core: PkgCore,
	/// The type from which this type inherits should be passed here. This will allow the documentation builders to build the proper inheritance chains. For example, when a function accepts tes3mobileActor, because tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer have inherits = "tes3mobileActor", the docs will be built with tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer parameters for that function automatically. This saves you the job of figuring out the complete inheritance chains.
	pub inherits: Option<Box<str>>,
	/// This is a flag for types that can't be accessed normally. There are some types which inherit from abstract ones.
	pub is_abstract: bool, 
	/// The value packages used by this class.
	pub values: Vec<ValuePkg>,

	pub functions: Vec<FunctionPkg>,
	pub methods: Vec<MethodPkg>,
	pub ops: Vec<OperatorPkg>
}

impl ClassPkg {
	pub fn stemmed_name(&self) -> &str {
		self.core.name.as_str()
			.trim_start_matches("ni")
			.trim_start_matches("tes3ui")
			.trim_start_matches("tes3")
	}
}

// =============================================================================
// VALUES
// =============================================================================

#[derive(Debug)]
#[repr(C)]
pub struct ValuePkg {
	pub core: PkgCore,
	/// Is this read only?
	pub read_only: bool,
	/// The value type
	pub ty: Option<Box<str>>,
	/// The default value
	pub default: Option<Lit>,
}
#[derive(Debug)]
#[repr(C)]
pub struct OperatorPkg {
	pub core: PkgCore,
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

#[derive(Debug, Default)]
#[repr(C)]
pub struct LibPkg {
	pub core: PkgCore,
	/// External link
	pub link: Option<Box<str>>,
	///For libraries with sub-namespaces such as mwse.mcm, etc., this array contians the nested namespaces.
	pub sublibs: Vec<LibPkg>,
	pub functions: Vec<FunctionPkg>,
	pub values: Vec<ValuePkg>,
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
#[repr(C)]
pub struct EventPkg {
	pub core: PkgCore,
	/// Filter for this event
	pub filter: Option<Box<str>>,
	/// Is this event blockable?
	pub blockable: bool,
	pub event_data: Vec<EventDatum>,
	pub links: Vec<EventLink>,
	pub related: Vec<Box<str>>,
}

