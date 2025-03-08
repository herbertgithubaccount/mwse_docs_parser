use std::path::Path;

use crate::{error::Error, lex::{self, Lit}, FromTokens, ParseErr};


// =============================================================================
// PACKAGE
// =============================================================================


/// Core fields that are present in every package
#[derive(Debug, Default)]
#[repr(C)]
pub struct PkgCore {
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
	///This is a flag for types that can't be accessed normally. There are some types which inherit from abstract ones.
	pub is_abstract: bool, 

	///A table containing the examples. Keys are the example's name/path to the example file.
	pub examples:  Option<Vec<Example>>,

	/// The package this package is a child of.
	/// This is set after parsing.
	pub parent: Option<Box<str>>,
}



#[derive(Debug, Clone)]
pub struct Example {
	pub path: Box<str>,
	pub title: Option<Box<str>>,
	pub description: Option<Box<str>>,
}



#[derive(Debug)]
pub enum EPkg {
	Class(ClassPkg),
	Function(FnPkg),
	Method(MethodPkg),
	Value(ValuePkg),
	Lib(LibPackage),
	Event(EventPackage),
	Operator(PackageOperator)
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
pub struct FnPkg {
	/// Stores information about the package, such as its name and description.
	pub core: PkgCore,
	pub args: Vec<FnArg>,
	pub rets: Vec<FnArg>,
}

#[derive(Debug)]
pub struct MethodPkg {
	/// Stores information about the package, such as its name and description.
	pub core: PkgCore,
	pub args: Vec<FnArg>,
	pub rets: Vec<FnArg>,
}

// =============================================================================
// CLASSES
// =============================================================================

#[derive(Debug, Default)]
pub struct ClassPkg {
	/// Stores information about the package, such as its name and description.
	pub core: PkgCore,
	/// The type from which this type inherits should be passed here. This will allow the documentation builders to build the proper inheritance chains. For example, when a function accepts tes3mobileActor, because tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer have inherits = "tes3mobileActor", the docs will be built with tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer parameters for that function automatically. This saves you the job of figuring out the complete inheritance chains.
	pub inherits: Option<Box<str>>,
	/// The value packages used by this class.
	pub values: Vec<ValuePkg>,

	pub functions: Vec<FnPkg>,
	pub methods: Vec<MethodPkg>,
}

// =============================================================================
// VALUES
// =============================================================================

#[derive(Debug)]
#[repr(C)]
pub struct ValuePkg{
	/// Stores information about the package, such as its name and description.
	pub core: PkgCore,
	/// Is this read only?
	pub read_only: bool,
	/// The value type
	pub ty: Option<Box<str>>,
	/// The default value
	pub default: Option<Lit>,
}
#[derive(Debug)]
pub struct PackageOperator {
	/// Stores information about the package, such as its name and description.
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

#[derive(Debug)]
pub struct LibPackage {
	/// Stores information about the package, such as its name and description.
	pub core: PkgCore,
	/// External link
	pub link: Option<Box<str>>,
	///For libraries with sub-namespaces such as mwse.mcm, etc., this array contians the nested namespaces.
	pub sublibs: Option<Vec<LibPackage>>,
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
pub struct EventPackage {
	/// Stores information about the package, such as its name and description.
	pub core: PkgCore,
	/// Filter for this event
	pub filter: Option<Box<str>>,
	/// Is this event blockable?
	pub blockable: bool,
	pub event_data: Vec<EventDatum>,
	pub links: Vec<EventLink>,
	pub related: Vec<Box<str>>,
}

