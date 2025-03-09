
use std::{collections::HashMap, path::{Component, Path, PathBuf}};

use derive_more::{From, TryInto};

use crate::{lex::Lit, NAMED_TYPES_PATH_OUT};
// use log::debug;

// use crate::{lex::Lit, GLOBALS_PATH_OUT, NAMED_TYPES_PATH_OUT};


#[derive(Debug, Clone)]
pub struct Example {
	pub path: Box<str>,
	pub title: Option<Box<str>>,
	pub description: Option<Box<str>>,
}



// =============================================================================
// PACKAGE
// =============================================================================

#[derive(Debug)]
#[repr(C)]
pub struct PkgCore {
	/// Name of the package.
	/// This is set after parsing.
	/// It always originates from the filename.
	pub name: Box<str>,
	/// The namespace the file was in. This will be the result of concatenating the parent directories,
	/// using `.` as a seperator.
	pub path: Box<Path>,
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

impl PkgCore {

	pub fn new_from_path(path: Box<Path>) -> Self {
		let fname = path.file_name().unwrap().to_str().unwrap();
		debug_assert!(fname.ends_with(".lua"));
		let name: Box<str> = Box::from(&fname[..fname.len() - 4]);
		PkgCore{ path, name, description: None, experimental: false, examples: None, deprecated: false, }
	}
	
	pub fn namespace(&self) -> String {
		let parts: Vec<&str> = self.path.components()
			.map(|c| c.as_os_str().to_str().unwrap())
			.collect();

		if parts.len() >= 3 {
			parts[2..parts.len() - 1].join(".")
		} else {
			String::new()
		}
	}

	/// Gets the relative part of the path of this file.
	/// For example, `docs/namedTypes/tes3mobilePlayer.lua` becomes `tesmobilePlayer.lua`.
	pub fn relative_path(&self) -> impl Iterator<Item = Component> {
		self.path.components().skip(2)
	}
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
#[derive(Debug, Default)]
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

#[derive(Debug)]
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
	pub fn new(core: PkgCore, is_abstract: bool, inherits: Option<Box<str>>) -> Self {
		Self {core, inherits, is_abstract, functions: vec![], values: vec![], methods: vec![], ops: vec![]}
	}

	/// Resolves the inheritance chain and retrieves all the values associated with this class.
	pub fn get_all_values<'a>(&'a self, class_map: &'a HashMap<&'a str, &'a ClassPkg>) -> (Vec<&'a ValuePkg>, Vec<&'a FunctionPkg>, Vec<&'a MethodPkg>) {
		let mut values: Vec<&ValuePkg> = self.values.iter().collect();
		let mut functions: Vec<&FunctionPkg> = self.functions.iter().collect();
		let mut methods: Vec<&MethodPkg> = self.methods.iter().collect();
		
		let mut parent_name = self.inherits.as_deref();
		while let Some(pname) = parent_name {
			if let Some(&parent) = class_map.get(pname) {
				parent_name = parent.inherits.as_deref();
				for val in &parent.values {
					values.push(val);
				}
				for func in &parent.functions {
					functions.push(func);
				}
				for m in &parent.methods {
					methods.push(m);
				}
			} else {
				panic!("Class {:?} inherits from a class named {pname:?}, but we could not find any documentation for that class.", 
					self.core.name.as_ref()
				)
			}
		}
		values.sort_by(|a,b| a.core.name.cmp(&b.core.name));
		functions.sort_by(|a,b| a.core.name.cmp(&b.core.name));
		methods.sort_by(|a,b| a.core.name.cmp(&b.core.name));
		(values, functions, methods)
	}


	pub fn mk_docs_out_path(&self) -> PathBuf {
		let mut path = PathBuf::from(NAMED_TYPES_PATH_OUT);
		// remove the `globals/namedTypes`
		path.extend(self.core.path.as_ref().components().skip(2));
		path.set_extension("md");

		path
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

#[derive(Debug)]
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

impl LibPkg {

	pub const MKDOCS_PATH_OUT: &'static str = "output/apis";

	pub fn new(core: PkgCore, link: Option<Box<str>>) -> Self {
		Self { core, link, sublibs: vec![], functions: vec![], values: vec![] }
	}

	pub fn get_all_values(&self) -> (Vec<&ValuePkg>, Vec<&FunctionPkg>) {
		let mut values: Vec<&ValuePkg> = self.values.iter().collect();
		let mut functions: Vec<&FunctionPkg> = self.functions.iter().collect();

		for sublib in &self.sublibs {
			let (vals, fns) = sublib.get_all_values();
			values.extend(vals);
			functions.extend(fns);
		}
		values.sort_by(|a,b| a.core.name.cmp(&b.core.name));
		functions.sort_by(|a,b| a.core.name.cmp(&b.core.name));
		(values, functions)
	}

	pub fn mkdocs_out_path(&self) -> PathBuf {
		let mut path = PathBuf::from(Self::MKDOCS_PATH_OUT);
		// remove the `globals/namedTypes`
		path.extend(self.core.path.as_ref().components().skip(2));
		path.set_extension("md");

		path
	}
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

