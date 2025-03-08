/// Recursively processes a file and builds a `EPkg` from it.
/// If there is a directory with the same name as `path`, then that directory will be recursively processed
/// and any packages found there will be added to the package returned by this function.
/// The `Box::pin` stuff is necessary for recursive async functions.
fn process_file(path: PathBuf) -> Pin<Box<dyn Send + Sync + Future<Output = Result<EPkg, Error>>>> {
	Box::pin(async move {
			
		// Only return a file if we were able to parse it.
		// Log the errors instead of killing the whole program.
		let mut epkg = match EPkg::parse_from_file(&path).await {
			Err(e) => {
				error!("{path:?}: {e:?}");
				return Err(e);
			},
			Ok(epkg) => epkg,
		};


		// Directory is what you get after removing the `.lua` extension from the filepath.
		let dir = path.with_extension("");

		// Recursively process the child directory if necessary
		if dir.exists() && dir.is_dir() {

			// process_dir(dir, ignore).await?
			trace!("processing {dir:?}");
			// Iterator over all the files in the directory.
			let mut dir_contents = tokio::fs::read_dir(dir).await?;

			// This stores all of the recursive calls to `process_file`.
			// These are started up in new threads and executed asynchronously,
			// then joined together at the end.
			// Doing this asynchronously results in a massive speedup.
			let mut file_handles = Vec::new();

			// Any descendent `EPkg`s that should be merged into this one.
			let mut children: Vec<EPkg> = Vec::new();



			// Both arms of this branch are responsible for iterating over the directory
			// and filling up `children`.
			// Things are split up in order to optimize the code, since one branch skips over
			// files that match the examples found in `epkg.core()`, and most files don't have examples.
			match epkg.core().examples.as_ref() {
				None => {
					while let Some(entry) = dir_contents.next_entry().await? {
						// directories are handled in recursive calls
						if !entry.file_type().await?.is_file() { continue; }
						// recursively process any files in this directory
						// if let Ok(epkg) = process_file(entry.path()).await {
						// 	children.push(epkg);
						// }
						file_handles.push(tokio::spawn(process_file(entry.path())));
					}
				},
				// `epkg` has examples. We shouldn't parse any files that are intended to be examples.
				Some(examples) => {
					let to_ignore: HashSet<Box<str>> = examples.iter()
						.map(|e| e.path.clone())
						.collect();
					while let Some(entry) = dir_contents.next_entry().await? {
				
						// directories are handled in recursive calls
						if !entry.file_type().await?.is_file() { continue; }
		
						// don't even try to parse the examples
						let path: PathBuf = entry.path().with_extension("");
						let filename = path.file_name().unwrap().to_str().unwrap();
						if to_ignore.contains(filename) {
							continue;
						}
						// recursively process any files in this directory
						// if let Ok(epkg) = process_file(entry.path()).await {
						// 	children.push(epkg);
						// }
						file_handles.push(tokio::spawn(process_file(entry.path())));
					}
				}
			};

			for file_handle in file_handles {
				if let Ok(epkg) = file_handle.await? {
					children.push(epkg);
				}
			}

			// Add the processed children to the parent file, but only if there are any children to add.
			// We are doing this `len` check in order to avoid triggering the `error`s in the
			// bottom branches of the `match &mut epkg` statement.
			if children.len() > 0 {
				match &mut epkg {
					EPkg::Class(cls_pkg) => {
						for child in children {
							match child {
								EPkg::Function(pkg) => cls_pkg.functions.push(pkg),
								EPkg::Method(pkg) => cls_pkg.methods.push(pkg),
								EPkg::Operator(pkg) => cls_pkg.ops.push(pkg),
								EPkg::Value(pkg) => cls_pkg.values.push(pkg),
								
								p => todo!("is this even possible? child = {:?}", p.core().name),
							}
						
						}
					},
					EPkg::Lib(lib_pkg) => for child in children {
						match child {
							EPkg::Function(pkg) => lib_pkg.functions.push(pkg),
							// EPkg::Method(pkg) => lib_pkg.methods.push(pkg),
							EPkg::Lib(pkg) => lib_pkg.sublibs.push(pkg),
							EPkg::Value(pkg) => lib_pkg.values.push(pkg),
							
							p => todo!("is this even possible? child = {:?}", p.core().name),
						}
					},
	
					EPkg::Function(par) => todo!("is this even possible? par = Function({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Method(par) => todo!("is this even possible? par = Method({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Value(par) => todo!("is this even possible? par = Value({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Event(par) => todo!("is this even possible? par = Event({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Operator(par) => todo!("is this even possible? par = Operator({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
				}
			}
		}
	

		Ok(epkg)
	})
}
