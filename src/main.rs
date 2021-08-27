use std::fmt::Display;
use std::path::PathBuf;

use structopt::StructOpt;
use tsuki_backend_llvm::{ExecutableFile, LlvmBackend, LlvmBackendConfig, OptimizationLevel};
use tsuki_frontend::backend::Backend;
use tsuki_frontend::common::{Errors, SourceFile};

#[derive(StructOpt)]
#[structopt(name = "tsuki")]
struct Options {
   /// The directory for storing intermediary files.
   #[structopt(long, parse(from_os_str))]
   cache_dir: Option<PathBuf>,

   /// The name of the package. This is used for controlling the object file's name.
   #[structopt(short = "p", long, name = "name")]
   package_name: String,

   /// The optimization level to use when compiling.
   #[structopt(long, name = "level", default_value = "essential")]
   optimize: OptimizationLevel,

   /// The root source file.
   #[structopt(name = "main file")]
   main_file: String,
}

const EXIT_COMPILE: i32 = 1;
const EXIT_FATAL: i32 = 2;

fn unwrap_error<T, E>(r: Result<T, E>) -> T
where
   E: Display,
{
   match r {
      Ok(ok) => ok,
      Err(error) => {
         eprintln!("error: {}", error);
         std::process::exit(EXIT_FATAL)
      }
   }
}

fn unwrap_errors<T>(r: Result<T, Errors>) -> T {
   match r {
      Ok(ok) => ok,
      Err(errors) => {
         errors.iter().for_each(|error| eprintln!("{:#}", error));
         std::process::exit(EXIT_COMPILE)
      }
   }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
   let options = Options::from_args();
   let backend = LlvmBackend::new(LlvmBackendConfig {
      cache_dir: &options.cache_dir.unwrap_or(std::env::current_dir()?.join("bin")),
      package_name: &options.package_name,
      // TODO: Cross-compilation.
      target_triple: None,
      optimization_level: options.optimize,
   });

   let source = unwrap_error(std::fs::read_to_string(&options.main_file));

   let object = unwrap_errors(backend.compile(SourceFile {
      filename: options.main_file,
      source,
   }));

   let executable = ExecutableFile::link(backend, &[object])?;
   executable.run(&[])?;

   Ok(())
}
