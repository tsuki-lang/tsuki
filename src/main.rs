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

   /// The `src` directory of the package.
   #[structopt(short = "r", long, name = "path")]
   package_root: PathBuf,

   /// The root source file. Must be located in the package root.
   #[structopt(short = "m", long, name = "main file")]
   main_file: PathBuf,

   /// Only check the code for validity, without compiling it.
   #[structopt(long)]
   check: bool,

   /// The optimization level to use when compiling.
   #[structopt(long, name = "level", default_value = "essential")]
   optimize: OptimizationLevel,

   /// Dumps the source code before compiling.
   #[structopt(long)]
   dump_source: bool,

   /// Dumps the AST directly after parsing.
   #[structopt(long)]
   dump_ast_pre_sem: bool,

   /// Dumps the AST after checking it semantically.
   #[structopt(long)]
   dump_ast_post_sem: bool,

   /// Dumps the generated LLVM IR.
   #[structopt(long)]
   dump_llvm_ir: bool,
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
   let frontend_debug_options = tsuki_frontend::DebugOptions {
      dump_source: options.dump_source,
      dump_ast_pre_sem: options.dump_ast_pre_sem,
      dump_ast_post_sem: options.dump_ast_post_sem,
   };
   let backend = LlvmBackend::new(LlvmBackendConfig {
      cache_dir: &options.cache_dir.unwrap_or(std::env::current_dir()?.join("bin")),
      package_name: &options.package_name,
      // TODO: Cross-compilation.
      target_triple: None,
      optimization_level: options.optimize,
      frontend_debug_options,
      backend_debug_options: tsuki_backend_llvm::DebugOptions {
         dump_ir: options.dump_llvm_ir,
      },
   });

   let source = unwrap_error(std::fs::read_to_string(&options.main_file));

   let source_file = unwrap_error(SourceFile::new(
      options.package_name,
      options.package_root,
      options.main_file,
      source,
   ));

   if options.check {
      let _ = unwrap_errors(tsuki_frontend::analyze(
         &source_file,
         &frontend_debug_options,
      ));
   } else {
      let object = unwrap_errors(backend.compile(source_file));
      let _executable = ExecutableFile::link(backend, &[object])?;
   }

   Ok(())
}
