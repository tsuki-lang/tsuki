use std::path::Path;

use tsuki_backend_llvm::{LlvmBackend, LlvmBackendConfig};
use tsuki_frontend::backend::{Backend, Executable};
use tsuki_frontend::common::{Errors, SourceFile};

fn unwrap_errors<T>(r: Result<T, Errors>) -> T {
   match r {
      Ok(ok) => ok,
      Err(errors) => {
         errors.iter().for_each(|error| eprintln!("{:#}", error));
         std::process::exit(1)
      }
   }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
   let executable = unwrap_errors(
      LlvmBackend::new(LlvmBackendConfig {
         cache_dir: &std::env::current_dir()?.join("bin"),
         executable_name: "test",
         target_triple: None,
      })
      .compile(SourceFile {
         filename: "test.tsu".into(),
         source: r#"
            __intrin_print_int32(42)
         "#
         .into(),
      }),
   );

   executable.run(&[])?;

   Ok(())
}
