use tsuki_backend_llvm::{ExecutableFile, LlvmBackend, LlvmBackendConfig};
use tsuki_frontend::backend::Backend;
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
   let backend = LlvmBackend::new(LlvmBackendConfig {
      cache_dir: &std::env::current_dir()?.join("bin"),
      package_name: "test",
      target_triple: None,
   });
   let object = unwrap_errors(
      backend.compile(SourceFile {
         filename: "test.tsu".into(),
         source: r#"
            __intrin_print_int32(42 + (3_i16 - 1_i16))
         "#
         .into(),
      }),
   );

   let executable = ExecutableFile::link(backend, &[object])?;
   executable.run(&[])?;

   Ok(())
}
