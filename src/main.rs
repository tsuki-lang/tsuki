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
            val a = 42
            __intrin_print_int32(a / 2 * 4)
            __intrin_print_float32(10.0)
            10.0_f64 + 5.0
            val b = 2
            do
              __intrin_print_int32(10)
         "#
         .into(),
      }),
   );

   let executable = ExecutableFile::link(backend, &[object])?;
   executable.run(&[])?;

   Ok(())
}
