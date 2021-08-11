use tsuki_backend_llvm::LlvmBackend;
use tsuki_frontend::common::{Errors, SourceFile};
use tsuki_frontend::backend::{Backend, Executable};

fn unwrap_errors<T>(r: Result<T, Errors>) -> T {
   match r {
      Ok(ok) => ok,
      Err(errors) => {
         errors.iter().for_each(|error| eprintln!("{:#}", error));
         std::process::exit(1)
      }
   }
}

fn main() {
   let executable = unwrap_errors(LlvmBackend::compile(SourceFile {
      filename: "test.tsu".into(),
      source: r#"
         2 + 2
      "#.into(),
   }));
   executable.run(&[]);
}
