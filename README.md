# tsuki

A programming language that focuses on being fun to program in, and aiding developers in writing more robust software, all while maintaining high performance.

The compiler is still in its infancy, and it'll probably take me a while before it's actually usable. In the meantime, you can check out the [spec](spec.md), which lays out the general feature set and vision of the language.

## Compiling

Right now compiling tsuki isn't exactly the most trivial of tasks, and Windows is not yet supported.

**Step 0.** Install a C (and C++) compiler.

tsuki depends on libc and uses whatever C compiler is available on the system as `cc` to link executables. This can be overridden using the `$TSUKI_CC` or `$CC` environment variables, in that order of priority. The C++ compiler is necessary to build LLVM.

**Step 1.** Compile LLVM 12.

The best way to get LLVM for tsuki is to build it manually. I had pretty bad experiences with using repository LLVM, with problems ranging from missing static libraries on Ubuntu, no `llvm-config` on Windows, to random SIGILLs after a month of hiatus on Arch.

So here's uncle Liquid's method of obtaining LLVM:
```shell
# This is where we're going to install LLVM, so change this to some sensible path.
# bash - in this case you also need to add this to .bashrc
export LLVM_SYS_120_PREFIX=$HOME/llvm
# fish
set -Ux LLVM_SYS_120_PREFIX ~/llvm

# Now it's time to get LLVM. We'll use their GitHub releases for that.
mkdir -p ~/llvm
wget https://github.com/llvm/llvm-project/releases/download/llvmorg-12.0.1/llvm-12.0.1.src.tar.xz
tar xJf llvm-12.0.1.src.tar.xz

# Now let's get the build going.
cd llvm-12.0.1.src
mkdir -p build
cd build
# If doing a release build, remove LLVM_ENABLE_ASSERTIONS, and set CMAKE_BUILD_TYPE to Release.
cmake .. -DCMAKE_INSTALL_PREFIX=$LLVM_SYS_120_PREFIX -DLLVM_ENABLE_ASSERTIONS=1 -DCMAKE_BUILD_TYPE=Debug -G Ninja

# IMPORTANT:
# Open a task manager or system monitor. You're going to want to look after your memory usage.
# If it starts growing rapidly, cancel the build and use --parallel 1.
# Linking uses up a lot of memory, so it's better to let it run a single linker at a time.
cmake --build . --target install --parallel 8
```

Maybe someday I'll make a dedicated script for this, but today is not that day.

**Step 2.** Compile and run.

With all that, running tsuki should be as simple as:
```
cargo run
```

## Using the compiler

While still in its early stages, the compiler is able to compile arbitrary user code into a working executable. The most basic usage of the compiler would be:
```
tsuki -p package-name path/to/code.tsu
```
`package-name` currently only specifies the name of the output file, but might be used for other things in the future, such as mangling and/or debug information.

Refer to the code examples in `code` to see what's currently implemented or being worked on.

## Other notes

### Nim version

tsuki used to be an embeddable scripting language for Nim. I'm bad with coming up with names, and the old tsuki wasn't finished anyways, so I decided to reuse the name for this new project. The overall syntax is similar, with minor tweaks and the addition of static typing (and features related to it).

If you'd like to check out the source code of the Nim version, it's kept on the [nim-version](tree/nim-version) branch of this repository. Feel free to fork it, expand upon it, and maybe even finish it. I think a general-purpose embeddable scripting language would be a nice addition to the Nim ecosystem, but I've moved on from using Nim as my main language, so if anybody wants to carry that torch – feel free to do so.
