# tsuki

A programming language that focuses on being fun to program in, and aiding developers in writing more robust software, all while maintaining high performance.

The compiler is still in its infancy, and it'll probably take me a while before it's actually usable. In the meantime, you can check out the [spec](spec.md), which lays out the general feature set and vision of the language.

## Compiling

Right now compiling tsuki isn't exactly the most trivial of tasks, and Windows is not yet supported.

**Step 0.** Install a C compiler.

tsuki depends on libc and uses whatever C compiler is available on the system as `cc` to link executables. This can be overridden using the `$TSUKI_CC` or `$CC` environment variables, in that order of priority.

**Step 1.** Install LLVM 12.

This step is dependent on what Linux distribution you use. On Arch Linux (as of now) it's as simple as:

```
pacman -S llvm
```

Problems may arise when LLVM 13 or newer is released, but I'll make sure this guide is up to date when that happens.

Not all distributions include LLVM static libraries in their `llvm` package (looking at you, Ubuntu), so you might have to use LLVM's binaries that can be downloaded off of [their GitHub](https://github.com/llvm/llvm-project/releases). __Be sure to download LLVM 12.x.x, but not anything older or newer than that.

After downloading LLVM, the environment variable `$LLVM_SYS_120_PREFIX` has to be set. If you installed LLVM from the package manager, simply set it to `/`. Otherwise, set it to the root directory of the extracted archive (the directory with `bin`, `lib`, etc.)

**Step 2.** Compile and run.

With all that, running tsuki should be as simple as:
```
cargo run
```

## Other notes

### Nim version

tsuki used to be an embeddable scripting language for Nim. I'm bad with coming up with names, and the old tsuki wasn't finished anyways, so I decided to reuse the name for this new project. The overall syntax is similar, with minor tweaks and the addition of static typing (and features related to it).

If you'd like to check out the source code of the Nim version, it's kept on the [nim-version](tree/nim-version) branch of this repository. Feel free to fork it, expand upon it, and maybe even finish it. I think a general-purpose embeddable scripting language would be a nice addition to the Nim ecosystem, but I've moved on from using Nim as my main language, so if anybody wants to carry that torch – feel free to do so.
