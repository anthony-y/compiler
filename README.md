# compiler
Compiler for a C/Go/Jai inspired language, written in C99.

This is a hobby project for fun, I don't expect people to write real code with it. This is the result of countless attempts to create a functioning compiler over the last four years, you can find [my very first attempt here](https://github.com/anthony-y/sail-lang).

Right now it only compiles on Linux, because I'm developing it under Windows Subsystem for Linux version 2. Just run `/build.sh` and it will spit out an ELF executable called `lang`, providing you have gcc installed on your Linux system.

## Syntax example

```go
#import "std/basic.lang"

msg := get_message();

proc main() {
    print(msg.data);
}

proc get_message(): string {
    return "Hello, world\n";
}
```

## Progress

I consider this project pre-alpha software, it has some bugs, but it mostly works. I'm working towards the basic feature set of the language being implemented robustly, then I will move onto real additions, right now it's basically a skin over C.

I am actively developing this project (as of September 2020), but here's some notes on the current progress:

- Currently outputting C code, this will change in the future.
- The bytecode interpreter for compile-time code execution is not currently working at all. It has been `#if`'d out for the time being while I redesign it.
- The lexer, parser, "resolver" (symbol resolution, expression-to-type evaluation, type inference) and checker (semantic and type checking) are fairly robust, though they most likely contain undiscovered bugs.

## How the code is organized

- `/scratch`: random files in the language itself which I use to test various aspects of the compiler, most of the code in here is nonsense and doesn't do anything lol.

- `/demos`: actual functioning code which serves some purpose.

- `src/headers/`: header files, this is where most of the types are declared.

- `src/headers/stb`: headers from the [public-domain libraries by Sean Barrett](https://github.com/nothings/stb).

- `src/`: actual C code, the file names should be self-explanatory, and each file has a comment describing what it contains at the very top.
