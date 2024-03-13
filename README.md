# Purity 

A purely functional terminal shell for general purpose usage and scripting.

![Purity Preview](https://github.com/Archaversine/purity/blob/main/pictures/preview.png)

## Configuration 

Purity offers two different files for configuration: `Config.hs` and `User.hs`.
Both files are configured with the [Haskell Programming Language](https://www.haskell.org/).

`Config.hs` is the configuration file for the shell itself. All definitions will 
automatically be imported into the shell's environment as a qualified import 
under `Config`. Any values defined in the `Config.hs` script should not be 
deleted as their absence can prevent the shell from starting successfully.

`User.hs` is the configuration for the user themselves. Definitions in this file 
are not required by the shell and may be modified to the user's preference. This 
file serves as a way to define aliases, functions, and other user-specific 
settings. All definitions will be automatically imported to the shell upon 
startup.

## Code Mode 

The shell has two modes: Command and Code mode. Code mode is a much stricter 
variant of command mode, in that it only allows for proper Haskell code to be 
executed. For example, assuming there is an `echo :: String -> IO ()` function 
in scope, `echo Hello, World!` will cause an error as opposed to `echo "Hello, World!"`.
(Both work in Command mode).

Code mode is not enabled by default on the shell. To switch to it, use the `#mode code` 
directive. You can also use the backslash character to toggle between modes

## Command Mode

This is the default mode of the shell. It allows for the execution of commands 
and Haskell code. It does this by attempting to auto-format the input into an 
acceptable Haskell expression. For example, `randomCmd input output "my text" MyValue 3` 
would be reformatted as `randomCmd "input" "output" "my text" MyValue 3`. This, however, 
has its drawbacks as expressions such as `1 + succ 2` are no longer valid since 
they will be reformatted to `1 + "succ" 2`, which yeilds a type error. To get 
around this, Purity will attempt to run it as an unformatted Haskell expression 
upon failure, and will attempt to run it as an external command if that fails.

In summary:

1. Attempt to format the input to a valid Haskell expression.
2. If unsuccessful, attempt to run the input as an unformatted Haskell expression.
3. If unsuccessful, attempt to run the input as an external command.
4. Display the error message as if it were a formatted Haskell expression.

This mode is enabled by default on the shell and can be switched to using the 
`#mode cmd` directive.

## Directives 

To add further functionality, Purity offers a set of directives that are persistent 
between shell modes. These directives are as follows: 

| Directive | Alias (if any) | Description | 
| --------- | -------------- | ----------- | 
| `#quit`   | `:q`           | Exits the shell. | 
| `#purge`  |                | Removes all imports and definitions. |
| `#mode`   |                | Switches between Command (`cmd`) and Code (`code`) mode. |
| `#import` |                | Imports a module. (e.g. `#import Data.List`). |
| `#importQ`|                | Imports a module qualified. (e.g. `#importQ Data.List:L`). |
| `#type`   | `:t`           | Shows the type of an expression. |
| `#kind`   | `:k`           | Shows the kind of an expression. |
| `#source` |                | Run a file as a script. |

## Scripting

Purity allows for the execution of scripts using the `#source` directive. This 
directive takes a file path as an argument and runs the file as a script (multiple are supported). 
The file is run in the same environment as the shell, meaning that any definitions 
made in the file will be available to the shell after the script has finished 
running.

### Code Blocks

There are times where multiple lines are neater to use than a single line. For 
this, Purity offers code blocks. These are defined with triple backticks.

For example:

````haskell 

-- This will run as a singular expression
```
let fib 0 = 0 
    fib 1 = 1 
    fib n = fib (n - 1) + fib (n - 2)
```

````

Code blocks are also supported in the shell itself.
