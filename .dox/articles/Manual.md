# End-User Manual {#manual}

Welcome to the `subprocess` module! This tool lets you run external commands (like programs or scripts) from your Fortran program, manage them, and capture their output. It’s designed to be flexible and easy to use, whether you’re running a command and waiting for it to finish or launching something in the background while your program keeps going.

This manual explains how to use the module in simple terms, with examples you can try out. We’ll assume you have a basic Fortran setup and can compile code that uses this module.

## What You Can Do with This Module

- **Run commands**: Execute any command or program, like `ls` on Linux or `dir` on Windows, with or without arguments.
- **Wait or don’t wait**: Run a command and wait for it to finish (synchronous), or let it run in the background (asynchronous).
- **Capture output**: Read what the command prints to the screen (stdout) or its error messages (stderr).
- **Control processes**: Check if a command is still running, wait for it to finish, or stop it early.
- **Pass input**: Send data to a command’s input (stdin), if it expects it.

The main tool you’ll use is the `process` type, which acts like a container for a command you want to run. You create a `process`, tell it what to do, and then use its features to manage it.

## Getting Started

To use this module, include it in your Fortran program:

```fortran
use subprocess
implicit none
```

You’ll also need to link against the `subprocess_handler` library when compiling, but that’s a detail for your compiler setup. For now, let’s focus on the code.

## Key Features of the `process` Type

When you create a `process`, it has some properties and actions (methods) you can use:

- **Properties**:
  - `pid`: The process ID (a number) assigned to the running command.
  - `filename`: The name of the command or program you’re running (e.g., `"notepad.exe"` or `"ls"`).
  - (Other internal details like exit codes and timing are handled automatically.)

- **Actions**:
  - `run`: Starts the command and waits for it to finish.
  - `runasync`: Starts the command and lets it run in the background.
  - `read_stdout`: Gets the command’s output.
  - `read_stderr`: Gets the command’s error messages.
  - `wait`: Waits for a background command to finish.
  - `kill`: Stops a running command.
  - `exit_code`: Tells you how the command ended (e.g., 0 means success).
  - `exit_time`: Shows how long the command ran (in milliseconds).
  - `has_exited`: Checks if the command is done.

## Examples

Let’s walk through some common tasks with examples. These assume you’re on a system where the commands work (e.g., Windows for `dir`, Linux/macOS for `ls`). Adjust the commands to match your system.

### 1. Running a Command and Waiting for It to Finish

Suppose you want to list files in a directory. On Windows, you’d use `dir`; on Linux, `ls`. Here’s how to run `dir` and wait for it to finish:

```fortran
program simple_run
    use subprocess
    implicit none
    type(process) :: proc

    ! Create a process for the "dir" command
    proc = process_new("dir")

    ! Run it and wait
    call proc%run()

    print *, "Command finished!"
end program simple_run
```

- **What happens**: The program runs `dir`, waits until it’s done, and then prints "Command finished!"
- **Note**: You won’t see the output of `dir` yet—we’ll cover that later.

You can also add arguments. Here’s how to run `echo` with a message:

```fortran
program run_with_args
    use subprocess
    implicit none
    type(process) :: proc

    proc = process_new("echo")
    call proc%run("Hello, world!")

    print *, "Echo done!"
end program run_with_args
```

- **What happens**: It runs `echo Hello, world!` and waits for it to finish.

You can add up to five arguments directly (e.g., `call proc%run("arg1", "arg2", "arg3")`), or use an array for more flexibility (see the advanced example later).

### 2. Running a Command in the Background

If you don’t want to wait, use `runasync`. Here’s how to start `notepad` (on Windows) and keep going:

```fortran
program async_run
    use subprocess
    implicit none
    type(process) :: proc

    proc = process_new("notepad.exe")
    call proc%runasync()

    print *, "Notepad is running in the background!"
    print *, "Process ID:", proc%pid

    ! Wait a bit (optional, for demo)
    call sleep(2)

    ! Check if it’s still running
    if (proc%has_exited()) then
        print *, "Notepad already closed."
    else
        print *, "Notepad is still open."
    end if
end program async_run
```

- **What happens**: Notepad opens, and your program keeps running. It prints the process ID and checks if Notepad is still open after 2 seconds.
- **Note**: `sleep` isn’t part of this module—you’d need a custom delay or skip that part.

### 3. Capturing Output

To see what a command prints, use `read_stdout`. Here’s how to run `dir` and print its output:

```fortran
program capture_output
    use subprocess
    implicit none
    type(process) :: proc
    character(:), allocatable :: output

    proc = process_new("dir")
    call proc%run()

    call proc%read_stdout(output)
    print *, "Output from dir:"
    print *, output
end program capture_output
```

- **What happens**: It runs `dir`, waits for it to finish, and then prints whatever `dir` outputted.
- **For errors**: Use `read_stderr` the same way to catch error messages.

### 4. Stopping a Command Early

If a command is running and you want to stop it, use `kill`. Here’s an example with a long-running command (like `ping` on Windows):

```fortran
program kill_process
    use subprocess
    implicit none
    type(process) :: proc

    proc = process_new("ping")
    call proc%runasync("localhost")  ! Start pinging in the background

    print *, "Pinging started. Waiting 3 seconds..."
    call sleep(3)

    if (.not. proc%has_exited()) then
        call proc%kill()
        print *, "Ping stopped!"
    else
        print *, "Ping already finished."
    end if
end program kill_process
```

- **What happens**: It starts `ping localhost`, waits 3 seconds, and then stops it if it’s still running.

### 5. Waiting for Multiple Commands

If you’re running several commands in the background, use `waitall` to wait for all of them. Here’s an example:

```fortran
program wait_all
    use subprocess
    implicit none
    type(process) :: procs(2)

    procs(1) = process_new("echo")
    procs(2) = process_new("echo")

    call procs(1)%runasync("First message")
    call procs(2)%runasync("Second message")

    print *, "Both echoes started. Waiting for them to finish..."
    call waitall(procs)

    print *, "All done!"
end program wait_all
```

- **What happens**: Two `echo` commands run in the background, and the program waits until both finish.

### 6. Checking Results

After a command finishes, you can check its exit code (0 usually means success) and how long it took:

```fortran
program check_results
    use subprocess
    implicit none
    type(process) :: proc
    integer :: code
    real(8) :: time_taken

    proc = process_new("echo")
    call proc%run("Test")

    code = proc%exit_code()
    time_taken = proc%exit_time()

    print *, "Exit code:", code
    print *, "Time taken (ms):", time_taken
end program check_results
```

- **What happens**: It runs `echo Test`, then shows the exit code and runtime in milliseconds.

## Tips for Using the Module

- **Command Names**: Use the full path (e.g., `"C:\Windows\notepad.exe"`) if the command isn’t in your system’s PATH.
- **Arguments**: For commands with spaces or special characters, test carefully—some systems might need quotes.
- **Background Commands**: If you use `runasync`, remember to check `has_exited()` or call `wait()` if you need to sync up later.
- **Output**: `read_stdout` and `read_stderr` only work after the command finishes with `run`, or you’ll need to manage timing with `runasync`.
- **Cleanup**: The module handles cleanup automatically when a `process` goes out of scope, but `kill()` ensures a command stops if needed.

## Advanced Example: Running a Script with Arguments

Here’s a more complex example running a hypothetical Python script with multiple arguments:

```fortran
program advanced_run
    use subprocess
    implicit none
    type(process) :: proc
    type(string) :: args(3)
    character(:), allocatable :: output

    ! Set up the process and arguments
    proc = process_new("python")
    args(1)%chars = "myscript.py"
    args(2)%chars = "--input"
    args(3)%chars = "data.txt"

    ! Run with an array of arguments
    call proc%run(args)

    ! Get the output
    call proc%read_stdout(output)
    print *, "Script output:"
    print *, output

    ! Check results
    print *, "Exit code:", proc%exit_code()
    print *, "Time (ms):", proc%exit_time()
end program advanced_run
```

- **What happens**: It runs `python myscript.py --input data.txt`, captures the output, and shows the results.
- **Note**: The `string` type is used here for argument arrays—set `chars` directly as shown.

## Troubleshooting

- **Command not found**: Check the `filename` and ensure it’s correct for your system.
- **No output**: Make sure you call `read_stdout` or `read_stderr` after the command finishes.
- **Process won’t stop**: Use `kill()` and check `has_exited()` to confirm it worked.
- **Errors**: Look at `exit_code()`—non-zero values mean something went wrong (specific meanings depend on the command).

## Wrapping Up

The `subprocess` module makes it easy to run and manage external commands from Fortran. Start with simple `run` calls, then explore `runasync` and output capturing as needed. Play with the examples, tweak them for your system, and you’ll be controlling processes like a pro!

For more details, check the API documentation (if you have it)—it lists every method and property in technical terms. Happy coding!