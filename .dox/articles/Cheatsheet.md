# Cheatsheet {#cheatsheet}
## Launching a Process

To launch a process you call `subprocess_create` like so:

```c
const char *command_line[] = {"echo", "\"Hello, world!\"", NULL};
struct subprocess_s subprocess;
int result = subprocess_create(command_line, 0, &subprocess);
if (0 != result) {
  // an error occurred!
}
```

You specify an array of string for the command line - terminating the array with
a `NULL` element.

If the process is created successfully then 0 is returned from
`subprocess_create`.

## Writing to the Standard Input of a Process

To write to the standard input of a child process you call `subprocess_stdin` to
get the FILE handle to write with, passing a previously created process, like
so:

```c
FILE* p_stdin = subprocess_stdin(&process);
fputs("Hello, world!", p_stdin);
```

Care must be taken to not write to the stdin after any call to `subprocess_join`
or `subprocess_destroy`.

## Reading from the Standard Output of a Process

To read from the standard output of a child process you call `subprocess_stdout`
to get the FILE handle to read with, passing a previously created process, like
so:

```c
FILE* p_stdout = subprocess_stdout(&process);
char hello_world[32];
fgets(hello_world, 32, p_stdout);
```

Care must be taken to not read from the stdout after any call to 
`subprocess_destroy`.

## Reading from the Standard Error of a Process

To read from the standard error of a child process you call `subprocess_stderr`
to get the FILE handle to read with, passing a previously created process, like
so:

```c
FILE* p_stderr = subprocess_stderr(&process);
char hello_world[32];
fgets(hello_world, 32, p_stderr);
```

Care must be taken to not read from the stderr after any call to 
`subprocess_destroy`.

## Waiting on a Process

To wait for a previously created process to finish executing you call
`subprocess_join` like so:

```c
int process_return;
int result = subprocess_join(&process, &process_return);
if (0 != result) {
  // an error occurred!
}
```

The return code of the child process is returned in the second argument (stored
into `process_return` in the example above). This parameter can be `NULL` if you
don't care about the process' return code.

If the child process encounters an unhandled exception, the return code will always
be filled with a _non zero_ value.

## Destroying a Process

To destroy a previously created process you call `subprocess_destroy` like so:

```c
int result = subprocess_destroy(&process);
if (0 != result) {
  // an error occurred!
}
```

Note that you can destroy a process before it has completed execution - this
allows you to spawn a process that would _outlive_ the execution of the parent
process for instance.

## Terminating a Process

To terminate a (possibly hung) previously created process you call
`subprocess_terminate` like so:

```c
int result = subprocess_terminate(&process);
if (0 != result) {
  // an error occurred!
}
```

Note that you still can call `subprocess_destroy`, and `subprocess_join` after
calling `subprocess_terminate`, and that the return code filled by
`subprocess_join(&process, &process_return)` is then guaranteed to be _non zero_.

## Reading Asynchronously

If you want to be able to read from a process _before_ calling `subprocess_join`
on it, you cannot use `subprocess_stdout` or `subprocess_stderr` because the
various operating systems that this library supports do not allow for this.

Instead you first have to call `subprocess_create` and specify the
`subprocess_option_enable_async` option - which enables asynchronous reading.

Then you must use the `subprocess_read_stdout` and `subprocess_read_stderr`
helper functions to do any reading from either pipe. Note that these calls _may_
block if there isn't any data ready to be read.

## Using a Custom Process Environment

The `subprocess_create_ex` entry-point contains an additional argument
`environment`. This argument is an array of `FOO=BAR` pairs terminating in a
`NULL` entry:

```c
const char *command_line[] = {"echo", "\"Hello, world!\"", NULL};
const char *environment[] = {"FOO=BAR", "HAZ=BAZ", NULL};
struct subprocess_s subprocess;
int result = subprocess_create_ex(command_line, 0, environment, &subprocess);
if (0 != result) {
  // an error occurred!
}
```

This lets you specify custom environments for spawned subprocesses.

Note though that you **cannot** specify `subprocess_option_inherit_environment`
with a custom environment. If you want to merge some custom environment with the
parent process environment then it is up to you as the user to query the original
parent variables you want to pass to the child, and specify them in the spawned
process' `environment`.

## Spawning a Process With No Window

If the `options` argument of `subprocess_create` contains
`subprocess_option_no_window` then, if the platform supports it, the process
will be launched with no visible window.

```c
const char *command_line[] = {"echo", "\"Hello, world!\"", NULL};
struct subprocess_s subprocess;
int result = subprocess_create(command_line, subprocess_option_no_window, &subprocess);
if (0 != result) {
  // an error occurred!
}
```

This option is _only required_ on Windows platforms at present if the behaviour
is seeked for.