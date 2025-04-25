# Brief {#intro}

Hey, glad you stumbled across `subprocess.f`! This little Fortran repo is all about making your life easier when you want to kick off external programs or commands from inside your Fortran code. It is designed to let you run a shell command, fire up a script, or juggle a bunch of processes without breaking a sweat. You can find the whole thing over at [https://github.com/davidpfister/subprocess.f](https://github.com/davidpfister/subprocess.f).

So, what’s the big deal with `subprocess.f`? Well, Fortran’s awesome for crunching numbers and doing heavy lifting in science or engineering, but sometimes you need to call out to other tools—like a script, a system utility, or whatever else you’ve got lying around.

Here’s the rundown of what you can do with it:
- **Run Stuff Your Way**: Use `run` to fire off a command and chill until it’s done, or go with `runasync` to let it hum along in the background while you keep working.
- **Grab the Output**: Snag whatever the command prints with `read_stdout` for the good stuff or `read_stderr` for the error messages.
- **Boss the Process Around**: Check if it’s still going with `has_exited`, wait for it with `wait`, or shut it down early with `kill`. Oh, and if you’ve got a bunch of them, `waitall` has your back.
- **Know What’s Up**: Peek at exit codes to see if it worked, or check how long it took with `exit_time`. Handy for figuring out if things went south or just took their sweet time.

The library’s split into two parts to keep things tidy:
- **`subprocess`**: This is your go-to module. It’s got a `process` type that’s super easy to use—just tell it what to run and how, and you’re off to the races.
- **`subprocess_handler`**: This is the behind-the-scenes magic. It’s the layer that talks to the system using some C tricks, so you don’t have to mess with the low-level stuff yourself.

The code’s all hanging out on GitHub, and we’d love for you to dig in, try it out, and maybe even toss us some ideas or fixes if you’re feeling generous. This documentation’s packed with all the juicy details—API breakdowns, examples you can tweak, and tips to get you rolling. So, grab a coffee, scroll through, and let’s get some processes running together!