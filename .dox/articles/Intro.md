# Brief {#intro}

Hey, glad you stumbled across `subprocess.f`! This little Fortran gem is all about making your life easier when you want to kick off external programs or commands from inside your Fortran code. Think of it as your friendly sidekick that lets your Fortran app chat with the outside world—whether that’s running a quick shell command, firing up a script, or juggling a bunch of processes without breaking a sweat. You can find the whole thing over at [https://github.com/davidpfister/subprocess.f](https://github.com/davidpfister/subprocess.f), and we’re pumped to have you poking around!

So, what’s the big deal with `subprocess.f`? Well, Fortran’s awesome for crunching numbers and doing heavy lifting in science or engineering, but sometimes you need to call out to other tools—like a Python script, a system utility, or whatever else you’ve got lying around. That’s where this library swoops in. It’s got a slick setup that lets you launch stuff, grab what it spits out, and keep tabs on it, all without getting bogged down in the nitty-gritty. Plus, it’s built to play nice whether you’re on Windows or a Linux/macOS box, so no worries about where you’re coding.

Here’s the rundown of what you can do with it:
- **Run Stuff Your Way**: Use `run` to fire off a command and chill until it’s done, or go with `runasync` to let it hum along in the background while you keep coding.
- **Grab the Output**: Snag whatever the command prints with `read_stdout` for the good stuff or `read_stderr` for the error messages. You can even send it some input with `stdin` if it’s that kind of program.
- **Boss the Process Around**: Check if it’s still going with `has_exited`, wait for it with `wait`, or shut it down early with `kill`. Oh, and if you’ve got a bunch of them, `waitall` has your back.
- **Know What’s Up**: Peek at exit codes to see if it worked, or check how long it took with `exit_time`. Handy for figuring out if things went south or just took their sweet time.

The library’s split into two parts to keep things tidy:
- **`subprocess`**: This is your go-to module. It’s got a `process` type that’s super easy to use—just tell it what to run and how, and you’re off to the races.
- **`subprocess_handler`**: This is the behind-the-scenes magic. It’s the grunt work layer that talks to the system using some C tricks, so you don’t have to mess with the low-level stuff yourself.

Whether you’re a scientist hooking Fortran up to some fancy external tool, a coder throwing together a wild automation setup, or just someone curious about what Fortran can do beyond math, this library’s got something for you. It’s all about giving you the freedom to mix Fortran with whatever else you’re working with, no fuss required.

We’ve poured some love into making this thing portable and flexible, so it’ll run pretty much anywhere—Windows, Linux, macOS, you name it. The code’s all hanging out on GitHub, and we’d love for you to dig in, try it out, and maybe even toss us some ideas or fixes if you’re feeling generous. This documentation’s packed with all the juicy details—API breakdowns, examples you can tweak, and tips to get you rolling. So, grab a coffee, scroll through, and let’s get some processes running together!