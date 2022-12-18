# d3d9-inject-suba-yokubukai

Repository for experiments on reverse-engineering `Konosuba: Kono Yokubukai Game ni Shinpan o!` embedded Squirrel script

## Features

- Rust wrappers for squirrel vm api
- Experimental squirrel debugger

## Debugger commands overview

    step                Step one debug callback call [aliases: s]
    continue            Continue execution [aliases: c]
    backtrace           Print call backtrace [aliases: bt]
    locals              Print local variables list at specified call stack level [aliases: loc]
    examine             Print value of local variable [aliases: x]
    breakpoint-add      Add new breakpoint [aliases: b, break]
    breakpoint-enable   Enable breakpoint. If number not specified, enable all [aliases: be]
    breakpoint-disable  Disable breakpoint. If number not specified, disable all [aliases: bd]
    breakpoint-clear    Clear breakpoint. If number not specified, clear all [aliases: bc]
    breakpoint-list     List all breakpoints [aliases: bl]
    evaluate            Compile and run arbitrary squirrel code [aliases: eval]
    buffer              Add, remove, edit and view script buffers [aliases: buf]
    trace               Continue execution, but print every debug event [aliases: t]
    set                 Set values of different debugging variables
    save                Save breakpoints and buffers
    load                Load breakpoints and buffers
    exit                Exit process
    help                Print this message or the help of the given subcommand(s)

