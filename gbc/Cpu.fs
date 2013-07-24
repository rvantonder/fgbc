open System

type cpu = { A : byte; B : byte; C : byte; D : byte; E : byte; // registers
             f_Z : byte; f_H : byte; f_N : byte; f_C : byte; // flags 
             HL : uint16; SP : uint16; PC : uint16; CPU_Ticks : uint16}


let state = { A = 0x00uy; B = 0x0uy; C = 0x00uy; D = 0x00uy; E = 0x00uy;
                  f_Z = 0x00uy; f_N = 0x0uy; f_H = 0x00uy; f_C = 0x00uy;
                  PC = 0x0000us; SP = 0x0000us; HL = 0x000dus; CPU_Ticks = 0x00us; }

type reg = 
    | A
    | B
    | C 
    | D 
    | E 
    | F_Z
    | F_H 
    | F_N
    | F_C
    | HL 
    | SP 
    | PC 
    | CPU_Ticks 
    | Mλ of reg * reg

let rec set v cpu = 
    match v with
    | A, x -> {cpu with A = x}
    | B, x -> {cpu with B = x}
    | C, x -> {cpu with C = x}
    | D, x -> {cpu with D = x}
    | E, x -> {cpu with E = x}
    | F_Z, x -> {cpu with f_Z = x}
    | F_H, x -> {cpu with f_H = x}
    | F_N, x -> {cpu with f_N = x}
    | F_C, x -> {cpu with f_C = x}
    | HL, x -> {cpu with HL = (uint16) x}
    | SP, x -> {cpu with SP = (uint16) x}
    | PC, x -> {cpu with PC = (uint16) x}
    | CPU_Ticks, x -> {cpu with CPU_Ticks = (uint16) x}
    | Mλ(r1, r2), x -> set (r1, x) cpu |> set (r2, x)

let rec modify v cpu = 
    match v with
    | A, x -> {cpu with A = cpu.A + x}
    | B, x -> {cpu with B = cpu.B + x}
    | C, x -> {cpu with C = cpu.C + x}
    | D, x -> {cpu with D = cpu.D + x}
    | E, x -> {cpu with E = cpu.E + x}
    | PC, x -> {cpu with PC =  cpu.PC + (uint16) x}
    | CPU_Ticks, x -> {cpu with CPU_Ticks = cpu.CPU_Ticks + (uint16) x}
    | _ -> failwith "Can't do that for you sorry"

let inc reg cpu = cpu |> modify (reg, 1uy) // actually accepts tuple
let dec reg cpu = cpu |> modify (reg, (255uy)) // same as (0uy-1uy)
//state |> set (A, 5uy) |> inc A |> inc B

let read adr = 0x0uy // placeholder
let write adr v = printfn "%d" adr //

let inline (@) a b = BitConverter.ToUInt16([|a; b|], 0)
let getBytes (n:uint16) = BitConverter.GetBytes(n)

let exec op state = 
    match op with
    | 0x00 -> state // NOP
    | 0x01 -> state |> inc PC |> set (C, read PC) |> inc PC |> set (B, read PC) |> modify (CPU_Ticks, 12uy)
    | 0x02 -> write (state.B@state.C) state.A ; state |> modify (CPU_Ticks, 8uy)
    | 0x03 -> state |> set (B, (getBytes ((state.B@state.C)+1us)).[0]) |> set (C, (getBytes ((state.B@state.C)+1us)).[1])
    | 0x04 -> inc B state
    | 0x05 -> dec B state
    | 0x06 -> state |> inc PC |> set (B, read PC) |> modify (CPU_Ticks, 8uy)
    | _ -> failwith "Sorry Dave, can't do that for you"

let init_state = { A = 0x01uy; B = 0x00uy; C = 0x13uy; D = 0x00uy; E = 0xd8uy;
                  f_Z = 0x01uy; f_N = 0x01uy; f_H = 0x01uy; f_C = 0x01uy;
                  PC = 0x0100us; SP = 0xfffeus; HL = 0x014dus; CPU_Ticks = 0x00us; }

let next_state = init_state |> exec 0x0

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

