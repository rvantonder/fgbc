type cpu = { A : byte; B : byte; C : byte; D : byte; E : byte; // registers
             f_Z : byte; f_H : byte; f_N : byte; f_C : byte; // flags 
             HL : uint16; SP : uint16; PC : uint16; CPU_Ticks : uint16}


let state = { A = 0x01uy; B = 0x0uy; C = 0x00uy; D = 0x00uy; E = 0x00uy;
                  f_Z = 0x00uy; f_N = 0x0uy; f_H = 0x00uy; f_C = 0x00uy;
                  PC = 0x0000us; SP = 0x0000us; HL = 0x000dus; CPU_Ticks = 0x00us; }
type op =
    | INC 
    | DEC
    | V of byte

type reg = 
    | A of op
    | B of op
    | C of op
    | D of byte
    | E of byte
    | F_Z of byte
    | F_H of byte
    | F_N of byte
    | F_C of byte
    | HL of uint16
    | SP of uint16
    | PC of uint16
    | CPU_Ticks of uint16
    | Mλ of reg * reg

type rg = 
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
    | Mλ of rg * rg

let rec modify cpu r = 
    match r with
    | A, x -> {cpu with A = cpu.A + x}
    | A, y -> {cpu with A = y}
    | PC, x -> {cpu with PC = cpu.PC + (uint16) x}
    | Mλ(r1, r2), x -> modify cpu (r1, x) |> modify <| (r2, x)

let inc_1 cpu r = inc 1uy cpu r
    
type t2 = {a : reg; b : reg}

let rec update_cpu cpu = function // implicitly matches A..B.. etc
    | A INC -> {cpu with A = cpu.A + 1uy}
    | A (V v) -> {cpu with A = v}
    //| x INC -> {cpu with x = cpu.x + 1uy }
    | B v -> {cpu with B = v}
    | C v -> {cpu with C = v}
    | D v -> {cpu with D = v}
    | E v -> {cpu with E = v}
    | F_Z v -> {cpu with f_Z = v}
    | F_H v -> {cpu with f_H = v}
    | F_N v -> {cpu with f_N = v}
    | F_C v -> {cpu with f_C = v}
    | HL v -> {cpu with HL = v}
    | SP v -> {cpu with SP = v}
    | PC v -> {cpu with PC = v}
    | CPU_Ticks v -> {cpu with CPU_Ticks = v}
    | Mλ(v1, v2) -> update_cpu cpu v1 |> update_cpu <| v2

let update_t2 t2 = function
    | A v -> printfn "A"
    | B v -> printfn "B"
    | _ -> printfn "_"

let c_add cpu r =
    match r with
    | A _ -> update_cpu cpu (A (cpu.A + 0x01uy))
    | _ -> failwith "fail"



//let q = { a = A(0x1uy) ; b = B(0x2uy)}
//let updated = update_t2 q (B(0x3uy))
//let updated2 = update_t2 q (B(q.a))


let incc state = c_add state (A(0x0uy))
incc state

let set1 cpu r = update_cpu cpu <| r(1uy) // set to 1, r refers to a reg type
let inc cpu r v = update_cpu cpu <| r(v+1uy) // r refers to reg type, not cpu reg, ambiguous
let t = B(5uy) // if this is a reg... then we can match the state

let state2 = inc state B state.B // don't like this, want: inc state B

let inc_PC cpu = update_cpu cpu <| PC(cpu.PC + 1us)

let init_cpu cpu = { A = 0x01uy; B = 0x0uy; C = 0x13uy; D = 0x0uy; E = 0xd8uy;
                  f_Z = 0x01uy; f_N = 0x0uy; f_H = 0x01uy; f_C = 0x01uy;
                  PC = 0x0100us; SP = 0xfffeus; HL = 0x014dus; CPU_Ticks = 0x0us; }

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
