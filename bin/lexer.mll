{
    open Parser
    open Ast
}

let newline = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let char = lowercase | uppercase 
let digit = '-'? ['0'-'9']
let label = (char | '_') (char | digit | '_' | '.')*

rule token = parse
    | eof { EOF }
    | newline+ { NEWLINE }
    | whitespace+ { token lexbuf }
    | "," { COMMA }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "." { DOT }
    | ";" { SEMI }
    | ":" { COLON }
    | "#" { HASH }
    | "=" { EQL }
    | "mov" { MOV }
    | "add" { ADD }
    | "sub" { SUB }
    | "mul" { MUL }
    | "and" { AND }
    | "orr" { ORR }
    | "lsl" { LSL }
    | "lsr" { LSR }
    | "asr" { ASR }
    | "ldr" { LDR }
    | "str" { STR }
    | "adr" { ADR }
    | "b" { B }
    | "bl" { BL }
    | "br" { BR }
    | "cbz" { CBZ }
    | "cbnz" { CBNZ }
    | "ret" { RET }
    | "text" { TEXT }
    | "global" { GLOBAL }
    | "sp" { REG SP }
    | "lr" { REG LR }
    | "xzr" { REG XZR }
    | "x" digit+ as r { REG (reg_of_string r) }
    | "#" digit+ as d { QUAD (Int64.of_int (int_of_string d)) }
    | label as l { LABEL l }
    | _ { raise (Error) }
