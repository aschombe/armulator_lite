let run1_ast : Arm.prog = [
    Arm.GloblDef("_start");
    Arm.TextDirect([
        { entry=true; lbl=("_start"); asm=Arm.Text([
            (Arm.Mov, [Arm.Reg(Arm.X0); Arm.Imm(Arm.Lit(15L))]);
            (Arm.Mov, [Arm.Reg(Arm.X1); Arm.Imm(Arm.Lit(57005L))]);
            (Arm.Mov, [Arm.Reg(Arm.X2); Arm.Imm(Arm.Lit(48879L))]);
            (Arm.Ret, []);
        ])}
    ]);
    Arm.DataDirect([
        { entry=false; lbl=("hello_str"); asm=Arm.Data([
        Arm.ByteArr([0x48; 0x65; 0x6c; 0x6c; 0x6f; 0x2c; 0x20; 0x57; 0x6f; 0x72; 0x6c; 0x64; 0x21; 0x0a; 0x00])
        ])};
        { entry=false; lbl=("hello_len"); asm=Arm.Data([
        Arm.Quad(14L)
        ])};
        { entry=false; lbl=("test_quad"); asm=Arm.Data([
        Arm.Quad(3735928559L)
        ])};
        { entry=false; lbl=("arr"); asm=Arm.Data([
        Arm.ByteArr([0x01; 0x02; 0x03; 0x04; 0x05])
        ])};
        { entry=false; lbl=("arr_len"); asm=Arm.Data([
        Arm.Quad(5L)
        ])}
    ])
]

let run2_ast : Arm.prog = [
Arm.GloblDef("_start");
Arm.TextDirect([
{ entry=false; lbl=("add_two_nums"); asm=Arm.Text([
    (Arm.Add, [Arm.Reg(Arm.X0); Arm.Reg(Arm.X0); Arm.Reg(Arm.X1)]);
    (Arm.Ret, [])
])};
{ entry=true; lbl=("_start"); asm=Arm.Text([
    (Arm.Adr, [Arm.Reg(Arm.X0); Arm.Imm(Arm.Lbl("n1"))]);
        (Arm.Ldr, [Arm.Reg(Arm.X0); Arm.Offset(Arm.Ind2(Arm.X0))]);
        (Arm.Adr, [Arm.Reg(Arm.X1); Arm.Imm(Arm.Lbl("n2"))]);
        (Arm.Ldr, [Arm.Reg(Arm.X1); Arm.Offset(Arm.Ind2(Arm.X1))]);
        (Arm.Bl, [Arm.Imm(Arm.Lbl("add_two_nums"))]);
        (Arm.Ret, [])
])}
]);Arm.DataDirect([
{ entry=false; lbl=("n1"); asm=Arm.Data([
Arm.Quad(10L)
])};
{ entry=false; lbl=("n2"); asm=Arm.Data([
Arm.Quad(20L)
])}
]);
]

let conditional_logic_ast : Arm.prog = []

let prog_eq (received: Arm.prog) (expected: Arm.prog) : (bool * string * string) = 
    let s_p2 = Arm_stringifier.string_of_prog expected in 
    let s_p1 = Arm_stringifier.string_of_prog received in
    (s_p1 = s_p2, s_p1, s_p2)

(* test name, file, expected AST *)
let armprograms = [
    ("run1", "armprograms/run1.s", run1_ast);
    ("run2", "armprograms/run2.s", run2_ast);
    ("conditional_logic", "armprograms/conditional_logic.s", conditional_logic_ast);
]

let read_file (file:string) : Arm_parser.code_line list =
  let lines = ref [] in
  let ln = ref 1 in
  let channel = open_in ("../" ^ file) in
  try while true; do
    lines := (!ln, input_line channel) :: !lines;
    ln := !ln + 1
  done; []
  with End_of_file ->
    close_in channel;
    List.rev !lines

let run_arm_test (_name, file, expected) : (bool * string * string) =
    let lines = read_file (file) in 
    try 
        let ast = Arm_parser.parse_assembly lines in 
        prog_eq ast expected
    with _ -> (false, "parse error", Arm_stringifier.string_of_prog expected)

let run_arm_tests () : bool =
    let count = ref 0 in 
    let passed = ref 0 in 
    List.iter (fun (name, file, ast) -> 
        let (test_result, r1, r2) = run_arm_test (name, file, ast) in 
        incr count;
        if test_result then begin
            incr passed;
            Printf.printf "passed - %s\n" name;
        end
        else begin 
            Printf.printf "failed - %s\n" name;
            Printf.printf "expected:\n%s\n" r2; 
            Printf.printf "got:\n%s\n" r1;
        end
    ) armprograms;
    Printf.printf "passed %d/%d tests - %.2f%%\n" !passed !count ((float_of_int !passed) /. (float_of_int !count) *. 100.0); 
    !passed = !count

let _ = assert(run_arm_tests())
