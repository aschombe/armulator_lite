%{
    open Ast 
%}

%token EOF
%token NEWLINE

%token COMMA /* , */
%token COLON /* : */
%token SEMI  /* ; */
%token LBRACKET /* [ */
%token RBRACKET /* ] */
%token DOT /* . */
%token HASH /* # */
%token EQL /* = */

/* opcodes */
%token MOV
%token ADD
%token SUB
%token MUL
%token AND
%token ORR
%token LSL 
%token LSR
%token ASR
%token LDR
%token STR
%token ADR
%token B 
%token BR
%token BL
%token RET 
%token CBZ 
%token CBNZ
%token CMP

/* condition codes */
%token GLOBAL
%token TEXT

%token <Ast.quad> QUAD

%token <Ast.reg> REG
%token <Ast.imm> IMM
%token <Ast.lbl> LABEL
%token <Ast.cnd> CND
%token <Ast.operand> OPERAND

%start <Ast.prog> prog
%%

prog:
    | dl=list(directive) EOF { dl }

directive:
    | DOT GLOBAL l=LABEL NEWLINE { Global l }
    | DOT TEXT NEWLINE il=insn_list NEWLINE { Text il }

insn_list:
    | l=LABEL COLON i=insn NEWLINE il=insn_list { (Some(l), i) :: il }
    | l=LABEL COLON i=insn NEWLINE { [Some(l), i] }
    | i=insn NEWLINE il=insn_list { (None, i) :: il }
    | i=insn NEWLINE { [None, i] }

insn:
    | MOV o1=OPERAND COMMA o2=OPERAND { (Mov, [o1; o2]) }
    | ADD o1=OPERAND COMMA o2=OPERAND COMMA o3=OPERAND { (Add, [o1; o2; o3]) }
    | SUB o1=OPERAND COMMA o2=OPERAND COMMA o3=OPERAND { (Sub, [o1; o2; o3]) }
    | BL l=LABEL { (Bl, [Label l]) }
    | BR l=LABEL { (Br, [Label l]) }
    | B DOT c=CND l=LABEL { (B c, [Label l]) }
    | CMP o1=OPERAND COMMA o2=OPERAND { (Cmp, [o1; o2]) }
    | RET { (Ret, []) }

