
lab1 : Stack machine --> added division and paranthesis
lab2 : in tiger/lib--> created 3 files : ast.ml, parser.mly, lexer.mll and read about the spim documentation
       in tiger/lib -> created a folder- backend
       in liger/lib/backend -> created mips folder
lab3 :
in liger/lib/backend/mips -> created 3 files 
reg.ml--> defined all the register types in mips
inst.ml --> converts anything to string
mips.ml --> converts into assembly

in lab4:
at first created the directory tiger0:
tiger0/ast.ml: defined the structure of program
tiger0/ir.ml: temp module and structure of ir lang
tiger0/translate.ml: converts to ir lang
added lexer.mll and parser.mly
tiger0/mips.ml: converts to mips assembly utilizing lexer.mll and parser.mly
-> changed executable name to tc - (public_name tc)
added dune for each folder(since I am getting the error if i use include sub_dir) and wrote content in it accordingly

in lab5- for loops :
made small changes to lab4:
in ast.ml : added For
in ir.ml: newlabel and also added suitable instruction types like label,jump,cond_jump
environment.ml : created an environment
translate.ml : completely changed such that env can be used
to run ->  dune exec tc -- test/foo.tig

using spim -> dune exec tc -- test/foo.tig> output.s
              spim -file output.s



in lab6: Tree ir : this lab is not done 

in lab7: basic blocks: added block.ml and implemented basic blocks using fold_right but the output is not correct..i have to modify block.ml
to run -> dune exec tb -- test/foo.tig 

in lab8: Graphs is not done
in lab9: modified changelog.md -> wrote everything that is done so far

