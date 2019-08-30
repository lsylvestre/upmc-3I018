# upmc-3I018


## MLCompiler + NativeVM    
*(OCaml > 4.05)*

```
$ cd src/MLCompiler/
$ make
$ ./compiler -vmconst
$ make -C ../NativeVM/  CONSTANTS_DIR=../MLCompiler
```
```
$ ./compiler -run tests/fact_de_6.js 
## runVM v3 (native)
## -----------------
## loading bytecode file: ../MLCompiler/tests/fact_de_6.js.bc
## -------------------
## 720
## -------------------
## VM stopping
$ ./compiler -rundebug tests/fact_de_6.js 
## runVM v3 (native)
## -----------------
## starting VM in debug mode
## ...
## === Execute next intruction ===
## >>> POP
## DISPLAY> 720
## State:
## PC = 59
## Globals = [Closure@3 - <>> <unit>]
## Stack = []
## Frame = Frame(pc=59,sp=0,env=<>)
## <- END
## === Finish execution ====
## -------------------
## VM stopping
$
```

## JCompiler + NativeVM
```
$ cd src/JCompiler/
$ ant -buildfile build.xml uberjar
$ java -jar jcompiler.jar -vmconst
$ make -C ../NativeVM/  CONSTANTS_DIR=../JCompiler
```
```
$ java -jar jcompiler.jar tests/fact_de_6.js 
$ cd ../NativeVM
$ ./runvm ../JCompiler/tests/fact_de_6.js.bc 
## runVM v3 (native)
## -----------------
## loading bytecode file: ../JCompiler/tests/fact_de_6.js.bc
## -------------------
## 720
## -------------------
## VM stopping
$ ./runvm -d ../JCompiler/tests/fact_de_6.js.bc 
## === Execute next intruction ===
## >>> POP
## DISPLAY> 720
## State:
## PC = 59
## Globals = [Closure@3 - <>> <unit>]
## Stack = []
## Frame = Frame(pc=59,sp=0,env=<>)
## <- END
## === Finish execution ====
## -------------------
## VM stopping
$
```
