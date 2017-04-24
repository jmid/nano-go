Static analysis prototype for nano-go, a small subset of the full Go
programming language.  The analysis infers the communication behaviour
and content of integer Go programs without running them.

Currently we support only the following subset of Go:

           e ::= i | ( e ) | x | - e
               | e + e | e - e | e * e | e % e
           b ::= true | false | ! b | ( b )
               | e == e | e <= e | e < e | b && b
    recvstmt ::= x = <- ch | <- ch
    sendstmt ::= ch <- e
        stmt ::= x = e
               | recvstmt
               | sendstmt
               | if b block else block
               | for [test] block
               | select { selcase* }
               | print(e)
       stmts ::= stmt [;]
               | stmt ; stmts
               | { stmts }
       block ::= { stmts } | {}
     selcase ::= case recvstmt : [stmts]
               | case sendstmt : [stmts]
      chdecl ::= x := make(chan int)
        func ::= go func() { [body] } ()
        body ::= var x int [;] body
               | stmts
        prog ::= package main
                 func main() { chdecl+ func+ body }
