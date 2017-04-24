package main

/* single comment */

// single line comment

/* in go the first 'end comment' ends the outer comment
  /* regardless of nesting */
func main() {
	ch1 := make(chan int)
	go func() { }()
	ch1 <- 1
}
