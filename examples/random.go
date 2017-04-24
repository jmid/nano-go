package main

/* example from language specification https://golang.org/ref/spec#SelectStmt  */
func main() {
	c := make(chan int)
	go func() {
		for {
			select {
			case c <- 0:
			case c <- 1:
			}
		}
	}()
	var x int;
	x = <- c;
	print(x);
	x = <- c;
	print(x);
	x = <- c;
	print(x);
}
