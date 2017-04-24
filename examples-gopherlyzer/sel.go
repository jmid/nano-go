package main

func main() {
	x := make(chan int) // changed bool channels to int channels
	y := make(chan int)
	z1 := make(chan int)
	z2 := make(chan int)
	
	go func() { x <- 1 }() // provide1
	go func() { y <- 0 }() // provide2
	
	go func() {            // collect1
		var tmp int;
		tmp = <-x; //z1 <- <-x
		z1 <- tmp;
	}()
	go func() {            // collect2
		var tmp int;
		tmp = <-y; //z1 <- <-y
		z1 <- tmp;
	}()
	go func() {            // collect1
		var tmp int;
		tmp = <-x; //z2 <- <-x
		z2 <- tmp;
	}()
	go func() {            // collect2
		var tmp int;
		tmp = <-y; //z2 <- <-y
		z2 <- tmp;
	}()
	var tmp int;
	tmp = <-z1;
	print(tmp);
	tmp = <-z2;
	print(tmp);
}
