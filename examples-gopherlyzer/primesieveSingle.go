package main

func main() {
	ch := make(chan int)
	out := make(chan int)
	
	go func() {	// func generate
		var i int;
		i = 2;
		for { // desugared for-loop
			ch <- i;
			i = i + 1
		}
	}()
	
	go func() {	// func filter
		var i int;
		var prime int;
		prime = <-ch;  // prime is local var, read from ch
		for {
			i = <-ch;
			if !(i % prime == 0) {
				out <- i
			} else {} // added empty else clause
		}
	}()

	var tmp int;
	tmp = <-out;
	print(tmp);
	tmp = <-out;
	print(tmp);
	tmp = <-out;
	print(tmp);
	tmp = <-out;
	print(tmp);
	tmp = <-out;
	print(tmp);
}
