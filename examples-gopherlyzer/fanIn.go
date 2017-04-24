package main

func main() {
	input1 := make(chan int)
	input2 := make(chan int)
	out := make(chan int)
	go func() { // work1
		for {
			input1 <- 41
		}
	}()
	go func() { // work2
		for {
			input2 <- 42
		}
	}()
	go func() { // fanin
		var s int;
		for {
			select {
			case s = <-input1:
				out <- s
			case s = <-input2:
				out <- s
			}
		}
	}()
	for {
		<-out
	}
}
