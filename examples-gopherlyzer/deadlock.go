package main

func main() {
   	ch := make(chan int)
   	done := make(chan int)
	go func() {              // Send
		ch <- 42
	}()
	go func() {              // Recv1
		var val int;
		val = <-ch;
		done <- val;
	}()
	go func() {              // Recv2
		var val int;
		val = <-ch;
		done <- val;
	}()
	go func() {              // Work
		for {}
	}()
	<-done;
	<-done
}
