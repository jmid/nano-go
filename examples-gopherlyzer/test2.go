package main

func main() {
	a := make(chan int)
	b := make(chan int)

	// func A inlined
	go func() {
		for {
			a <- 4;
			<-b;
		}
	}()

	// func B inlined	
	var k int; // k explicit decl
	for {
		k = <-a;
		if 5 < k {  // changed > to <
			b <- 4
		} else {} // added empty else clause
	}
}
