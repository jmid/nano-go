package main

func main() {
	ch := make(chan int)
	ch1 := make(chan int)
	ch2 := make(chan int)
	ch3 := make(chan int)
	
	go func() { // func generate
		var i int;
		i = 2;
		for {
			ch <- i;
			i = i + 1;
		}
	}()
	
	//------ 1 ------
	//prime := <-ch
	//fmt.Println(prime)
	go func() { // func filter
		var i int;
		var prime int;
		prime = <-ch;
		for {
			i = <-ch;
			if !(i % prime == 0) {
				ch1 <- i
			} else {}
		}
	}()
	
	//------ 2 ------
	//prime := <-ch1
	//fmt.Println(prime)
	go func() { // func filter2
		var i int;
		var prime int;
		prime = <-ch1;
		for {
			i = <-ch1;
			if !(i % prime == 0) {
				ch2 <- i
			} else {}
		}
	}()
	
	//------ 3 ------
	//prime := <-ch2
	//fmt.Println(prime)
	go func() { // func filter3
		var i int;
		var prime int;
		prime = <-ch2;
		for {
			i = <-ch2;
			if !(i % prime == 0) {
				ch3 <- i
			} else {}
		}
	}()

	var tmp int;
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
	tmp = <-ch3; print(tmp);
}
