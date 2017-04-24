package main

/* simple math server from Vasconcelos-Gay-Ravara:TCS06 */
func main() {
	add := make(chan int)
	res := make(chan int)
	go func() {     // server process
		var n1 int;
		var n2 int;
		for {
			n1 = <- add;
			n2 = <- add;
			res <- n1+n2;
		}
	}()
	var result int; // client process
	add <- 3;
	add <- 4;
	result = <- res;
	print(result)
}
