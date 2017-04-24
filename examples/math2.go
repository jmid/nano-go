package main

/* another math server inspired by Vasconcelos-Gay-Ravara:TCS06 */
func main() {
	add := make(chan int)
	square := make(chan int)
	go func() {     // server process
		var n int;
		var n1 int;
		var n2 int;
		for {
			select {
			case n1 = <- add:
				n2 = <- add;
				add <- n1+n2;
			case n = <- square:
				square <- n * n;
			}
		}
	}()
	var result int; // client process
	select {
	case add <- 3:
		add <- 4;
		result = <- add;
	case square <- 3:
		result = <- square;
	};
	print(result)

}
