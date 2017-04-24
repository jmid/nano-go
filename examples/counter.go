package main

/* a cyclic counter server (wraps around at value 8) and client (queries thrice per iteration) */
func main() {
	ch := make(chan int)
	go func() {
		var c1 int;
		var c2 int;
		var c3 int;
		for {
			c1 = <- ch;
			c2 = <- ch;
			c3 = <- ch;
			print(c1);
			print(c2);
			print(c3);
		}
	}()
	var cnt int;
	cnt = 0;
	for {
		ch <- cnt;
		if cnt == 8 { cnt = 0 } else { cnt = cnt + 1 };
	}
}
