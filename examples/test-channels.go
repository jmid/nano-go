package main

func main() {
	messages := make(chan int)
	go func() { messages <- 1 }()
	go func() { messages <- 5 }()
	var msg int;
	msg = <- messages;
          print(msg);
	msg = <- messages;
	print(msg)
}
