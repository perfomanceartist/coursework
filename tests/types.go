package main
import "fmt"
 
type person struct{
    name string
    age int
}
 
type person struct{
    name string
    age int
    contactInfo contact
}

func (p person) print(){
}
 
func (p person) eat(meal string){
}
func (p person) eat(meal string) (int x) {
}
func (p person) eat(meal string, x int) (int, int){
}

type Stream interface{
    read() string
    write(string)
    close()
}
 
func writeToStream(stream Stream, text string){
    stream.write(text)
}
func closeStream(stream Stream){
    stream.close()
}


func main() {
     
    var tom = person {name: "Tom", age: 24}
    fmt.Println(tom.name)  
    tom.age = 38
	
	//myFile := &File{}
    //var tomPointer *person = &tom    
   	//(*tomPointer).age = 32
	
}