package main
 
import "fmt"
 
func createPointer(x int) *int{
    *p = x
    return p
}


func main() {
     
    var x int = 4       // определяем переменную
    var p *int          // определяем указатель 
    p = &x              // указатель получает адрес переменной

	var p *int  = &x                // указатель получает адрес переменной
	*p = 25
	pf := &f
    fmt.Println("Value:", *p)       // значение переменной x
	*p = nil
}