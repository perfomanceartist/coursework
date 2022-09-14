package main
import  "fmt"
 
func main() {
    var f func(int, int) int = add
    //f := func(x, y int) int{ return x + y} ??????????????????????????????????
    //var f func(int, int) int = func(x, y int) int{ return x + y}
    //action(5, 6, func (x int, y int) int { return x * y }) 
}
 
func add(x int, y int){
    //add([]int{5, 6, 7, 2, 3}) ??????????????????????????????????
}

func action(n1 int, n2 int, operation func(int, int) int){
}


func selectFn(n int) (func(int, int) int){
}


func add(x, y int, a, b, c float32){
}

func add(numbers ...int){
}

func add(x, y int) int {
    return x + y
}

func add(x, y int) (z int) {
    z = x + y
    return
}


func add(x, y int, firstName, lastName string) (int , string) {
    return z, fullName
}
